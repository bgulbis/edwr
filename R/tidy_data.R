# Tidy EDW data

#' Transform to a tidy data set
#'
#' \code{tidy_data} transforms raw EDW data into a tidy format
#'
#' This is an S3 generic function for tidying EDW data read in using
#' \code{\link{read_data}}. The function invokes the appropriate method based on
#' the type of data being transformed (i.e., lab results, medication data,
#' etc.).
#'
#' The data frame passed to \code{ref} should contain three character columns:
#' name, type, and group. The name column should contain either generic
#' medication names or medication classes. The type column should specify
#' whether the value in name is a "class" or "med". The group column should
#' specify whether the medication is a continous ("cont") or scheduled ("sched")
#' medication.
#'
#' @param x A data frame with an edw class type
#' @param ... additional arguments passed on to individual methods
#' @param censor A logical, if TRUE will add a column indicating the data was
#'   censored (default)
#' @param ref A data frame with three columns: name, type, and group. See
#'   details below.
#' @param sched A data frame with intermittent medications
#' @param dc A data frame with discharge date/times
#'
#' @examples
#' # tidy lab data; non-numeric results will be converted to NA
#' suppressWarnings(print(head(
#'   tidy_data(labs)
#' )))
#'
#' # tidy labs without marking censored data (will be converted to NA's)
#' suppressWarnings(print(head(
#'   tidy_data(labs, censor = FALSE)
#' )))
#'
#' # make a reference data frame for tidying meds
#' ref <- tibble::tibble(
#'   name = c("heparin", "warfarin", "antiplatelet agents"),
#'   type = c("med", "med", "class"),
#'   group = c("cont", "sched", "sched")
#' )
#'
#' # tidy continuous medications; will keep only heparin drips
#' print(head(
#'   tidy_data(meds_cont, meds_sched, ref)
#' ))
#'
#' # tidy intermittent medications; will keep warfarin and antiplatelet agents
#' print(head(
#'   tidy_data(meds_sched, ref)
#' ))
#'
#' @export
tidy_data <- function(x, ...) {
    UseMethod("tidy_data")
}

#' @export
#' @rdname tidy_data
tidy_data.default <- function(x, ...) {
    warning("No method available for objects of this class")
    x
}


#' @details For diagnosis, checks to see whether the code is a valid ICD-9-CM or
#'   ICD-10-CM code. For codes that are valid for both (i.e., "E" and "V"
#'   codes), then it looks to see if the code matches a defined ICD-9-CM or
#'   ICD-10-CM code. For codes that are defined in both, then the designated
#'   code type from the source is used.
#'
#' @export
#' @rdname tidy_data
tidy_data.diagnosis <- function(x, ...) {
    diag_code <- sym("diag.code")
    icd9 <- sym("icd9")
    icd10 <- sym("icd10")

    # find codes which are valid
    valid_codes <- x %>%
        mutate(
            !!"icd9" := icd::icd_is_valid(
                icd::as.icd9cm(!!diag_code)
            ),
            !!"icd10" := icd::icd_is_valid(
                icd::as.icd10cm(!!diag_code)
            )
        )

    # if code only valid in one type, then assign it to the correct group
    assign <-filter(valid_codes, !!parse_expr("!(icd9 & icd10)"))

    # find codes which are valid in both ICD9/10 and check if they are defined
    undefined <- valid_codes %>%
        filter(!!icd9, !!icd10) %>%
        mutate(
            !!"icd9" := icd::icd_is_defined(
                icd::as.icd9cm(!!diag_code)
            ),
            !!"icd10" := icd::icd_is_defined(
                icd::as.icd10cm(!!diag_code)
            )
        )

    # if code only defined in one type, assign it to the correct group
    icd_defined <- filter(undefined, !!parse_expr("!(icd9 & icd10)"))

    # for codes defined in both, use the source assignment from EDW
    source_default <- undefined %>%
        filter(!!icd9, !!icd10) %>%
        mutate(
            !!"icd9" := !!parse_expr(
                'code.source == "ICD-9-CM" | code.source == "ICD9"'
            )
        )

    df <- dplyr::bind_rows(assign, icd_defined, source_default) %>%
        select(-!!icd10)

    reclass(x, df)
}

#' @export
#' @rdname tidy_data
tidy_data.labs <- function(x, censor = TRUE, ...) {
    if ("lab.result" %in% colnames(x)) {
        lab_result <- sym("lab.result")
    } else if ("event.result" %in% colnames(x)) {
        lab_result <- sym("event.result")
    } else {
        warning("No valid result column found, need lab.result or event.result")
        return(x)
    }

    df <- x
    # create a column noting if data was censored
    if (censor) {
        df <- mutate(
            df,
            !!"censor.low" := stringr::str_detect(!!lab_result, "<"),
            !!"censor.high" := stringr::str_detect(!!lab_result, ">")
        )
    }

    # convert lab results to numeric values
    df <- dplyr::mutate_at(df, dplyr::vars(!!lab_result), as.numeric)

    reclass(x, df)
}

#' @details For locations, this function accounts for incorrect departure
#'   time from raw EDW data by calculating the departure time using the arrival
#'   time of the next unit (unless it was the patient's last unit during the
#'   hospitalization in which case the recorded departure time is used). It also
#'   combines multiple rows of data when the patient did not actually leave that
#'   unit.
#'
#' @export
#' @rdname tidy_data
tidy_data.locations <- function(x, ...) {
    arrive_datetime <- sym("arrive.datetime")
    depart_datetime <- sym("depart.datetime")
    diff_unit <- sym("diff.unit")
    unit_count <- sym("unit.count")

    if (attr(x, "data") == "edw") {
        id <- sym("pie.id")
        depart_recorded <- sym("depart.recorded")

        df <- x %>%
            arrange(!!id, !!arrive_datetime) %>%
            group_by(!!id) %>%

            # determine if pt went to different unit, count num of different units
            mutate(
                !!"diff.unit" := !!parse_expr(
                    "is.na(unit.to) | is.na(dplyr::lag(unit.to)) | unit.to != dplyr::lag(unit.to)"
                ),
                !!"unit.count" := cumsum(!!diff_unit)
            ) %>%

            # use the count to group multiple rows of the same unit together
            group_by(!!id, !!unit_count) %>%
            summarize(
                !!"location" := dplyr::first(!!sym("unit.to")),
                !!"arrive.datetime" := dplyr::first(!!arrive_datetime),
                !!"depart.recorded" := dplyr::last(!!depart_datetime)
            ) %>%

            # use the arrival time for the next unit to calculate a depart time; if
            # there is no arrival time for the next unit then used the depart
            # date/time from EDW
            group_by(!!id) %>%
            mutate(
                !!"depart.datetime" := dplyr::lead(!!arrive_datetime),
                !!"depart.datetime" := dplyr::coalesce(
                    !!depart_datetime,
                    !!depart_recorded
                )
            ) %>%
            ungroup() %>%
            mutate(!!"unit.length.stay" := difftime(
                !!depart_datetime,
                !!arrive_datetime,
                units = "days")
            ) %>%
            select(-!!depart_recorded)
    } else {
        id <- sym("millennium.id")

        # tidy location data from MBO
        df <- x %>%
            arrange(!!id, !!arrive_datetime) %>%
            group_by(!!id) %>%
            # determine if pt went to different unit, count num of different units
            mutate(
                !!"diff.unit" := !!parse_expr(
                    "unit.name != dplyr::lag(unit.name)"
                ),
                !!"diff.unit" := dplyr::coalesce(!!sym("diff.unit"), TRUE),
                !!"unit.count" := cumsum(!!diff_unit)
            ) %>%
            # use the count to group multiple rows of the same unit together
            group_by(!!id, !!unit_count) %>%
            summarize(
                !!"location" := dplyr::first(!!sym("unit.name")),
                !!"arrive.datetime" := dplyr::first(!!arrive_datetime),
                !!"depart.datetime" := max(!!depart_datetime)
            ) %>%
            # combine location stays that are < 5 minutes
            mutate(
                !!"duration" := difftime(
                    !!depart_datetime,
                    !!arrive_datetime,
                    units = "mins"
                ),
                !!"diff.unit" := !!parse_expr(
                    "duration > 5 | is.na(dplyr::lag(duration))"
                ),
                !!"unit.count" := cumsum(!!diff_unit)
            ) %>%
            # use the count to group multiple rows of the same unit together
            group_by(!!id, !!unit_count) %>%
            summarize(
                !!"location" := dplyr::first(!!sym("location")),
                !!"arrive.datetime" := dplyr::first(!!arrive_datetime),
                !!"depart.datetime" := max(!!depart_datetime)
            ) %>%
            # determine again if pt went to different unit, count num of
            # different units
            mutate(
                !!"diff.unit" := !!parse_expr(
                    "location != dplyr::lag(location)"
                ),
                !!"diff.unit" := dplyr::coalesce(!!diff_unit, TRUE),
                !!"unit.count" := cumsum(!!diff_unit)
            ) %>%
            # final grouping of multiple rows of the same unit together
            group_by(!!id, !!unit_count) %>%
            summarize(
                !!"location" := dplyr::first(!!sym("location")),
                !!"arrive.datetime" := dplyr::first(!!arrive_datetime),
                !!"depart.datetime" := max(!!depart_datetime)
            ) %>%
            ungroup() %>%
            mutate(
                !!"unit.length.stay" := difftime(
                    !!depart_datetime,
                    !!arrive_datetime,
                    units = "days"
                )
            )
    }

    reclass(x, df)
}

#' Title
#'
#' @param x tibble
#' @param group string indicating cont or sched
#' @param ref
#'
#' @return tibble
#'
#' @keywords internal
tidy_fun <- function(x, group, ref = NULL) {
    id <- set_id_quo(x)

    if(!is.null(ref)) {
        # for any med classes, lookup the meds included in the class
        y <- filter(
            ref,
            !!parse_expr(
                paste0('type == "class" & group == "', group, '"')
            )
        )
        class_meds <- med_lookup(y$name)

        # join the list of meds with any indivdual meds included
        y <- filter(
            ref,
            !!parse_expr(
                paste0('type == "med" & group == "', group, '"')
            )
        )

        lookup_meds <- c(y$name, class_meds$med.name) %>%
            stringr::str_to_lower()

        df <- filter(x, !!parse_expr("med %in% lookup_meds"))
    } else {
        df <- x
    }

    # remove any rows in continuous data which are actually scheduled doses,
    # then filter to meds in lookup, then sort by pie.id, med, med.datetime
    df <- df %>%
        arrange(!!id, !!sym("med"), !!sym("med.datetime"))

    reclass(x, df)
}

#' @export
#' @rdname tidy_data
tidy_data.meds_cont <- function(x, sched, ref = NULL, ...) {
    # remove any rows in continuous data which are actually scheduled doses,
    # then filter to meds in lookup, then sort by pie.id, med, med.datetime
    x %>%
        anti_join(sched, by = "event.id") %>%
        tidy_fun("cont", ref)
}

#' @export
#' @rdname tidy_data
tidy_data.meds_inpt <- function(x, ref =  NULL, ...) {
    event_tag <- sym("event.tag")

    sched <- x %>%
        filter(is.na(!!event_tag)) %>%
        tidy_fun("sched", ref)

    cont <- x %>%
        filter(!is.na(!!event_tag)) %>%
        tidy_fun("cont", ref)

    dplyr::bind_rows(sched, cont) %>%
        arrange(!!sym("millennium.id"), !!sym("med.datetime"))
}

#' @export
#' @rdname tidy_data
tidy_data.meds_sched <- function(x, ref = NULL, ...) {
    tidy_fun(x, "sched", ref)
}

#' @details For services, this function accounts for incorrect end times
#'   from raw EDW data by calculating the end time using the start time of the
#'   next service (unless  it was the patient's last service during the
#'   hospitalization). It also  combines multiple rows of data when the patient
#'   did not actually leave  that service.
#'
#' @export
#' @rdname tidy_data
tidy_data.services <- function(x, ...) {
    df <- x %>%
        arrange(!!sym("start.datetime")) %>%
        group_by(!!sym("pie.id")) %>%

        # determine if they went to a different service, then make a count of
        # different services
        mutate(
            !!"diff.service" := !!parse_expr(
                # "is.na(service) | is.na(dplyr::lag(service)) | service !=
                # dplyr::lag(service)"
                sprintf(
                    "is.na(service) | is.na(%1$s) | service != %1$s",
                    "dplyr::lag(service)"
                )
            ),
            !!"service.count" := cumsum(!!sym("diff.service"))
        ) %>%

        # use the service.count to group multiple rows of the same service
        # together and combine data
        group_by(!!sym("pie.id"), !!sym("service.count")) %>%
        summarize(
            !!"service" := dplyr::first(!!sym("service")),
            !!"start.datetime" := dplyr::first(!!sym("start.datetime")),
            !!"end.recorded" := dplyr::last(!!sym("end.datetime"))
        ) %>%

        # use the start time for the next service to calculate an end time
        group_by(!!sym("pie.id")) %>%
        mutate(!!"end.datetime" := dplyr::lead(!!sym("start.datetime")),
               !!"end.datetime" := dplyr::coalesce(
                   !!sym("end.datetime"),
                   !!sym("end.recorded")
               ),
               !!"service.duration" := difftime(
                   !!sym("end.datetime"),
                   !!sym("start.datetime"),
                   units = "days")
        ) %>%
        ungroup() %>%
        select(-!!sym("end.recorded"))

    reclass(x, df)
}

#' @details For vent_times, this function accounts for incorrect start and stop
#'   times from raw EDW data. If there is not a recorded stop time then the
#'   discharge time will be used as the stop time.
#'
#' @export
#' @rdname tidy_data
tidy_data.vent_times <- function(x, dc, ...) {
    if (missing(dc)) stop("Please include data frame with discharge.datetime")

    id <- set_id_quo(x)

    if ("vent.datetime" %in% colnames(x)) {
        vent_datetime <- sym("vent.datetime")
        vent_event <- "vent.event"
    } else if ("event.datetime" %in% colnames(x)) {
        vent_datetime <- sym("event.datetime")
        vent_event <- "event"
    } else {
        warning("No valid date/time columns found, need vent.datetime or event.datetime")
        return(x)
    }

    stop_datetime <- sym("stop.datetime")

    # remove any missing data
    df <- x %>%
        filter(!is.na(!!vent_datetime)) %>%
        arrange(!!vent_datetime) %>%
        group_by(!!id) %>%

        # if it's the first event or the next event is a stop, then count as a
        # new vent event
        mutate(
            !!"diff.event" := !!parse_expr(
                sprintf(
                    "is.na(dplyr::lag(%1$s)) | %1$s != lag(%1$s)",
                    vent_event
                )
            ),
            !!"event.count" := cumsum(!!sym("diff.event"))
        ) %>%

        # for each event count, get the first and last date/time
        group_by(!!!quos(!!id, !!sym("event.count"))) %>%
        summarize(
            !!"event" := dplyr::first(!!sym(vent_event)),
            !!"first.event.datetime" := dplyr::first(!!vent_datetime),
            !!"last.event.datetime" := dplyr::last(!!vent_datetime)
        ) %>%

        # use the last date/time of the next event as stop date/time; this would
        # be the last stop event if there are multiple stop events in a row. if
        # there isn't a stop date/time because there was start with no stop, use
        # the discharge date/time as stop date/time
        left_join(
            dc[c(rlang::quo_text(id), "discharge.datetime")],
            by = rlang::quo_text(id)
        ) %>%
        group_by(!!id) %>%
        mutate(
            !!"stop.datetime" := dplyr::lead(!!sym("last.event.datetime")),
            !!"stop.datetime" := dplyr::coalesce(
                !!stop_datetime,
                !!sym("discharge.datetime")
            )
        ) %>%
        filter(!!parse_expr('event == "vent start time"')) %>%
        select(
            !!id,
            !!"start.datetime" := !!sym("first.event.datetime"),
            !!stop_datetime
        ) %>%
        ungroup() %>%
        mutate(!!"vent.duration" := difftime(
            !!stop_datetime,
            !!sym("start.datetime"),
            units = "hours")
        )

    reclass(x, df)
}
