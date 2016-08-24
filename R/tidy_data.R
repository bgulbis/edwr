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
#'   tidy_data(meds_cont, ref, meds_sched)
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
    # find codes which are valid
    valid.codes <- mutate_(x, .dots = set_names(
        x = list(~icd::icd_is_valid(icd::as.icd9cm(diag.code)),
                 ~icd::icd_is_valid(icd::as.icd10cm(diag.code))),
        nm = list("icd9", "icd10")
    ))

    # if code only valid in one type, then assign it to the correct group
    assign <- filter_(valid.codes,
                      .dots = list(~!(icd9 == TRUE & icd10 == TRUE)))

    # find codes which are valid in both ICD9/10 and check if they are defined
    undefined <- filter_(valid.codes,
                         .dots = list(~icd9 == TRUE, ~icd10 == TRUE)) %>%
        mutate_(.dots = set_names(
            x = list(~icd::icd_is_defined(icd::as.icd9cm(diag.code)),
                     ~icd::icd_is_defined(icd::as.icd10cm(diag.code))),
            nm = list("icd9", "icd10")
        ))

    # if code only defined in one type, assign it to the correct group
    icd_defined <- filter_(undefined,
                           .dots = list(~!(icd9 == TRUE & icd10 == TRUE)))

    # for codes defined in both, use the source assignment from EDW
    source_default <- filter_(undefined,
                              .dots = list(~icd9 == TRUE, ~icd10 == TRUE)) %>%
        mutate_(.dots = set_names(
            x = list(~code.source == "ICD-9-CM"),
            nm = "icd9"
        ))

    dplyr::bind_rows(assign, icd_defined, source_default) %>%
        select_(.dots = quote(-icd10))
}

#' @export
#' @rdname tidy_data
tidy_data.labs <- function(x, censor = TRUE, ...) {
    # create a column noting if data was censored
    if (censor == TRUE) {
        x[["censor.low"]] <- stringr::str_detect(x[["lab.result"]], "<")
        x[["censor.high"]] <- stringr::str_detect(x[["lab.result"]], ">")
    }

    # convert lab results to numeric values
    x[["lab.result"]] <- as.numeric(x[["lab.result"]])
    x
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
    arrange_(x, "arrive.datetime") %>%
        group_by_("pie.id") %>%

        # determine if pt went to different unit, count num of different units
        mutate_(.dots = set_names(
            x = list(~is.na(unit.to) | is.na(dplyr::lag(unit.to)) |
                         unit.to != dplyr::lag(unit.to),
                     ~cumsum(diff.unit)),
            nm = list("diff.unit", "unit.count")
        )) %>%

        # use the count to group multiple rows of the same unit together
        group_by_(.dots = list("pie.id", "unit.count")) %>%
        summarise_(.dots = set_names(
            x = list(~dplyr::first(unit.to),
                     ~dplyr::first(arrive.datetime),
                     ~dplyr::last(depart.datetime)),
            nm = list("location", "arrive.datetime", "depart.recorded")
        )) %>%

        # use the arrival time for the next unit to calculate a depart time; if
        # there is no arrival time for the next unit then used the depart
        # date/time from EDW
        group_by_(.dots = "pie.id") %>%
        mutate_(.dots = set_names(
            x = list(~dplyr::lead(arrive.datetime),
                     ~dplyr::coalesce(depart.datetime, depart.recorded)),
            nm = list("depart.datetime", "depart.datetime")
        )) %>%

        ungroup() %>%
        mutate_(.dots = set_names(
            x = list(~difftime(depart.datetime, arrive.datetime, units = "days")),
            nm = "unit.length.stay"
        )) %>%
        select_(.dots = list(quote(-depart.recorded)))
}

#' @export
#' @rdname tidy_data
tidy_data.meds_cont <- function(x, ref, sched, ...) {
    # for any med classes, lookup the meds included in the class
    y <- filter_(ref, .dots = list(~type == "class", ~group == "cont"))
    class.meds <- med_lookup(y$name)

    # join the list of meds with any indivdual meds included
    y <- filter_(ref, .dots = list(~type == "med", ~group == "cont"))
    lookup.meds <- c(y$name, class.meds$med.name)

    # remove any rows in continuous data which are actually scheduled doses,
    # then filter to meds in lookup, then sort by pie.id, med, med.datetime
    x <- anti_join(x, sched, by = "event.id") %>%
        filter_(.dots = list(~med %in% lookup.meds)) %>%
        arrange_(.dots = list("pie.id", "med", "med.datetime"))

    # convert rate to numeric values
    x[["med.rate"]] <- as.numeric(x[["med.rate"]])
    x
}

#' @export
#' @rdname tidy_data
tidy_data.meds_sched <- function(x, ref, ...) {
    # for any med classes, lookup the meds included in the class
    y <- filter_(ref, .dots = list(~type == "class", ~group == "sched"))
    class.meds <- med_lookup(y$name)

    # join the list of meds with any indivdual meds included
    y <- filter_(ref, .dots = list(~type == "med", ~group == "sched"))
    lookup.meds <- c(y$name, class.meds$med.name)

    # filter to keep only meds in lookup
    x <- filter_(x, .dots = list(~med %in% lookup.meds)) %>%
        arrange_(.dots = list("pie.id", "med", "med.datetime"))

    # convert dose to numeric values
    x[["med.dose"]] <- as.numeric(x[["med.dose"]])
    x
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
    arrange_(x, "start.datetime") %>%
        group_by_("pie.id") %>%

        # determine if they went to a different service, then make a count of
        # different services
        mutate_(.dots = set_names(
            x = list(~dplyr::if_else(is.na(service) |
                                         is.na(dplyr::lag(service)) |
                                         service != dplyr::lag(service),
                                     TRUE, FALSE),
                     ~cumsum(diff.service)),
            nm = list("diff.service", "service.count")
        )) %>%

        # use the service.count to group multiple rows of the same service
        # together and combine data
        group_by_(.dots = list("pie.id", "service.count")) %>%
        summarise_(.dots = set_names(
            x = list(~dplyr::first(service),
                     ~dplyr::first(start.datetime),
                     ~dplyr::last(end.datetime)),
            nm = list("service", "start.datetime", "end.recorded")
        )) %>%

        # use the start time for the next service to calculate an end time
        group_by_("pie.id") %>%
        mutate_(.dots = set_names(
            x = list(~dplyr::lead(start.datetime),
                     ~dplyr::if_else(is.na(end.calculated),
                                     difftime(end.recorded,
                                              start.datetime,
                                              units = "days"),
                                     difftime(end.calculated,
                                              start.datetime,
                                              units = "days")
                     )),
            nm = list("end.calculated", "service.duration")
        )) %>%

        ungroup() %>%
        select_(.dots = list(quote(-end.recorded),
                                    quote(-end.calculated)))
}

#' @details For vent_times, this function accounts for incorrect start and stop
#'   times from raw EDW data. If there is not a recorded stop time then the
#'   discharge time will be used as the stop time.
#'
#' @export
#' @rdname tidy_data
tidy_data.vent_times <- function(x, dc, ...) {
    # remove any missing data
    filter_(x, .dots = list(~!is.na(vent.datetime))) %>%
        arrange_("vent.datetime") %>%
        group_by_("pie.id") %>%

        # if it's the first event or the next event is a stop, then count as a
        # new vent event
        mutate_(.dots = set_names(
            x = list(~is.na(dplyr::lag(vent.event)) |
                         vent.event != lag(vent.event),
                     ~cumsum(diff.event)),
            nm = list("diff.event", "event.count")
        )) %>%

        # for each event count, get the first and last date/time
        group_by_(.dots = list("pie.id", "event.count")) %>%
        summarise_(.dots = set_names(
            x = list(~dplyr::first(vent.event),
                     ~dplyr::first(vent.datetime),
                     ~dplyr::last(vent.datetime)),
            nm = list("event", "first.event.datetime", "last.event.datetime")
        )) %>%

        # use the last date/time of the next event as stop date/time; this would
        # be the last stop event if there are multiple stop events in a row. if
        # there isn't a stop date/time because there was start with no stop, use
        # the discharge date/time as stop date/time
        left_join(dc[c("pie.id", "discharge.datetime")], by = "pie.id") %>%
        group_by_("pie.id") %>%
        mutate_(.dots = set_names(
            x = list(~dplyr::lead(last.event.datetime),
                     ~dplyr::coalesce(stop.datetime, discharge.datetime)),
            nm = list("stop.datetime", "stop.datetime")
        )) %>%

        filter_(.dots = list(~event == "vent start time")) %>%
        select_(.dots = list("pie.id",
                             "start.datetime" = "first.event.datetime",
                             "stop.datetime")) %>%
        ungroup() %>%
        mutate_(.dots = set_names(
            x = list(~difftime(stop.datetime, start.datetime, units = "hours")),
            nm = "vent.duration"
        ))
}
