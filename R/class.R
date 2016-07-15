# class.R
#
# get and set class types used in edwr
#

# default values ---------------------------------------
val.pie <- list("pie.id" = "`PowerInsight Encounter Id`")
val.dt <- "`Clinical Event End Date/Time`"
val.ce <- "`Clinical Event`"
val.res <- "`Clinical Event Result`"

# constructor functions --------------------------------

#' Construct edwr data types
#'
#' Takes an R object and sets class to an edwr type.
#'
#' @param x object to set class \code{edwr}
#'
#' @name set_edwr_class
#' @keywords internal
edwr <- function(x) {
    cl <- class(x)
    if ("edwr" %in% cl) return (x)
    class(x) <- c("edwr", cl)
    x
}

#' @rdname set_edwr_class
#' @export
as.edwr <- function(x) {
    if (missing(x)) x <- character()
    if (is.edwr(x)) return(x)
    after <- match("edwr", class(x), nomatch = 0L)
    class(x) <- append(class(x), "edwr", after = after)
    x
}

# create classes which are children of edwr class; each class represents a
# different type of data from EDW (labs, meds, demographics, etc.); in general,
# these functions will rename variables to desired names, remove duplicate rows,
# convert names (of labs, meds, etc.) to lower case (to avoid case-sensitive
# matching errors when working with the data), parse date/time values into
# POSIXct vectors; any extra columns contained in the csv file will be left
# unchanged

#' @rdname set_edwr_class
#' @export
as.blood <- function(x) {
    if (missing(x)) x <- character()
    if (is.blood(x)) return(x)
    if (!is.edwr(x)) x <- as.edwr(x)

    prods <- c("Cryo(.*)" = "cryo",
               "FFP(.*)" = "ffp",
               "(P)?RBC(.*)" = "prbc",
               "Platelet(.*)" = "platelet")

    df <- rename_(.data = x, .dots = c(val.pie, list(
        "blood.datetime" = val.dt,
        "blood.prod" = val.ce,
        "blood.type" = val.res
    ))) %>%
        dplyr::distinct_() %>%
        mutate_(.dots = set_names(
            x = list(~stringr::str_replace_all(blood.prod, prods)),
            nm = "blood.prod"
        ))

    after <- match("blood", class(x), nomatch = 0L)
    class(df) <- append(class(x), "blood", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.charges <- function(x) {
    if (missing(x)) stop("Missing object")
    if (is.charges(x)) return(x)
    if (!is.edwr(x)) x <- as.edwr(x)

    df <- rename_(.data = x, .dots = c(val.pie, list(
        "cdm.code" = "`Cdm Code`",
        "service.date" = "`Service Date`",
        "institution" = "`Institution Desc`"
    ))) %>%
        dplyr::distinct_()

    after <- match("charges", class(x), nomatch = 0L)
    class(df) <- append(class(x), "charges", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.demographics <- function(x) {
    if (missing(x)) stop("Missing object")
    if (is.demographics(x)) return(x)
    if (!is.edwr(x)) x <- as.edwr(x)

    df <- rename_(.data = x, .dots = c(val.pie, list(
        "age" = "`Age- Years (Visit)`",
        "disposition" = "`Discharge Disposition`",
        "length.stay" = "`LOS (Actual)`",
        "visit.type" = "`Encounter Type`",
        "person.id" = "`Person ID`",
        "facility" = "`Person Location- Facility (Curr)`"
    ))) %>%
        dplyr::distinct_() %>%
        readr::type_convert(col_types = readr::cols(
            pie.id = "c",
            person.id = "c"
        ))

    names(df) <- stringr::str_to_lower(names(df))

    after <- match("demographics", class(x), nomatch = 0L)
    class(df) <- append(class(x), "demographics", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.diagnosis <- function(x) {
    if (missing(x)) stop("Missing object")
    if (is.diagnosis(x)) return(x)
    if (!is.edwr(x)) x <- as.edwr(x)

    df <- rename_(.data = x, .dots = c(val.pie, list(
        "diag.code" = "`Diagnosis Code`",
        "code.source" = "`Diagnosis Code Source Vocabulary`",
        "diag.type" = "`Diagnosis Type`",
        "diag.seq" = "`Diagnosis Code Sequence`",
        "present.admit" = "`Present on Admission`"
    ))) %>%
        dplyr::distinct_()

    after <- match("diagnosis", class(x), nomatch = 0L)
    class(df) <- append(class(x), "diagnosis", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.encounters <- function(x) {
    if (missing(x)) stop("Missing object")
    if (is.encounters(x)) return(x)
    if (!is.edwr(x)) x <- as.edwr(x)

    df <- rename_(.data = x, .dots = c(val.pie, list(
        "person.id" = "`Person ID`",
        "admit.datetime" = "`Admit Date & Time`",
        "visit.type" = "`Encounter Type`",
        "facility" = "`Person Location- Facility (Curr)`",
        "disposition" = "`Discharge Disposition`"
    ))) %>%
        dplyr::distinct_() %>%
        mutate_(.dots = set_names(
            x = list(~format_dates(admit.datetime)),
            nm = "admit.datetime"
        ))

    after <- match("encounters", class(x), nomatch = 0L)
    class(df) <- append(class(x), "encounters", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.events <- function(x) {
    if (missing(x)) x <- character()
    if (is.events(x)) return(x)
    if (!is.edwr(x)) x <- as.edwr(x)

    df <- rename_(.data = x, .dots = c(val.pie, list(
        "event.datetime" = val.dt,
        "event" = val.ce,
        "event.result" = val.res
    ))) %>%
        dplyr::distinct_() %>%
        mutate_(.dots = set_names(
            x = list(~stringr::str_to_lower(event),
                     ~format_dates(event.datetime)),
            nm = list("event", "event.datetime")
        ))

    after <- match("events", class(x), nomatch = 0L)
    class(df) <- append(class(x), "events", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.icu_assess <- function(x) {
    if (missing(x)) x <- character()
    if (is.icu_assess(x)) return(x)
    if (!is.edwr(x)) x <- as.edwr(x)

    df <- rename_(.data = x, .dots = c(val.pie, list(
        "assess.datetime" = val.dt,
        "assessment" = val.ce,
        "assess.result" = val.res
    ))) %>%
        dplyr::distinct_() %>%
        mutate_(.dots = set_names(
            x = list(~stringr::str_to_lower(assessment),
                     ~format_dates(assess.datetime)),
            nm = list("assessment", "assess.datetime")
        ))

    after <- match("icu_assess", class(x), nomatch = 0L)
    class(df) <- append(class(x), "icu_assess", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.id <- function(x) {
    if (missing(x)) stop("Missing object")
    if (is.id(x)) return(x)
    if (!is.edwr(x)) x <- as.edwr(x)

    df <- rename_(.data = x, .dots = c(val.pie, list(
        "fin" = "`Formatted Financial Nbr`",
        "person.id" = "`Person ID`"
    ))) %>%
        dplyr::distinct_()

    after <- match("id", class(x), nomatch = 0L)
    class(df) <- append(class(x), "id", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.labs <- function(x) {
    if (missing(x)) x <- character()
    if (is.labs(x)) return(x)
    if (!is.edwr(x)) x <- as.edwr(x)

    df <- rename_(.data = x, .dots = c(val.pie, list(
        "lab.datetime" = val.dt,
        "lab" = val.ce,
        "lab.result" = val.res
    ))) %>%
        dplyr::distinct_() %>%
        mutate_(.dots = set_names(
            x = list(~stringr::str_to_lower(lab),
                     ~format_dates(lab.datetime)),
            nm = list("lab", "lab.datetime")
        ))

    after <- match("labs", class(x), nomatch = 0L)
    class(df) <- append(class(x), "labs", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.locations <- function(x) {
    if (missing(x)) x <- character()
    if (is.locations(x)) return(x)
    if (!is.edwr(x)) x <- as.edwr(x)

    df <- rename_(.data = x, .dots = c(val.pie, list(
        "arrive.datetime" = "`Location Arrival Date & Time`",
        "depart.datetime" = "`Location Depart Date & Time`",
        "unit.to" = "`Person Location - Nurse Unit (To)`",
        "unit.from" = "`Person Location - Nurse Unit (From)`"
    ))) %>%
        dplyr::distinct_() %>%
        mutate_(.dots = set_names(
            x = list(~format_dates(arrive.datetime),
                     ~format_dates(depart.datetime),
                     ~dplyr::na_if(unit.to, ""),
                     ~dplyr::na_if(unit.from, "")),
            nm = list("arrive.datetime", "depart.datetime", "unit.to",
                      "unit.from")
        ))

    after <- match("locations", class(x), nomatch = 0L)
    class(df) <- append(class(x), "locations", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.measures <- function(x) {
    if (missing(x)) x <- character()
    if (is.measures(x)) return(x)
    if (!is.edwr(x)) x <- as.edwr(x)

    df <- rename_(.data = x, .dots = c(val.pie, list(
        "measure.datetime" = val.dt,
        "measure" = val.ce,
        "measure.result" = val.res,
        "measure.units" = "`Clinical Event Result Units`"
    ))) %>%
        dplyr::distinct_() %>%
        mutate_(.dots = set_names(
            x = list(~stringr::str_to_lower(measure),
                     ~format_dates(measure.datetime)),
            nm = list("measure", "measure.datetime")
        ))

    after <- match("measures", class(x), nomatch = 0L)
    class(df) <- append(class(x), "measures", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.meds_cont <- function(x) {
    if (missing(x)) x <- character()
    if (is.meds_cont(x)) return(x)
    if (!is.edwr(x)) x <- as.edwr(x)

    df <- rename_(.data = x, .dots = c(val.pie, list(
        "order.id" = "`Clinical Event Order ID`",
        "event.id" = "`Event ID`",
        "med.datetime" = val.dt,
        "med" = val.ce,
        "med.rate" = "`Infusion Rate`",
        "med.rate.units" = "`Infusion Rate Unit`",
        "route" = "`Route of Administration - Short`",
        "event.tag" = "`Event Tag`"
    ))) %>%
        dplyr::distinct_() %>%
        mutate_(.dots = set_names(
            x = list(~stringr::str_to_lower(med),
                     ~format_dates(med.datetime),
                     ~dplyr::na_if(med.rate.units, "")),
            nm = list("med", "med.datetime", "med.rate.units")
        ))

    after <- match("meds_cont", class(x), nomatch = 0L)
    class(df) <- append(class(x), "meds_cont", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.meds_freq <- function(x) {
    # inherits from meds_sched
    if (missing(x)) x <- character()
    if (is.meds_freq(x)) return(x)
    if (!is.edwr(x)) x <- as.edwr(x)
    if (!is.meds_sched(x)) x <- as.meds_sched(x)

    df <- rename_(.data = x, .dots = list(
        "freq" = "`Parent Order Frequency Description`"
    ))

    after <- match("meds_freq", class(x), nomatch = 0L)
    class(df) <- append(class(x), "meds_freq", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.meds_home <- function(x) {
    if (missing(x)) x <- character()
    if (is.meds_home(x)) return(x)
    if (!is.edwr(x)) x <- as.edwr(x)

    df <- rename_(.data = x, .dots = c(val.pie, list(
        "med" = "`Order Catalog Short Description`",
        "order.name" = "`Order Catalog Mnemonic`",
        "med.type" = "`Orig Orderable Type-Flag Desc`"
    ))) %>%
        dplyr::distinct_() %>%
        mutate_(.dots = set_names(
            x = list(~stringr::str_to_lower(med)),
            nm = "med"
        ))

    after <- match("meds_home", class(x), nomatch = 0L)
    class(df) <- append(class(x), "meds_home", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.meds_sched <- function(x) {
    if (missing(x)) x <- character()
    if (is.meds_sched(x)) return(x)
    if (!is.edwr(x)) x <- as.edwr(x)

    df <- rename_(.data = x, .dots = c(val.pie, list(
        "order.id" = "`Clinical Event Order ID`",
        "event.id" = "`Event ID`",
        "med.datetime" = val.dt,
        "med" = val.ce,
        "med.dose" = "`Dosage Amount`",
        "med.dose.units" = "`Dosage Unit`",
        "route" = "`Route of Administration - Short`",
        "event.type" = "`Event Type Code`"
    ))) %>%
        dplyr::distinct_() %>%
        mutate_(.dots = set_names(
            x = list(~stringr::str_to_lower(med),
                     ~format_dates(med.datetime)),
            nm = list("med", "med.datetime")
        ))

    after <- match("meds_sched", class(x), nomatch = 0L)
    class(df) <- append(class(x), "meds_sched", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.services <- function(x) {
    if (missing(x)) x <- character()
    if (is.services(x)) return(x)
    if (!is.edwr(x)) x <- as.edwr(x)

    df <- rename_(.data = x, .dots = c(val.pie, list(
        "start.datetime" = "`Medical Service Begin Date & Time`",
        "end.datetime" = "`Medical Service End Date & Time`",
        "service" = "`Medical Service`",
        "service.from" = "`Previous Medical Service`"
    ))) %>%
        dplyr::distinct_() %>%
        mutate_(.dots = set_names(
            x = list(~format_dates(start.datetime),
                     ~format_dates(end.datetime),
                     ~dplyr::na_if(service, ""),
                     ~dplyr::na_if(service.from, "")),
            nm = list("start.datetime", "end.datetime", "service",
                      "service.from")
        ))

    after <- match("services", class(x), nomatch = 0L)
    class(df) <- append(class(x), "services", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.vent_times <- function(x) {
    if (missing(x)) x <- character()
    if (is.vent_times(x)) return(x)
    if (!is.edwr(x)) x <- as.edwr(x)

    df <- rename_(.data = x, .dots = c(val.pie, list(
        "vent.datetime" = "`Clinical Event Date Result`",
        "vent.event" = val.ce
    ))) %>%
        dplyr::distinct_() %>%
        mutate_(.dots = set_names(
            x = list(~stringr::str_to_lower(vent.event),
                     ~format_dates(vent.datetime)),
            nm = list("vent.event", "vent.datetime")
        ))

    after <- match("vent_times", class(x), nomatch = 0L)
    class(df) <- append(class(x), "vent_times", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.visits <- function(x) {
    if (missing(x)) x <- character()
    if (is.visits(x)) return(x)
    if (!is.edwr(x)) x <- as.edwr(x)

    df <- rename_(.data = x, .dots = c(val.pie, list(
        "arrival.datetime" = "`Arrival Date & Time`",
        "admit.datetime" = "`Admit Date & Time`",
        "discharge.datetime" = "`Discharge Date & Time`",
        "visit.type" = "`Encounter Type`",
        "admit.source" = "`Admit Source`",
        "admit.type" = "`Admit Type`",
        "facility" = "`Person Location- Facility (Curr)`",
        "nurse.unit.admit" = "`Person Location- Nurse Unit (Admit)`"
    ))) %>%
        dplyr::distinct_() %>%
        mutate_(.dots = set_names(
            x = list(~format_dates(arrival.datetime),
                     ~format_dates(admit.datetime),
                     ~format_dates(discharge.datetime)),
            nm = list("arrival.datetime", "admit.datetime", "discharge.datetime")
        ))

    after <- match("visits", class(x), nomatch = 0L)
    class(df) <- append(class(x), "visits", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.warfarin <- function(x) {
    if (missing(x)) x <- character()
    if (is.warfarin(x)) return(x)
    if (!is.edwr(x)) x <- as.edwr(x)

    df <- rename_(.data = x, .dots = c(val.pie, list(
        "warfarin.datetime" = val.dt,
        "warfarin.event" = val.ce,
        "warfarin.result" = val.res
    ))) %>%
        dplyr::distinct_() %>%
        mutate_(.dots = set_names(
            x = list(~stringr::str_to_lower(warfarin.event),
                     ~format_dates(warfarin.datetime)),
            nm = list("warfarin.event", "warfarin.datetime")
        ))

    after <- match("warfarin", class(x), nomatch = 0L)
    class(df) <- append(class(x), "warfarin", after = after)
    df
}

# class test functions ---------------------------------

#' Test edwr-related classes
#'
#' Takes an R object and checks for an edwr class type.
#'
#' @param x object which may have an edwr class type
#' @export
is.edwr <- function(x) inherits(x, "edwr")

#' @export
is.blood <- function(x) inherits(x, "blood")

#' @export
is.charges <- function(x) inherits(x, "charges")

#' @export
is.demographics <- function(x) inherits(x, "demographics")

#' @export
is.diagnosis <- function(x) inherits(x, "diagnosis")

#' @export
is.encounters <- function(x) inherits(x, "encounters")

#' @export
is.events <- function(x) inherits(x, "events")

#' @export
is.icu_assess <- function(x) inherits(x, "icu_assess")

#' @export
is.id <- function(x) inherits(x, "id")

#' @export
is.labs <- function(x) inherits(x, "labs")

#' @export
is.locations <- function(x) inherits(x, "locations")

#' @export
is.measures <- function(x) inherits(x, "measures")

#' @export
is.meds_cont <- function(x) inherits(x, "meds_cont")

#' @export
is.meds_freq <- function(x) inherits(x, "meds_freq")

#' @export
is.meds_home <- function(x) inherits(x, "meds_home")

#' @export
is.meds_sched <- function(x) inherits(x, "meds_sched")

#' @export
is.mpp <- function(x) inherits(x, "mpp")

#' @export
is.mrn <- function(x) inherits(x, "mrn")

#' @export
is.order_by <- function(x) inherits(x, "order_by")

#' @export
is.order_detail <- function(x) inherits(x, "order_detail")

#' @export
is.patients <- function(x) inherits(x, "patients")

#' @export
is.problems <- function(x) inherits(x, "problems")

#' @export
is.procedures9 <- function(x) inherits(x, "procedures9")

#' @export
is.procedures10 <- function(x) inherits(x, "procedures10")

#' @export
is.radiology <- function(x) inherits(x, "radiology")

#' @export
is.services <- function(x) inherits(x, "services")

#' @export
is.surgeries <- function(x) inherits(x, "surgeries")

#' @export
is.uop <- function(x) inherits(x, "uop")

#' @export
is.vent_settings <- function(x) inherits(x, "vent_settings")

#' @export
is.vent_times <- function(x) inherits(x, "vent_times")

#' @export
is.visits <- function(x) inherits(x, "visits")

#' @export
is.vitals <- function(x) inherits(x, "vitals")

#' @export
is.warfarin <- function(x) inherits(x, "warfarin")
