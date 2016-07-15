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

#' @rdname set_edwr_class
#' @export
as.demographics <- function(x) {
    if (missing(x)) stop("Missing object")
    if (is.demographics(x)) return(x)
    if (!is.edwr(x)) x <- as.edwr(x)

    # rename variables to desired names, remove duplicate rows, and make sure
    # variables have the desired types; any extra columns will be left as is
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
as.labs <- function(x) {
    if (missing(x)) x <- character()
    if (is.labs(x)) return(x)
    if (!is.edwr(x)) x <- as.edwr(x)

    # rename variables to desired names and remove duplicate rows; then convert
    # all lab names to lower case to avoid matching errors and set date/time
    # format; any extra columns will be left unchanged
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

    # rename variables to desired names and remove duplicate rows; then convert
    # all lab names to lower case to avoid matching errors and set date/time
    # format; any extra columns will be left unchanged
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
as.meds_cont <- function(x) {
    if (missing(x)) x <- character()
    if (is.meds_cont(x)) return(x)
    if (!is.edwr(x)) x <- as.edwr(x)

    # rename variables to desired names and remove duplicate rows; then convert
    # all med names to lower case to avoid matching errors and set date/time
    # format; any extra columns will be left unchanged
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
as.meds_home <- function(x) {
    if (missing(x)) x <- character()
    if (is.meds_home(x)) return(x)
    if (!is.edwr(x)) x <- as.edwr(x)

    # rename variables to desired names and remove duplicate rows; then convert
    # all med names to lower case to avoid matching errors and set date/time
    # format; any extra columns will be left unchanged
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

    # rename variables to desired names and remove duplicate rows; then convert
    # all med names to lower case to avoid matching errors and set date/time
    # format; any extra columns will be left unchanged
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
as.warfarin <- function(x) {
    if (missing(x)) x <- character()
    if (is.warfarin(x)) return(x)
    if (!is.edwr(x)) x <- as.edwr(x)

    # rename variables to desired names and remove duplicate rows; then convert
    # all lab names to lower case to avoid matching errors and set date/time
    # format; any extra columns will be left unchanged
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
