# class.R
#
# get and set class types used in edwr
#

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
    df <- dplyr::rename_(.data = x, .dots = list(
        "pie.id" = "`PowerInsight Encounter Id`",
        "age" = "`Age- Years (Visit)`",
        "disposition" = "`Discharge Disposition`",
        "length.stay" = "`LOS (Actual)`",
        "visit.type" = "`Encounter Type`",
        "person.id" = "`Person ID`",
        "facility" = "`Person Location- Facility (Curr)`"
    )) %>%
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
    df <- dplyr::rename_(.data = x, .dots = list(
        "pie.id" = "`PowerInsight Encounter Id`",
        "lab.datetime" = "`Clinical Event End Date/Time`",
        "lab" = "`Clinical Event`",
        "lab.result" = "`Clinical Event Result`"
    )) %>%
        dplyr::distinct_() %>%
        dplyr::mutate_(.dots = purrr::set_names(
            x = list(~stringr::str_to_lower(lab),
                     ~format_dates(lab.datetime)),
            nm = list("lab", "lab.datetime")
        ))

    after <- match("labs", class(x), nomatch = 0L)
    class(df) <- append(class(x), "labs", after = after)
    df
}

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

#' Set the default format for reading date/time variables
#'
#' @return A readr::collector object
#'
#' @keywords internal
format_dates <- function(x) {
    readr::parse_datetime(
        x = x,
        format = "%Y/%m/%d %H:%M:%S",
        locale = readr::locale(tz = "US/Central")
    )
}