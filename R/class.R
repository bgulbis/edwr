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
as.labs <- function(x) {
    if (missing(x)) x <- character()
    if (is.labs(x)) return(x)
    after <- match("labs", class(x), nomatch = 0L)
    class(x) <- append(class(x), "labs", after = after)
    x
}

#' @rdname set_edwr_class
#' @export
as.demographics <- function(x) {
    if (missing(x)) stop("Missing object")
    if (is.demographics(x)) return(x)
    if (!is.edwr(x)) x <- as.edwr(x)

    df <- dplyr::rename_(
        .data = x,
        .dots = list(
            "pie.id" = "`PowerInsight Encounter Id`",
            "age" = "`Age- Years (Visit)`",
            "race" = "Race",
            "disposition" = "`Discharge Disposition`",
            "length.stay" = "`LOS (Actual)`",
            "visit.type" = "`Encounter Type`",
            "person.id" = "`Person ID`",
            "facility" = "`Person Location- Facility (Curr)`"
        )
    ) %>%
        dplyr::distinct_() %>%
        readr::type_convert(
            col_types = readr::cols(
                pie.id = "c",
                age = "i",
                race = "c",
                disposition = "c",
                length.stay = "d",
                visit.type = "c",
                person.id = "c",
                facility = "c"
            ),
            na = c("", "NA", "Unknown")
        )

    after <- match("demographics", class(x), nomatch = 0L)
    class(df) <- append(class(x), "demographics", after = after)
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
is.demographics <- function(x) inherits(x, "demographics")

#' @export
is.labs <- function(x) inherits(x, "labs")

# create generic functions for dplyr verbs which maintain edwr class types

#' @export
filter.edwr <- function(.data, ...) {
    y <- NextMethod()
    class(y) <- class(.data)
    y
}

#' @export
filter_.edwr <- function(.data, ..., .dots) {
    y <- NextMethod()
    class(y) <- class(.data)
    y
}
