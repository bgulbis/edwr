# class.R
#
# get and set class types used in edwr
#

# default values ---------------------------------------
val.pie <- list("pie.id" = "`PowerInsight Encounter Id`")
val.mil <- list("millennium.id" = "`Encounter Identifier`")
val.dt <- "`Clinical Event End Date/Time`"
val.ce <- "`Clinical Event`"
val.res <- "`Clinical Event Result`"

# constructor functions --------------------------------

#' Construct edwr data types
#'
#' Takes an R object and sets class to an edwr type.
#'
#' @param x object to set class \code{tbl_edwr}
#' @param varnames named character list, where the name is the desired variable
#'   name; overrides default names
#' @param extras named character list; if given, will append to default varnames
#'
#' @name set_edwr_class
#' @keywords internal
tbl_edwr <- function(x) {
    cl <- class(x)
    if ("tbl_edwr" %in% cl) return (x)
    class(x) <- c("tbl_edwr", cl)
    x
}

#' @rdname set_edwr_class
#' @export
as.tbl_edwr <- function(x) {
    if (missing(x)) x <- character()
    if (is.tbl_edwr(x)) return(x)
    after <- match("tbl_edwr", class(x), nomatch = 0L)
    class(x) <- append(class(x), "tbl_edwr", after = after)
    x
}

# create classes which inherit from tbl_edwr class; each class represents a
# different type of data from EDW (labs, meds, demographics, etc.); in general,
# these functions will rename variables to desired names, remove duplicate rows,
# convert names (of labs, meds, etc.) to lower case (to avoid case-sensitive
# matching errors when working with the data), parse date/time values into
# POSIXct vectors; any extra columns contained in the csv file will be left
# unchanged

#' @rdname set_edwr_class
#' @export
as.admit <- function(x, varnames = NULL, extras = NULL) {
    if (missing(x)) stop("Missing object")
    if (is.admit(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw" & is.null(varnames)) {
        varnames <- c(val.pie, list(
            "visit.type" = "`Encounter Type`",
            "visit.type.class" = "`Encounter Type Class`",
            "admit.type" = "`Admit Type`",
            "admit.source" = "`Admit Source`"
        ))

        # default CDW/MBO names
    } else if (attr(x, "data") == "mbo" & is.null(varnames)) {
        varnames <- c(val.mil, list(
            "visit.type" = "`Encounter Class Subtype`"
        ))
    }

    # if extra var names are given, append those to the list
    if (!is.null(extras)) {
        varnames <- c(varnames, extras)
    }

    df <- select_(.data = x, .dots = varnames) %>%
        dplyr::distinct_()

    after <- match("admit", class(x), nomatch = 0L)
    class(df) <- append(class(x), "admit", after = after)
    df
}


#' @rdname set_edwr_class
#' @export
as.blood <- function(x, varnames = NULL, extras = NULL) {
    # inherits from events class
    if (missing(x)) x <- character()
    if (is.blood(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw" & is.null(varnames)) {
        if (!is.events(x)) x <- as.events(x)
        varnames <- c(val.pie, list(
            "blood.datetime" = "event.datetime",
            "blood.prod" = "event",
            "blood.type" = "event.result"
        ))

        # default CDW/MBO names
    } else if (attr(x, "data") == "mbo" & is.null(varnames)) {
        varnames <- c(val.mil, list(
            "blood.datetime" = "`Date and Time - Performed`",
            "blood.prod" = "`Clinical Event`",
            "blood.type" = "`Clinical Event Result`",
            # "event.result.units" = "`Clinical Event Result Units`",
            "blood.location" = "`Nurse Unit (Event)`",
            "event.id" = "`Event Id`",
            "event.parent.id" = "`Parent Event Id`",
            "order.id" = "`Order Id`",
            "order.parent.id" = "`Parent Order Id`"

        ))
    }

    # if extra var names are given, append those to the list
    if (!is.null(extras)) {
        varnames <- c(varnames, extras)
    }

    prods <- c("Cryo(.*)" = "cryo",
               "FFP(.*)" = "ffp",
               "(P)?RBC(.*)" = "prbc",
               "Platelet(.*)" = "platelet")

    df <- select_(.data = x, .dots = varnames) %>%
        dplyr::distinct_() %>%
        dmap_at("blood.prod", stringr::str_replace_all, pattern = prods) %>%
        format_dates("blood.datetime")

    after <- match("blood", class(x), nomatch = 0L)
    class(df) <- append(class(x), "blood", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.charges <- function(x) {
    if (missing(x)) stop("Missing object")
    if (is.charges(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

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
as.demographics <- function(x, varnames = NULL, extras = NULL) {
    if (missing(x)) stop("Missing object")
    if (is.demographics(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw" & is.null(varnames)) {
        varnames <- c(val.pie, list(
            "age" = "`Age- Years (Visit)`",
            "gender" = "Sex",
            "race" = "Race",
            "disposition" = "`Discharge Disposition`",
            "length.stay" = "`LOS (Actual)`",
            "visit.type" = "`Encounter Type`",
            "person.id" = "`Person ID`",
            "facility" = "`Person Location- Facility (Curr)`"
        ))

        # default CDW/MBO names
    } else if (attr(x, "data") == "mbo" & is.null(varnames)) {
        varnames <- c(val.mil, list(
            "age" = "`Age- Years (At Admit)`",
            "gender" = "Gender",
            "race" = "Race",
            "disposition" = "`Discharge Disposition`",
            "length.stay" = "`LOS (Curr)`",
            "visit.type" = "`Encounter Class Subtype`",
            "facility" = "`Facility (Curr)`"
        ))
    }

    # if extra var names are given, append those to the list
    if (!is.null(extras)) {
        varnames <- c(varnames, extras)
    }

    df <- select_(.data = x, .dots = varnames) %>%
        dplyr::distinct_() %>%
        purrrlyr::dmap_at(c("age", "length.stay"), as.numeric)

    after <- match("demographics", class(x), nomatch = 0L)
    class(df) <- append(class(x), "demographics", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.diagnosis <- function(x, varnames = NULL, extras = NULL) {
    if (missing(x)) stop("Missing object")
    if (is.diagnosis(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw" & is.null(varnames)) {
        varnames <- c(val.pie, list(
            "diag.code" = "`Diagnosis Code`",
            "code.source" = "`Diagnosis Code Source Vocabulary`",
            "diag.type" = "`Diagnosis Type`",
            "diag.seq" = "`Diagnosis Code Sequence`",
            "present.admit" = "`Present on Admission`"
        ))

    # default CDW/MBO names
    } else if (attr(x, "data") == "mbo" & is.null(varnames)) {
        varnames <- c(val.mil, list(
            "diag.code" = "`Diagnosis Code`",
            "code.source" = "`Diagnosis Code Source Vocabulary`",
            "diag.type" = "`Diagnosis Type`",
            "diag.seq" = "`Diagnosis Code (Primary VS Non Primary)`"
        ))
    }

    # if extra var names are given, append those to the list
    if (!is.null(extras)) {
        varnames <- c(varnames, extras)
    }

    df <- select_(.data = x, .dots = varnames) %>%
        dplyr::distinct_()

    after <- match("diagnosis", class(x), nomatch = 0L)
    class(df) <- append(class(x), "diagnosis", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.drg <- function(x) {
    if (missing(x)) stop("Missing object")
    if (is.drg(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    df <- select_(.data = x, .dots = c(val.pie, list(
        "drg" = "`Grouping Code`",
        "drg.source" = "`Grouping Code Source Vocabulary`",
        "drg.type" = "`Grouping Code Type`"
    ))) %>%
        dplyr::distinct_()

    after <- match("drg", class(x), nomatch = 0L)
    class(df) <- append(class(x), "drg", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.encounters <- function(x, varnames = NULL, extras = NULL) {
    if (missing(x)) stop("Missing object")
    if (is.encounters(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw" & is.null(varnames)) {
        varnames <- c(val.pie, list(
            "millennium.id" = "`Millennium Encounter ID`",
            "person.id" = "`Person ID`",
            "admit.datetime" = "`Admit Date & Time`",
            "visit.type" = "`Encounter Type`",
            "facility" = "`Person Location- Facility (Curr)`",
            "disposition" = "`Discharge Disposition`"
        ))

        # default CDW/MBO names
    } else if (attr(x, "data") == "mbo" & is.null(varnames)) {
        varnames <- c(val.mil, list(
            "admit.datetime" = "`Date and Time - Admit`",
            "visit.type" = "`Encounter Class Subtype`",
            "facility" = "`Facility (Curr)`",
            "disposition" = "`Discharge Disposition`"
        ))
    }

    # if extra var names are given, append those to the list
    if (!is.null(extras)) {
        varnames <- c(varnames, extras)
    }

    df <- select_(.data = x, .dots = varnames) %>%
        dplyr::distinct_() %>%
        format_dates("admit.datetime")

    after <- match("encounters", class(x), nomatch = 0L)
    class(df) <- append(class(x), "encounters", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.events <- function(x, varnames = NULL, extras = NULL) {
    if (missing(x)) x <- character()
    if (is.events(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw" & is.null(varnames)) {
        varnames <- c(val.pie, list(
            "event.datetime" = val.dt,
            "event" = val.ce,
            "event.result" = val.res,
            "event.location" = "`Nurse Unit of Clinical Event`",
            "order.id" = "`Clinical Event Order ID`"
        ))

        # default CDW/MBO names
    } else if (attr(x, "data") == "mbo" & is.null(varnames)) {
        varnames <- c(val.mil, list(
            "event.datetime" = "`Date and Time - Performed`",
            "event" = val.ce,
            "event.result" = val.res,
            "event.result.units" = "`Clinical Event Result Units`",
            "event.location" = "`Nurse Unit (Event)`",
            "event.id" = "`Event Id`",
            "event.parent.id" = "`Parent Event Id`",
            "order.id" = "`Order Id`",
            "order.parent.id" = "`Parent Order Id`"
        ))
    }

    # if extra var names are given, append those to the list
    if (!is.null(extras)) {
        varnames <- c(varnames, extras)
    }

    df <- select_(.data = x, .dots = varnames) %>%
        dplyr::distinct_() %>%
        format_dates("event.datetime") %>%
        purrrlyr::dmap_at("event", stringr::str_to_lower)

    after <- match("events", class(x), nomatch = 0L)
    class(df) <- append(class(x), "events", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.icu_assess <- function(x) {
    # inherits from events class
    if (missing(x)) x <- character()
    if (is.icu_assess(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)
    if (!is.events(x)) x <- as.events(x)

    df <- rename_(.data = x, .dots = list(
        "assess.datetime" = "event.datetime",
        "assessment" = "event",
        "assess.result" = "event.result"
    ))

    after <- match("icu_assess", class(x), nomatch = 0L)
    class(df) <- append(class(x), "icu_assess", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.id <- function(x, varnames = NULL, extras = NULL) {
    if (missing(x)) stop("Missing object")
    if (is.id(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw" & is.null(varnames)) {
        varnames <- c(val.pie, list(
            "millennium.id" = "`Millennium Encounter ID`",
            "fin" = "`Formatted Financial Nbr`",
            "person.id" = "`Person ID`"
        ))

        # default CDW/MBO names
    } else if (attr(x, "data") == "mbo" & is.null(varnames)) {
        varnames <- c(val.mil, list(
            "fin" = "`Financial Number`"
        ))
    }

    # if extra var names are given, append those to the list
    if (!is.null(extras)) {
        varnames <- c(varnames, extras)
    }

    df <- select_(.data = x, .dots = varnames) %>%
        dplyr::distinct_()

    after <- match("id", class(x), nomatch = 0L)
    class(df) <- append(class(x), "id", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.labs <- function(x, varnames = NULL, extras = NULL) {
    # inherits from events class
    if (missing(x)) x <- character()
    if (is.labs(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw" & is.null(varnames)) {
        # if (!is.events(x)) x <- as.events(x)
        varnames <- c(val.pie, list(
            "lab.datetime" = val.dt,
            "lab" = val.ce,
            "lab.result" = val.res
        ))

        # default CDW/MBO names
    } else if (attr(x, "data") == "mbo" & is.null(varnames)) {
        varnames <- c(val.mil, list(
            "lab.datetime" = "`Date and Time - Nurse Draw`",
            "lab" = "`Lab Event (FILTER ON)`",
            "lab.result" = "`Lab Result`",
            "lab.result.units" = "`Lab Result Units`",
            "lab.draw.location" = "`Nurse Unit (Lab)`",
            "lab.id" = "`Lab Event Id`"
        ))
    }

    # if extra var names are given, append those to the list
    if (!is.null(extras)) {
        varnames <- c(varnames, extras)
    }

    df <- select_(.data = x, .dots = varnames) %>%
        dplyr::distinct_() %>%
        purrrlyr::dmap_at("lab", stringr::str_to_lower) %>%
        format_dates("lab.datetime")

    after <- match("labs", class(x), nomatch = 0L)
    class(df) <- append(class(x), "labs", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.locations <- function(x, varnames = NULL, extras = NULL) {
    if (missing(x)) x <- character()
    if (is.locations(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw" & is.null(varnames)) {
        varnames <- c(val.pie, list(
            "arrive.datetime" = "`Location Arrival Date & Time`",
            "depart.datetime" = "`Location Depart Date & Time`",
            "unit.to" = "`Person Location - Nurse Unit (To)`",
            "unit.from" = "`Person Location - Nurse Unit (From)`"
        ))

        # default CDW/MBO names
    } else if (attr(x, "data") == "mbo" & is.null(varnames)) {
        varnames <- c(val.mil, list(
            "arrive.datetime" = "`Date and Time - Nurse Unit Begin`",
            "depart.datetime" = "`Date and Time - Nurse Unit End`",
            "unit.name" = "`Nurse Unit All`"
        ))
    }

    # if extra var names are given, append those to the list
    if (!is.null(extras)) {
        varnames <- c(varnames, extras)
    }

    df <- select_(.data = x, .dots = varnames) %>%
        dplyr::distinct_() %>%
        format_dates(c("arrive.datetime", "depart.datetime"))

    # if (attr(x, "data") == "edw") {
    #     df <- purrrlyr::dmap_at(df, dplyr::na_if, y = "")
    # }

    after <- match("locations", class(x), nomatch = 0L)
    class(df) <- append(class(x), "locations", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.measures <- function(x, varnames = NULL, extras = NULL) {
    # inherits from events class
    if (missing(x)) x <- character()
    if (is.measures(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw" & is.null(varnames)) {
        if (!is.events(x)) x <- as.events(x)
        varnames <- c(val.pie, list(
            "measure.datetime" = "event.datetime",
            "measure" = "event",
            "measure.result" = "event.result",
            "measure.units" = "`Clinical Event Result Units`"
        ))

        # default CDW/MBO names
    } else if (attr(x, "data") == "mbo" & is.null(varnames)) {
        varnames <- c(val.mil, list(
            "measure.datetime" = "`Date and Time - Performed`",
            "measure" = "`Clinical Event`",
            "measure.result" = "`Clinical Event Result`",
            "measure.units" = "`Clinical Event Result Units`"
        ))
    }

    # if extra var names are given, append those to the list
    if (!is.null(extras)) {
        varnames <- c(varnames, extras)
    }

    df <- select_(.data = x, .dots = varnames) %>%
        dplyr::distinct_() %>%
        purrrlyr::dmap_at("measure", stringr::str_to_lower) %>%
        purrrlyr::dmap_at("measure.result", as.numeric) %>%
        format_dates("measure.datetime")

    after <- match("measures", class(x), nomatch = 0L)
    class(df) <- append(class(x), "measures", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.meds_cont <- function(x) {
    if (missing(x)) x <- character()
    if (is.meds_cont(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

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
        purrrlyr::dmap_at("med", stringr::str_to_lower) %>%
        purrrlyr::dmap_at("med.rate.units", dplyr::na_if, y = "") %>%
        format_dates("med.datetime")

    after <- match("meds_cont", class(x), nomatch = 0L)
    class(df) <- append(class(x), "meds_cont", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.meds_freq <- function(x) {
    # inherits from meds_sched class
    if (missing(x)) x <- character()
    if (is.meds_freq(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)
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
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    df <- select_(.data = x, .dots = c(val.pie, list(
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
as.meds_inpt <- function(x, varnames = NULL, extras = NULL) {
    if (missing(x)) x <- character()
    if (is.meds_cont(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw" & is.null(varnames)) {
        varnames <- c(val.pie, list(
            "order.id" = "`Clinical Event Order ID`",
            "event.id" = "`Event ID`",
            "med.datetime" = val.dt,
            "med" = val.ce,
            "med.rate" = "`Infusion Rate`",
            "med.rate.units" = "`Infusion Rate Unit`",
            "route" = "`Route of Administration - Short`",
            "event.tag" = "`Event Tag`"
        ))

        # default CDW/MBO names
    } else if (attr(x, "data") == "mbo" & is.null(varnames)) {
        varnames <- c(val.mil, list(
            "order.id" = "`Order Id`",
            "order.parent.id" = "`Parent Order Id`",
            "event.id" = "`Med Event Id`",
            "med.datetime" = "`Date and Time - Administration`",
            "med" = "`Medication (Generic)`",
            "med.dose" = "`Admin Dosage`",
            "med.dose.units" = "`Admin Dosage Unit`",
            "med.rate" = "`Infusion Rate`",
            "med.rate.units" = "`Infusion Unit`",
            "route" = "`Admin Route`",
            "event.tag" = "`Infusion Actions`",
            "med.location" = "`Nurse Unit (Med)`"
        ))
    }

    # if extra var names are given, append those to the list
    if (!is.null(extras)) {
        varnames <- c(varnames, extras)
    }

    df <- select_(.data = x, .dots = varnames) %>%
        dplyr::distinct_() %>%
        purrrlyr::dmap_at("med", stringr::str_to_lower) %>%
        purrrlyr::dmap_at(c("med.dose", "med.rate"), as.numeric) %>%
        # purrrlyr::dmap_at("med.rate.units", ~dplyr::na_if(.x, "")) %>%
        format_dates("med.datetime")

    after <- match("meds_inpt", class(x), nomatch = 0L)
    class(df) <- append(class(x), "meds_inpt", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.meds_sched <- function(x) {
    if (missing(x)) x <- character()
    if (is.meds_sched(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

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
        purrrlyr::dmap_at("med", stringr::str_to_lower) %>%
        format_dates("med.datetime")

    after <- match("meds_sched", class(x), nomatch = 0L)
    class(df) <- append(class(x), "meds_sched", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.mrn <- function(x) {
    if (missing(x)) stop("Missing object")
    if (is.mrn(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    df <- rename_(.data = x, .dots = list(
        "mrn" = "`MRN- Organization`",
        "person.id" = "`Person ID`"
    )) %>%
        dplyr::distinct_()

    after <- match("mrn", class(x), nomatch = 0L)
    class(df) <- append(class(x), "mrn", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.mpp <- function(x) {
    if (missing(x)) stop("Missing object")
    if (is.mpp(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    df <- rename_(.data = x, .dots = c(val.pie, list(
        "mpp" = "`MPP (which generated order)`"
    ))) %>%
        dplyr::distinct_()

    after <- match("mpp", class(x), nomatch = 0L)
    class(df) <- append(class(x), "mpp", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.order_action <- function(x, varnames = NULL, extras = NULL) {
    if (missing(x)) x <- character()
    if (is.order_action(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw" & is.null(varnames)) {
        varnames <- c(val.pie, list(
            "order.id" = "`Source Order ID`",
            "action.datetime" = "`Order Action Date & Time`",
            "provider" = "`Action Provider`",
            "provider.role" = "`Action Provider Position`",
            "action.type" = "`Action Type`",
            "action.provider" = "`Action Personnel`",
            "action.provider.role" = "`Action Personnel Position`",
            "action.comm" = "`Action Communication Type`",
            "order.status" = "`Order Department Status - Generic`"
        ))

        # default CDW/MBO names
    } else if (attr(x, "data") == "mbo" & is.null(varnames)) {
        varnames <- c(val.mil, list(
            "order.id" = "`Order Id`",
            "order.parent.id" = "`Parent Order Id`",
            "action.datetime" = "`Date and Time - Order Action`",
            "order" = "`Mnemonic (Primary Generic) FILTER ON`",
            "order.location" = "`Nurse Unit (Order)`",
            "action.type" = "`Order Action Type`",
            "action.provider" = "`Order Action Personnel`",
            "action.provider.role" = "`Order Action Personnel Position`",
            "action.comm" = "`Order Communication Type`",
            "order.status" = "`Order Status (Curr)`"
        ))
    }

    # if extra var names are given, append those to the list
    if (!is.null(extras)) {
        varnames <- c(varnames, extras)
    }

    df <- select_(.data = x, .dots = varnames) %>%
        dplyr::distinct_() %>%
        format_dates("action.datetime")

    after <- match("order_action", class(x), nomatch = 0L)
    class(df) <- append(class(x), "order_action", after = after)
    df
}


#' @rdname set_edwr_class
#' @export
as.order_by <- function(x) {
    if (missing(x)) x <- character()
    if (is.order_by(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    colnm <- list(
        "order" = "`Order Catalog Mnemonic`",
        "order.unit" = "`Person Location- Nurse Unit (Order)`",
        "provider" = "`Action Provider`",
        "provider.role" = "`Action Provider Position`",
        "action.datetime" = "`Order Action Date & Time`",
        "action.type" = "`Action Type`",
        "action.provider" = "`Action Personnel`",
        "action.provider.role" = "`Action Personnel Position`",
        "action.comm" = "`Action Communication Type`"
    )

    if ("Source Order ID" %in% names(x)) {
        colnm <- c(list("order.id" = "`Source Order ID`"), colnm)
    }

    if ("Order Department Status - Generic" %in% names(x)) {
        colnm <- c(colnm, list("order.status" = "`Order Department Status - Generic`"))
    }

    df <- rename_(.data = x, .dots = c(val.pie, colnm)) %>%
        dplyr::distinct_() %>%
        format_dates("action.datetime")

    after <- match("order_by", class(x), nomatch = 0L)
    class(df) <- append(class(x), "order_by", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.order_detail <- function(x, varnames = NULL, extras = NULL) {
    if (missing(x)) x <- character()
    if (is.order_detail(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw" & is.null(varnames)) {
        varnames <- c(val.pie, list(
            "order.id" = "`Source Order ID`",
            "order" = "`Order Catalog Mnemonic`",
            "ingredient" = "`Ingredient Catalog Short Desc`",
            "ingredient.dose" = "`Dose Strength - Order`",
            "ingredient.unit" = "`Dose Strength - Unit`",
            "ingredient.freetext" = "`Dose Freetext`",
            "order.unit" = "`Person Location- Nurse Unit (Order)`",
            "action.type" = "`Action Type`",
            "action.datetime" = "`Order Action Date & Time`"
        ))

        dt_name <- "action.datetime"

        # default CDW/MBO names
    } else if (attr(x, "data") == "mbo" & is.null(varnames)) {
        varnames <- c(val.mil, list(
            "order.id" = "`Order Id`",
            "order.datetime" = "`Date and Time - Original (Placed)`",
            "order" = "`Mnemonic (Primary Generic) FILTER ON`",
            "ingredient.dose" = "`Order Strength Dose`",
            "ingredient.unit" = "`Order Strength Dose Unit`",
            "route" = "`Order Route`",
            "freq" = "Frequency",
            "order.provider" = "`Ordering Provider LIMITS`",
            "order.provider.position" = "`Ordering Provider Position LIMITS`"
        ))

        dt_name <- "order.datetime"
    }

    # if extra var names are given, append those to the list
    if (!is.null(extras)) {
        varnames <- c(varnames, extras)
    }

    df <- select_(.data = x, .dots = varnames) %>%
        dplyr::distinct_() %>%
        format_dates(dt_name)

    after <- match("order_detail", class(x), nomatch = 0L)
    class(df) <- append(class(x), "order_detail", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.order_info <- function(x) {
    if (missing(x)) x <- character()
    if (is.order_info(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    df <- rename_(.data = x, .dots = c(val.pie, list(
        "order.id" = "`Source Order ID`",
        "detail.datetime" = "`Detail Date & Time`",
        "detail" = "`Detail Display Value`",
        "detail.descr" = "`Field Description`"
    ))) %>%
        dplyr::distinct_() %>%
        format_dates("detail.datetime")
        # mutate_(.dots = set_names(
        #     x = list(~format_dates(detail.datetime)),
        #     nm = list("detail.datetime")
        # ))

    after <- match("order_info", class(x), nomatch = 0L)
    class(df) <- append(class(x), "order_info", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.order_timing <- function(x) {
    if (missing(x)) x <- character()
    if (is.order_timing(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    colnm <- list(
        "order.id" = "`Source Order ID`",
        "order" = "`Order Catalog Mnemonic`",
        "order.unit" = "`Person Location- Nurse Unit (Order)`",
        "order.datetime" = "`Order Date & Time`",
        "request.datetime" = "`Order Request Date & Time`",
        # "review.datetime" = "`Review Date and Time`",
        # "review.person" = "`Review Personnel`",
        "complete.datetime" = "`Order Complete Date & Time`",
        "complete.person" = "`Complete Personnel`",
        "discontinue.datetime" = "`Order Discontinue Date & Time`",
        "cancel.datetime" = "`Order Cancel Date & Time`",
        "cancel.person" = "`Order Personnel - Cancel`",
        "cancel.reason" = "`Order Cancel Reason`"
    )

    dtm <- c("order.datetime",
             "request.datetime",
             # "review.datetime",
             "complete.datetime",
             "discontinue.datetime",
             "cancel.datetime")

    df <- rename_(.data = x, .dots = c(val.pie, colnm)) %>%
        dplyr::distinct_() %>%
        format_dates(dtm)
        # purrrlyr::dmap_at(dtm, format_dates)

    after <- match("order_timing", class(x), nomatch = 0L)
    class(df) <- append(class(x), "order_timing", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.output <- function(x, varnames = NULL, extras = NULL) {
    # inherits from events class
    if (missing(x)) x <- character()
    if (is.output(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)
    # if (!is.events(x)) x <- as.events(x)

    # default EDW names
    if (attr(x, "data") == "edw" & is.null(varnames)) {
        varnames <- c(val.pie, list(
            "output.datetime" = val.dt,
            "output" = val.ce,
            "output.result" = val.res
        ))

        # default CDW/MBO names
    } else if (attr(x, "data") == "mbo" & is.null(varnames)) {
        varnames <- c(val.mil, list(
            "output.datetime" = "`Date and Time - Performed`",
            "output" = val.ce,
            "output.result" = val.res,
            "output.result.units" = "`Clinical Event Result Units`",
            "output.location" = "`Nurse Unit (Event)`",
            "event.id" = "`Event Id`",
            "event.parent.id" = "`Parent Event Id`"
        ))
    }

    # if extra var names are given, append those to the list
    if (!is.null(extras)) {
        varnames <- c(varnames, extras)
    }

    df <- rename_(.data = x, .dots = varnames) %>%
        dplyr::distinct_() %>%
        purrrlyr::dmap_at("output.result", as.numeric) %>%
        format_dates("output.datetime")

    after <- match("output", class(x), nomatch = 0L)
    class(df) <- append(class(x), "output", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.pain_scores <- function(x, varnames = NULL, extras = NULL) {
    if (missing(x)) x <- character()
    if (is.events(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw" & is.null(varnames)) {
        varnames <- c(val.pie, list(
            "event.datetime" = val.dt,
            "event" = val.ce,
            "event.result" = val.res,
            "event.location" = "`Nurse Unit of Clinical Event`",
            "order.id" = "`Clinical Event Order ID`"
        ))

        # default CDW/MBO names
    } else if (attr(x, "data") == "mbo" & is.null(varnames)) {
        varnames <- c(val.mil, list(
            "event.datetime" = "`Date and Time - Scheduled OR Given On`",
            "event" = val.ce,
            "event.result" = val.res,
            "event.result.units" = "`Clinical Event Result Units`",
            "event.location" = "`Nurse Unit (Event)`",
            "event.id" = "`Event Id`",
            "event.parent.id" = "`Parent Event Id`",
            "order.id" = "`Clinical Event Order Id`"
        ))
    }

    # if extra var names are given, append those to the list
    if (!is.null(extras)) {
        varnames <- c(varnames, extras)
    }

    df <- select_(.data = x, .dots = varnames) %>%
        dplyr::distinct_() %>%
        format_dates("event.datetime") %>%
        purrrlyr::dmap_at("event", stringr::str_to_lower)

    after <- match("events", class(x), nomatch = 0L)
    class(df) <- append(class(x), "events", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.patients <- function(x, varnames = NULL, extras = NULL) {
    if (missing(x)) stop("Missing object")
    if (is.patients(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw" & is.null(varnames)) {
        varnames <- c(val.pie, list(
            "age" = "`Age- Years (Visit)`",
            "discharge.datetime" = "`Discharge Date & Time`",
            "visit.type" = "`Encounter Type`",
            "facility" = "`Person Location- Facility (Curr)`"
        ))

    # default CDW/MBO names
    } else if (attr(x, "data") == "mbo" & is.null(varnames)) {
        varnames <- c(val.mil, list(
            "age" = "`Age- Years (At Admit)`",
            "discharge.datetime" = "`Date and Time - Discharge`",
            "visit.type" = "`Encounter Class Subtype`",
            "facility" = "`Facility (Curr)`"
        ))
    }

    # if extra var names are given, append those to the list
    if (!is.null(extras)) {
        varnames <- c(varnames, extras)
    }

    df <- rename_(.data = x, .dots = varnames) %>%
        dplyr::distinct_() %>%
        purrrlyr::dmap_at("age", as.numeric) %>%
        format_dates("discharge.datetime")

    after <- match("patients", class(x), nomatch = 0L)
    class(df) <- append(class(x), "patients", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.problems <- function(x) {
    if (missing(x)) stop("Missing object")
    if (is.problems(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    df <- rename_(.data = x, .dots = c(val.pie, list(
        "problem" = "`Problem - Description`",
        "classification" = "`Problem Classification`",
        "confirm" = "`Problem Confirmation Status`",
        "free.text" = "`Problem Free Text`",
        "severity" = "`Problem Severity`",
        "active" = "`Problem Source Active Indicator`",
        "onset.datetime" = "`Problem Onset Date & Time`",
        "life.cycle.datetime" = "`Problem Life Cycle Date & Time`",
        "life.cycle" = "`Problem Life Cycle`"
    ))) %>%
        dplyr::distinct_() %>%
        mutate_(.dots = set_names(
            x = list(~format_dates(onset.datetime),
                     ~format_dates(life.cycle.datetime)),
            nm = list("onset.datetime", "life.cycle.datetime")
        ))

    after <- match("problems", class(x), nomatch = 0L)
    class(df) <- append(class(x), "problems", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.procedures <- function(x, varnames = NULL, extras = NULL) {
    if (missing(x)) stop("Missing object")
    if (is.procedures(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw" & is.null(varnames)) {
        varnames <- c(val.pie, list(
            "proc.date" = "`Procedure Date and Time`",
            "proc.code" = "`Procedure Code`",
            "proc.source" = "`Procedure Code Source Vocabulary`",
            "proc.seq" = "`Procedure Code Sequence`"
        ))

        # default CDW/MBO names
    } else if (attr(x, "data") == "mbo" & is.null(varnames)) {
        varnames <- c(val.mil, list(
            "proc.date" = "`Procedure Date and Time`",
            "proc.code" = "`Procedure Code`",
            "proc.source" = "`Diagnosis Code Source Vocabulary`",
            "proc.seq" = "`Procedure Code Sequence`"
        ))
    }

    # if extra var names are given, append those to the list
    if (!is.null(extras)) {
        varnames <- c(varnames, extras)
    }

    df <- rename_(.data = x, .dots = varnames) %>%
        dplyr::distinct_() %>%
        format_dates("proc.date")

    after <- match("procedures", class(x), nomatch = 0L)
    class(df) <- append(class(x), "procedures", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.radiology <- function(x) {
    if (missing(x)) stop("Missing object")
    if (is.radiology(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    df <- rename_(.data = x, .dots = c(val.pie, list(
        "rad.datetime" = val.dt,
        "rad.type" = val.ce
    ))) %>%
        dplyr::distinct_() %>%
        mutate_(.dots = set_names(
            x = list(~format_dates(rad.datetime)),
            nm = "rad.datetime"
        ))

    after <- match("radiology", class(x), nomatch = 0L)
    class(df) <- append(class(x), "radiology", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.services <- function(x) {
    if (missing(x)) x <- character()
    if (is.services(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

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
as.surgery_times <- function(x, varnames = NULL, extras = NULL) {
    if (missing(x)) x <- character()
    if (is.vent_times(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw" & is.null(varnames)) {
        varnames <- c(val.pie, list(
            surgery_start = `Start Date/Time`,
            surgery_stop = `Stop Date/Time`,
            room_in = `Patient In Room Date/Time`,
            room_out = `Patient Out Room Date/Time`,
            recovery_in = `Patient In Recovery Date/Time`,
            recovery_out = `Patient Out Recovery Date/Time`
        ))
    }

    # if extra var names are given, append those to the list
    if (!is.null(extras)) {
        varnames <- c(varnames, extras)
    }

    df <- select_(.data = x, .dots = varnames) %>%
        dplyr::distinct_() %>%
        format_dates(c("surgery_start",
                       "surgery_stop",
                       "room_in",
                       "room_out",
                       "recovery_in",
                       "recovery_out"))

    after <- match("surgery_times", class(x), nomatch = 0L)
    class(df) <- append(class(x), "surgery_times", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.surgeries <- function(x) {
    if (missing(x)) stop("Missing object")
    if (is.surgeries(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    df <- rename_(.data = x, .dots = c(val.pie, list(
        "asa.class" = "`ASA Class Description`",
        "add.on" = "`Add On Indicator`",
        "surgery" = "Procedure",
        "primary.proc" = "`Primary Procedure Indicator`",
        "surg.start.datetime" = "`Start Date/Time`",
        "surg.stop.datetime" = "`Stop Date/Time`"
    ))) %>%
        dplyr::distinct_() %>%
        mutate_(.dots = set_names(
            x = list(~format_dates(surg.start.datetime),
                     ~format_dates(surg.stop.datetime),
                     ~dplyr::if_else(add.on == 1, TRUE, FALSE),
                     ~dplyr::if_else(primary.proc == 1, TRUE, FALSE)),
            nm = list("surg.start.datetime", "surg.stop.datetime", "add.on",
                      "primary.proc")
        ))

    names(df) <- stringr::str_to_lower(names(df))

    after <- match("surgeries", class(x), nomatch = 0L)
    class(df) <- append(class(x), "surgeries", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.uop <- function(x) {
    # inherits from events class
    if (missing(x)) x <- character()
    if (is.uop(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)
    if (!is.events(x)) x <- as.events(x)

    df <- rename_(.data = x, .dots = list(
        "uop.datetime" = "event.datetime",
        "uop" = "event",
        "uop.result" = "event.result"
    ))

    after <- match("uop", class(x), nomatch = 0L)
    class(df) <- append(class(x), "uop", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.vent_settings <- function(x) {
    # inherits from events class
    if (missing(x)) x <- character()
    if (is.vent_settings(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)
    if (!is.events(x)) x <- as.events(x)

    df <- rename_(.data = x, .dots = list(
        "vent.datetime" = "event.datetime",
        "vent.event" = "event",
        "vent.result" = "event.result"
    ))

    after <- match("vent_settings", class(x), nomatch = 0L)
    class(df) <- append(class(x), "vent_settings", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.vent_times <- function(x, varnames = NULL, extras = NULL) {
    if (missing(x)) x <- character()
    if (is.vent_times(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw" & is.null(varnames)) {
        varnames <- c(val.pie, list(
            "vent.datetime" = "`Clinical Event Date Result`",
            "vent.event" = val.ce
        ))

        # default CDW/MBO names
    } else if (attr(x, "data") == "mbo" & is.null(varnames)) {
        varnames <- c(val.mil, list(
            "vent.datetime" = "`Date and Time - Performed`",
            "vent.event" = val.ce,
            "vent.location" = "`Nurse Unit (Event)`",
            "event.id" = "`Event Id`",
            "event.parent.id" = "`Parent Event Id`",
            "order.id" = "`Order Id`",
            "order.parent.id" = "`Parent Order Id`"
        ))
    }

    # if extra var names are given, append those to the list
    if (!is.null(extras)) {
        varnames <- c(varnames, extras)
    }

    df <- select_(.data = x, .dots = varnames) %>%
        dplyr::distinct_() %>%
        purrrlyr::dmap_at("vent.event", stringr::str_to_lower) %>%
        format_dates("vent.datetime")

    after <- match("vent_times", class(x), nomatch = 0L)
    class(df) <- append(class(x), "vent_times", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.visits <- function(x) {
    if (missing(x)) x <- character()
    if (is.visits(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

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
        format_dates("arrival.datetime") %>%
        format_dates("admit.datetime") %>%
        format_dates("discharge.datetime")

    after <- match("visits", class(x), nomatch = 0L)
    class(df) <- append(class(x), "visits", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.vitals <- function(x, varnames = NULL, extras = NULL) {
    # inherits from events class
    if (missing(x)) x <- character()
    if (is.vitals(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw" & is.null(varnames)) {
        varnames <- c(val.pie, list(
            "vital.datetime" = val.dt,
            "vital" = val.ce,
            "vital.result" = val.res,
            "vital.result.units" = "`Clinical Event Result Units`"
        ))

        # default CDW/MBO names
    } else if (attr(x, "data") == "mbo" & is.null(varnames)) {
        varnames <- c(val.mil, list(
            "vital.datetime" = "`Date and Time - Performed`",
            "vital" = "`Clinical Event`",
            "vital.result" = "`Clinical Event Result`",
            "vital.result.units" = "`Clinical Event Result Units`",
            "vital.location" = "`Nurse Unit (Event)`",
            "vital.id" = "`Event Id`",
            "vital.parent.id" = "`Parent Event Id`"
        ))
    }


    # if extra var names are given, append those to the list
    if (!is.null(extras)) {
        varnames <- c(varnames, extras)
    }

    df <- select_(.data = x, .dots = varnames) %>%
        dplyr::distinct_() %>%
        purrrlyr::dmap_at("vital", stringr::str_to_lower) %>%
        purrrlyr::dmap_at("vital.result", as.numeric) %>%
        format_dates("vital.datetime")

    after <- match("vitals", class(x), nomatch = 0L)
    class(df) <- append(class(x), "vitals", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.warfarin <- function(x, varnames = NULL, extras = NULL) {
    # inherits from events class
    if (missing(x)) x <- character()
    if (is.warfarin(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)
    # if (!is.events(x)) x <- as.events(x)

    # default EDW names
    if (attr(x, "data") == "edw" & is.null(varnames)) {
        varnames <- c(val.pie, list(
            "warfarin.datetime" = val.dt,
            "warfarin.event" = val.ce,
            "warfarin.result" = val.res
        ))

        # default CDW/MBO names
    } else if (attr(x, "data") == "mbo" & is.null(varnames)) {
        varnames <- c(val.mil, list(
            "warfarin.datetime" = "`Date and Time - Performed`",
            "warfarin.event" = val.ce,
            "warfarin.result" = val.res
        ))
    }

    # if extra var names are given, append those to the list
    if (!is.null(extras)) {
        varnames <- c(varnames, extras)
    }

    df <- rename_(.data = x, .dots = varnames) %>%
        dplyr::distinct_() %>%
        purrrlyr::dmap_at("warfarin.event", stringr::str_to_lower) %>%
        format_dates("warfarin.datetime")

    after <- match("warfarin", class(x), nomatch = 0L)
    class(df) <- append(class(x), "warfarin", after = after)
    df
}

# class test functions ---------------------------------

#' Test edwr-related classes
#'
#' Takes an R object and checks for a tbl_edwr class type.
#'
#' @param x object which may have a tbl_edwr class type
#' @keywords internal
is.tbl_edwr <- function(x) inherits(x, "tbl_edwr")

#' @rdname is.tbl_edwr
#' @export
is.admit <- function(x) inherits(x, "admit")

#' @rdname is.tbl_edwr
#' @export
is.blood <- function(x) inherits(x, "blood")

#' @rdname is.tbl_edwr
#' @export
is.charges <- function(x) inherits(x, "charges")

#' @rdname is.tbl_edwr
#' @export
is.demographics <- function(x) inherits(x, "demographics")

#' @rdname is.tbl_edwr
#' @export
is.diagnosis <- function(x) inherits(x, "diagnosis")

#' @rdname is.tbl_edwr
#' @export
is.drg <- function(x) inherits(x, "drg")

#' @rdname is.tbl_edwr
#' @export
is.encounters <- function(x) inherits(x, "encounters")

#' @rdname is.tbl_edwr
#' @export
is.events <- function(x) inherits(x, "events")

#' @rdname is.tbl_edwr
#' @export
is.icu_assess <- function(x) inherits(x, "icu_assess")

#' @rdname is.tbl_edwr
#' @export
is.id <- function(x) inherits(x, "id")

#' @rdname is.tbl_edwr
#' @export
is.labs <- function(x) inherits(x, "labs")

#' @rdname is.tbl_edwr
#' @export
is.locations <- function(x) inherits(x, "locations")

#' @rdname is.tbl_edwr
#' @export
is.measures <- function(x) inherits(x, "measures")

#' @rdname is.tbl_edwr
#' @export
is.meds_cont <- function(x) inherits(x, "meds_cont")

#' @rdname is.tbl_edwr
#' @export
is.meds_freq <- function(x) inherits(x, "meds_freq")

#' @rdname is.tbl_edwr
#' @export
is.meds_home <- function(x) inherits(x, "meds_home")

#' @rdname is.tbl_edwr
#' @export
is.meds_inpt <- function(x) inherits(x, "meds_inpt")

#' @rdname is.tbl_edwr
#' @export
is.meds_sched <- function(x) inherits(x, "meds_sched")

#' @rdname is.tbl_edwr
#' @export
is.mpp <- function(x) inherits(x, "mpp")

#' @rdname is.tbl_edwr
#' @export
is.mrn <- function(x) inherits(x, "mrn")

#' @rdname is.tbl_edwr
#' @export
is.order_action <- function(x) inherits(x, "order_action")

#' @rdname is.tbl_edwr
#' @export
is.order_by <- function(x) inherits(x, "order_by")

#' @rdname is.tbl_edwr
#' @export
is.order_detail <- function(x) inherits(x, "order_detail")

#' @rdname is.tbl_edwr
#' @export
is.order_info <- function(x) inherits(x, "order_info")

#' @rdname is.tbl_edwr
#' @export
is.order_timing <- function(x) inherits(x, "order_timing")

#' @rdname is.tbl_edwr
#' @export
is.output <- function(x) inherits(x, "output")

#' @rdname is.tbl_edwr
#' @export
is.pain_scores <- function(x) inherits(x, "pain_scores")

#' @rdname is.tbl_edwr
#' @export
is.patients <- function(x) inherits(x, "patients")

#' @rdname is.tbl_edwr
#' @export
is.problems <- function(x) inherits(x, "problems")

#' @rdname is.tbl_edwr
#' @export
is.procedures <- function(x) inherits(x, "procedures")

#' @rdname is.tbl_edwr
#' @export
is.radiology <- function(x) inherits(x, "radiology")

#' @rdname is.tbl_edwr
#' @export
is.services <- function(x) inherits(x, "services")

#' @rdname is.tbl_edwr
#' @export
is.surgery_times <- function(x) inherits(x, "surgery_times")

#' @rdname is.tbl_edwr
#' @export
is.surgeries <- function(x) inherits(x, "surgeries")

#' @rdname is.tbl_edwr
#' @export
is.uop <- function(x) inherits(x, "uop")

#' @rdname is.tbl_edwr
#' @export
is.vent_settings <- function(x) inherits(x, "vent_settings")

#' @rdname is.tbl_edwr
#' @export
is.vent_times <- function(x) inherits(x, "vent_times")

#' @rdname is.tbl_edwr
#' @export
is.visits <- function(x) inherits(x, "visits")

#' @rdname is.tbl_edwr
#' @export
is.vitals <- function(x) inherits(x, "vitals")

#' @rdname is.tbl_edwr
#' @export
is.warfarin <- function(x) inherits(x, "warfarin")
