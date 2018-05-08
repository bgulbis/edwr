# class.R
#
# get and set class types used in edwr
#

# default values ---------------------------------------
edw_id <- list("pie.id" = "PowerInsight Encounter Id")
mbo_id <- list("millennium.id" = "Encounter Identifier")
event_dt <- "Clinical Event End Date/Time"
event_nm <- "Clinical Event"
event_val <- "Clinical Event Result"

#' Assign edwr class type
#'
#' @param df new data_frame
#' @param x initial data_frame
#' @param new_class string with new class name
#'
#' @return data frame
#'
#' @keywords internal
assign_class <- function(df, x, new_class) {
    after <- match(new_class, class(x), nomatch = 0L)
    class(df) <- append(class(x), new_class, after = after)
    df
}

#' Assign desired names to columns
#'
#' @param x data frame
#' @param varnames named list of standard column names for given class
#' @param extras optional named list of additional column names
#'
#' @return data frame
#'
#' @keywords internal
assign_names <- function(x, varnames, extras = NULL) {
    # if extra var names are given, append those to the list
    if (!is.null(extras)) {
        varnames <- c(varnames, extras)
    }

    x %>%
        rename(!!!varnames) %>%
        distinct()
}

# constructor functions --------------------------------

#' Construct edwr data types
#'
#' Takes an R object and sets class to an edwr type.
#'
#' @param x object to set class \code{tbl_edwr}
#' @param order_var logical indicating if order.id fields are included, defaults
#'   to TRUE
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

    assign_class(x, x, "tbl_edwr")
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
as.admit <- function(x, extras = NULL) {
    if (missing(x)) stop("Missing object")
    if (is.admit(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw") {
        varnames <- c(edw_id, list(
            "visit.type" = "Encounter Type",
            "visit.type.class" = "Encounter Type Class",
            "admit.type" = "Admit Type",
            "admit.source" = "Admit Source"
        ))

        # default CDW/MBO names
    } else {
        varnames <- c(mbo_id, list(
            "visit.type" = "Encounter Class Subtype"
        ))
    }

    x %>%
        assign_names(varnames, extras) %>%
        assign_class(x, "admit")
}


#' @rdname set_edwr_class
#' @export
as.blood <- function(x, extras = NULL) {
    # inherits from events class
    if (missing(x)) x <- character()
    if (is.blood(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw") {
        if (!is.events(x)) x <- as.events(x)
        varnames <- c(edw_id, list(
            "blood.datetime" = "event.datetime",
            "blood.prod" = "event",
            "blood.type" = "event.result"
        ))

        # default CDW/MBO names
    } else {
        varnames <- c(mbo_id, list(
            "blood.datetime" = "Date and Time - Performed",
            "blood.prod" = "Clinical Event",
            "blood.type" = "Clinical Event Result",
            "blood.location" = "Nurse Unit (Event)",
            "event.id" = "Event Id",
            "event.parent.id" = "Parent Event Id",
            "order.id" = "Order Id",
            "order.parent.id" = "Parent Order Id"
        ))
    }

    prods <- c("Cryo(.*)" = "cryo",
               "FFP(.*)" = "ffp",
               "(P)?RBC(.*)" = "prbc",
               "Platelet(.*)" = "platelet")

    x %>%
        assign_names(varnames, extras) %>%
        dplyr::mutate_at("blood.prod",
                         stringr::str_replace_all,
                         pattern = prods) %>%
        format_dates("blood.datetime") %>%
        assign_class(x, "blood")
}

#' @rdname set_edwr_class
#' @export
as.charges <- function(x, extras = NULL) {
    if (missing(x)) stop("Missing object")
    if (is.charges(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw") {
        varnames <- c(edw_id, list(
            "cdm.code" = "Cdm Code",
            "service.date" = "Service Date",
            "institution" = "Institution Desc"
        ))
        # default CDW/MBO names
    }

    x %>%
        assign_names(varnames, extras) %>%
        assign_class(x, "charges")
}

#' @rdname set_edwr_class
#' @export
as.demographics <- function(x, extras = NULL) {
    if (missing(x)) stop("Missing object")
    if (is.demographics(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw") {
        varnames <- c(edw_id, list(
            "age" = "Age- Years (Visit)",
            "gender" = "Sex",
            "race" = "Race",
            "disposition" = "Discharge Disposition",
            "length.stay" = "LOS (Actual)",
            "visit.type" = "Encounter Type",
            "person.id" = "Person ID",
            "facility" = "Person Location- Facility (Curr)"
        ))
    # default CDW/MBO names
    } else {
        varnames <- c(mbo_id, list(
            "age" = "Age- Years (At Admit)",
            "gender" = "Gender",
            "race" = "Race",
            "disposition" = "Discharge Disposition",
            "length.stay" = "LOS (Curr)",
            "visit.type" = "Encounter Class Subtype",
            "facility" = "Facility (Curr)"
        ))
    }

    x %>%
        assign_names(varnames, extras) %>%
        dplyr::mutate_at(c("age", "length.stay"), as.numeric) %>%
        assign_class(x, "demographics")
}

#' @rdname set_edwr_class
#' @export
as.diagnosis <- function(x, extras = NULL) {
    if (missing(x)) stop("Missing object")
    if (is.diagnosis(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw") {
        varnames <- c(edw_id, list(
            "diag.code" = "Diagnosis Code",
            "code.source" = "Diagnosis Code Source Vocabulary",
            "diag.type" = "Diagnosis Type",
            "diag.seq" = "Diagnosis Code Sequence",
            "present.admit" = "Present on Admission"
        ))

    # default CDW/MBO names
    } else {
        varnames <- c(mbo_id, list(
            "diag.code" = "Diagnosis Code",
            "code.source" = "Diagnosis Code Source Vocabulary",
            "diag.type" = "Diagnosis Type",
            "diag.seq" = "Diagnosis Code (Primary VS Non Primary)"
        ))
    }

    x %>%
        assign_names(varnames, extras) %>%
        assign_class(x, "diagnosis")
}

#' @rdname set_edwr_class
#' @export
as.drg <- function(x, extras = NULL) {
    if (missing(x)) stop("Missing object")
    if (is.drg(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw") {
        varnames <- c(edw_id, list(
            "drg" = "Grouping Code",
            "drg.source" = "Grouping Code Source Vocabulary",
            "drg.type" = "Grouping Code Type"
        ))
        # default CDW/MBO names
    } else {
        varnames <- c(mbo_id, list(
            "drg" = "DRG Code",
            "drg.desc" = "DRG Description",
            "drg.priority" = "DRG Priority"
        ))
    }

    x %>%
        assign_names(varnames, extras) %>%
        assign_class(x, "drg")
}

#' @rdname set_edwr_class
#' @export
as.encounters <- function(x, extras = NULL) {
    if (missing(x)) stop("Missing object")
    if (is.encounters(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw") {
        varnames <- c(edw_id, list(
            "millennium.id" = "Millennium Encounter ID",
            "person.id" = "Person ID",
            "admit.datetime" = "Admit Date & Time",
            "visit.type" = "Encounter Type",
            "facility" = "Person Location- Facility (Curr)",
            "disposition" = "Discharge Disposition"
        ))

        # default CDW/MBO names
    } else {
        varnames <- c(mbo_id, list(
            "admit.datetime" = "Date and Time - Admit",
            "visit.type" = "Encounter Class Subtype",
            "facility" = "Facility (Curr)",
            "disposition" = "Discharge Disposition"
        ))
    }

    x %>%
        assign_names(varnames, extras) %>%
        format_dates("admit.datetime") %>%
        assign_class(x, "encounters")
}

#' @rdname set_edwr_class
#' @export
as.events <- function(x, extras = NULL, order_var = TRUE) {
    if (missing(x)) x <- character()
    if (is.events(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw") {
        varnames <- c(edw_id, list(
            "event.datetime" = event_dt,
            "event" = event_nm,
            "event.result" = event_val,
            "event.location" = "Nurse Unit of Clinical Event",
            "order.id" = "Clinical Event Order ID"
        ))

        # default CDW/MBO names
    } else {
        if (sum(stringr::str_detect(names(x), "Date and Time - Performed")) >= 1) {
            dt <- "Date and Time - Performed"
        } else {
            dt <- "Date and Time - Scheduled OR Given On"
        }

        varnames <- c(mbo_id, list(
            "event.datetime" = dt,
            "event" = event_nm,
            "event.result" = event_val,
            "event.result.units" = "Clinical Event Result Units",
            "event.location" = "Nurse Unit (Event)",
            "event.id" = "Event Id",
            "event.parent.id" = "Parent Event Id"
        ))
    }

    if (order_var) {
        varnames <- c(varnames, list(
            "order.id" = "Order Id",
            "order.parent.id" = "Parent Order Id"
        ))
    }

    x %>%
        assign_names(varnames, extras) %>%
        dplyr::mutate_at("event", stringr::str_to_lower) %>%
        format_dates("event.datetime") %>%
        assign_class(x, "events")
}

#' @rdname set_edwr_class
#' @export
as.icu_assess <- function(x, extras = NULL) {
    # inherits from events class
    if (missing(x)) x <- character()
    if (is.icu_assess(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)
    if (!is.events(x)) x <- as.events(x, extras)

    x %>%
        rename(!!!list(
            "assess.datetime" = "event.datetime",
            "assessment" = "event",
            "assess.result" = "event.result"
        )) %>%
        assign_class(x, "icu_assess")
}

#' @rdname set_edwr_class
#' @export
as.id <- function(x, extras = NULL) {
    if (missing(x)) stop("Missing object")
    if (is.id(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw") {
        varnames <- c(edw_id, list(
            "millennium.id" = "Millennium Encounter ID",
            "fin" = "Formatted Financial Nbr",
            "person.id" = "Person ID"
        ))

        # default CDW/MBO names
    } else {
        varnames <- c(mbo_id, list(
            "fin" = "Financial Number"
        ))
    }

    x %>%
        assign_names(varnames, extras) %>%
        assign_class(x, "id")
}

#' @rdname set_edwr_class
#' @export
as.labs <- function(x, extras = NULL) {
    # inherits from events class
    if (missing(x)) x <- character()
    if (is.labs(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw") {
        # if (!is.events(x)) x <- as.events(x)
        varnames <- c(edw_id, list(
            "lab.datetime" = event_dt,
            "lab" = event_nm,
            "lab.result" = event_val
        ))
    # default CDW/MBO names
    } else {
        varnames <- c(mbo_id, list(
            "lab.datetime" = "Date and Time - Nurse Draw",
            "lab" = "Lab Event (FILTER ON)",
            "lab.result" = "Lab Result",
            "lab.result.units" = "Lab Result Units",
            "lab.draw.location" = "Nurse Unit (Lab)",
            "lab.id" = "Lab Event Id"
        ))
    }

    x %>%
        assign_names(varnames, extras) %>%
        dplyr::mutate_at("lab", stringr::str_to_lower) %>%
        format_dates("lab.datetime") %>%
        assign_class(x, "labs")
}

#' @rdname set_edwr_class
#' @export
as.locations <- function(x, extras = NULL) {
    if (missing(x)) x <- character()
    if (is.locations(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw") {
        varnames <- c(edw_id, list(
            "arrive.datetime" = "Location Arrival Date & Time",
            "depart.datetime" = "Location Depart Date & Time",
            "unit.to" = "Person Location - Nurse Unit (To)",
            "unit.from" = "Person Location - Nurse Unit (From)"
        ))

        # default CDW/MBO names
    } else {
        varnames <- c(mbo_id, list(
            "arrive.datetime" = "Date and Time - Nurse Unit Begin",
            "depart.datetime" = "Date and Time - Nurse Unit End",
            "unit.name" = "Nurse Unit All"
        ))
    }

    x %>%
        assign_names(varnames, extras) %>%
        format_dates(c("arrive.datetime", "depart.datetime")) %>%
        assign_class(x, "locations")
}

#' @rdname set_edwr_class
#' @export
as.measures <- function(x, extras = NULL) {
    # inherits from events class
    if (missing(x)) x <- character()
    if (is.measures(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw") {
        if (!is.events(x)) x <- as.events(x)
        varnames <- c(edw_id, list(
            "measure.datetime" = "event.datetime",
            "measure" = "event",
            "measure.result" = "event.result",
            "measure.units" = "Clinical Event Result Units"
        ))

        # default CDW/MBO names
    } else {
        if (sum(stringr::str_detect(names(x), "Date and Time - Performed")) >= 1) {
            dt <- "Date and Time - Performed"
        } else {
            dt <- "Date and Time - Scheduled OR Given On"
        }

        varnames <- c(mbo_id, list(
            "measure.datetime" = dt,
            "measure" = "Clinical Event",
            "measure.result" = "Clinical Event Result",
            "measure.units" = "Clinical Event Result Units"
        ))
    }

    x %>%
        assign_names(varnames, extras) %>%
        dplyr::mutate_at("measure", stringr::str_to_lower) %>%
        dplyr::mutate_at("measure.result", as.numeric) %>%
        format_dates("measure.datetime") %>%
        assign_class(x, "measures")
}

#' @rdname set_edwr_class
#' @export
as.meds_admin <- function(x, extras = NULL) {
    if (missing(x)) x <- character()
    if (is.meds_admin(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw") {
        varnames <- c(edw_id, list(
            "order.id" = "Clinical Event Order ID",
            "event.id" = "Event ID",
            "med.datetime" = event_dt,
            "med" = event_nm
        ))

        # default CDW/MBO names
    } else {
        varnames <- c(mbo_id, list(
            "med" = "Medication (Generic)",
            "scheduled_datetime" = "Date and Time - Scheduled",
            "admin_datetime" = "Date and Time - Administration",
            "admin_end_datetime" = "Date and Time - Administration End",
            "document_source" = "Med Documentation Source",
            "scan_patient" = "Scanned Armband (PPID)",
            "scan_med" = "Scanned Medication (PMID)",
            "med.location" = "Nurse Unit (Med)",
            "order.id" = "Order Id",
            "order.parent.id" = "Parent Order Id"
        ))
    }

    x %>%
        assign_names(varnames, extras) %>%
        dplyr::mutate_at("med", stringr::str_to_lower) %>%
        format_dates(c("scheduled_datetime",
                       "admin_datetime",
                       "admin_end_datetime")) %>%
        assign_class(x, "meds_admin")
}

#' @rdname set_edwr_class
#' @export
as.meds_cont <- function(x, extras = NULL) {
    if (missing(x)) x <- character()
    if (is.meds_cont(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw") {
        varnames <- c(edw_id, list(
            "order.id" = "Clinical Event Order ID",
            "event.id" = "Event ID",
            "med.datetime" = event_dt,
            "med" = event_nm,
            "med.rate" = "Infusion Rate",
            "med.rate.units" = "Infusion Rate Unit",
            "route" = "Route of Administration - Short",
            "event.tag" = "Event Tag"
        ))
    }

    x %>%
        assign_names(varnames, extras) %>%
        dplyr::mutate_at("med", stringr::str_to_lower) %>%
        dplyr::mutate_at("med.rate.units", dplyr::na_if, y = "") %>%
        dplyr::mutate_at("med.rate", as.numeric) %>%
        format_dates("med.datetime") %>%
        assign_class(x, "meds_cont")
}

#' @rdname set_edwr_class
#' @export
as.meds_freq <- function(x, extras = NULL) {
    # inherits from meds_sched class
    if (missing(x)) x <- character()
    if (is.meds_freq(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)
    if (!is.meds_sched(x)) x <- as.meds_sched(x, extras)

    x %>%
        rename(!!!(list("freq" = "Parent Order Frequency Description"))) %>%
        assign_class(x, "meds_freq")
}

#' @rdname set_edwr_class
#' @export
as.meds_home <- function(x, extras = NULL) {

    fx <- function(df, x) {
        if (attr(x, "data") == "mbo") {
            df <- format_dates(df, "order.datetime")
        }
        df
    }

    if (missing(x)) x <- character()
    if (is.meds_home(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw") {
        varnames <- c(edw_id, list(
            "med" = "Order Catalog Short Description",
            "order.name" = "Order Catalog Mnemonic",
            "med.type" = "Orig Orderable Type-Flag Desc"
        ))

        # default CDW/MBO names
    } else {
        varnames <- c(mbo_id, list(
            "order.datetime" = "Date and Time - Original (Placed)",
            "med" = "Mnemonic (Primary Generic) FILTER ON",
            "med.type" = "Order Type"
        ))
    }

    x %>%
        assign_names(varnames, extras) %>%
        dplyr::mutate_at("med", stringr::str_to_lower) %>%
        fx(x) %>%
        assign_class(x, "meds_home")
}

#' @rdname set_edwr_class
#' @export
as.meds_inpt <- function(x, extras = NULL) {
    if (missing(x)) x <- character()
    if (is.meds_cont(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw") {
        varnames <- c(edw_id, list(
            "order.id" = "Clinical Event Order ID",
            "event.id" = "Event ID",
            "med.datetime" = event_dt,
            "med" = event_nm,
            "med.rate" = "Infusion Rate",
            "med.rate.units" = "Infusion Rate Unit",
            "route" = "Route of Administration - Short",
            "event.tag" = "Event Tag"
        ))

        # default CDW/MBO names
    } else {
        varnames <- c(mbo_id, list(
            "order.id" = "Order Id",
            "order.parent.id" = "Parent Order Id",
            "event.id" = "Med Event Id",
            "med.datetime" = "Date and Time - Administration",
            "med" = "Medication (Generic)",
            "med.dose" = "Admin Dosage",
            "med.dose.units" = "Admin Dosage Unit",
            "med.rate" = "Infusion Rate",
            "med.rate.units" = "Infusion Unit",
            "route" = "Admin Route",
            "event.tag" = "Infusion Actions",
            "med.location" = "Nurse Unit (Med)"
        ))
    }

    x %>%
        assign_names(varnames, extras) %>%
        dplyr::mutate_at("med", stringr::str_to_lower) %>%
        dplyr::mutate_at(c("med.dose", "med.rate"), as.numeric) %>%
        format_dates("med.datetime") %>%
        assign_class(x, "meds_inpt")
}

#' @rdname set_edwr_class
#' @export
as.meds_sched <- function(x, extras = NULL) {
    if (missing(x)) x <- character()
    if (is.meds_sched(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw") {
        varnames <- c(edw_id, list(
            "order.id" = "Clinical Event Order ID",
            "event.id" = "Event ID",
            "med.datetime" = event_dt,
            "med" = event_nm,
            "med.dose" = "Dosage Amount",
            "med.dose.units" = "Dosage Unit",
            "route" = "Route of Administration - Short",
            "event.type" = "Event Type Code"
        ))
    }

    x %>%
        assign_names(varnames, extras) %>%
        dplyr::mutate_at("med", stringr::str_to_lower) %>%
        dplyr::mutate_at("med.dose", as.numeric) %>%
        format_dates("med.datetime") %>%
        assign_class(x, "meds_sched")
}

#' @rdname set_edwr_class
#' @export
as.md <- function(x, extras = NULL) {
    if (missing(x)) stop("Missing object")
    if (is.md(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw") {
        varnames <- c(edw_id, list())
    } else {
        varnames <- c(
            mbo_id,
            list(
                "attending" = "Attending Physician-All",
                "md.start" = "Date and Time - Phys Begin Effective",
                "md.stop" = "Date and Time - Phys End Effective"
            )
        )
    }

    x %>%
        assign_names(varnames, extras) %>%
        format_dates(c("md.start", "md.stop")) %>%
        assign_class(x, "md")
}

#' @rdname set_edwr_class
#' @export
as.mrn <- function(x, extras = NULL) {
    if (missing(x)) stop("Missing object")
    if (is.mrn(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw") {
        varnames <- c(edw_id, list(
            "mrn" = "MRN- Organization",
            "person.id" = "Person ID"
        ))
    }

    x %>%
        assign_names(varnames, extras) %>%
        assign_class(x, "mrn")
}

#' @rdname set_edwr_class
#' @export
as.mpp <- function(x, extras = NULL) {
    if (missing(x)) stop("Missing object")
    if (is.mpp(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw") {
        varnames <- c(edw_id, list(
            "mpp" = "MPP (which generated order)"
        ))
    }

    x %>%
        assign_names(varnames, extras) %>%
        assign_class(x, "mpp")
}

#' @rdname set_edwr_class
#' @export
as.order_action <- function(x, extras = NULL) {
    if (missing(x)) x <- character()
    if (is.order_action(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw") {
        varnames <- c(edw_id, list(
            "order.id" = "Source Order ID",
            "action.datetime" = "Order Action Date & Time",
            "provider" = "Action Provider",
            "provider.role" = "Action Provider Position",
            "action.type" = "Action Type",
            "action.provider" = "Action Personnel",
            "action.provider.role" = "Action Personnel Position",
            "action.comm" = "Action Communication Type",
            "order.status" = "Order Department Status - Generic"
        ))

        # default CDW/MBO names
    } else {
        varnames <- c(mbo_id, list(
            "order.id" = "Order Id",
            "order.parent.id" = "Parent Order Id",
            "action.datetime" = "Date and Time - Order Action",
            "order" = "Mnemonic (Primary Generic) FILTER ON",
            "order.location" = "Nurse Unit (Order)",
            "action.type" = "Order Action Type",
            "action.provider" = "Order Action Personnel",
            "action.provider.role" = "Order Action Personnel Position",
            "action.comm" = "Order Communication Type",
            "order.status" = "Order Status (Curr)"
        ))
    }

    x %>%
        assign_names(varnames, extras) %>%
        format_dates("action.datetime") %>%
        assign_class(x, "order_action")
}


#' @rdname set_edwr_class
#' @export
as.order_by <- function(x, extras = NULL) {
    if (missing(x)) x <- character()
    if (is.order_by(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw") {
        varnames <- list(
            "order" = "Order Catalog Mnemonic",
            "order.unit" = "Person Location- Nurse Unit (Order)",
            "provider" = "Action Provider",
            "provider.role" = "Action Provider Position",
            "action.datetime" = "Order Action Date & Time",
            "action.type" = "Action Type",
            "action.provider" = "Action Personnel",
            "action.provider.role" = "Action Personnel Position",
            "action.comm" = "Action Communication Type"
        )
    }

    if ("Source Order ID" %in% names(x)) {
        varnames <- c(list("order.id" = "Source Order ID"), varnames)
    }

    if ("Order Department Status - Generic" %in% names(x)) {
        varnames <- c(varnames, list("order.status" = "Order Department Status - Generic"))
    }

    x %>%
        assign_names(varnames, extras) %>%
        format_dates("action.datetime") %>%
        assign_class(x, "order_by")
}

#' @rdname set_edwr_class
#' @export
as.order_detail <- function(x, extras = NULL) {
    if (missing(x)) x <- character()
    if (is.order_detail(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw") {
        varnames <- c(edw_id, list(
            "order.id" = "Source Order ID",
            "order" = "Order Catalog Mnemonic",
            "ingredient" = "Ingredient Catalog Short Desc",
            "ingredient.dose" = "Dose Strength - Order",
            "ingredient.unit" = "Dose Strength - Unit",
            "ingredient.freetext" = "Dose Freetext",
            "order.unit" = "Person Location- Nurse Unit (Order)",
            "action.type" = "Action Type",
            "action.datetime" = "Order Action Date & Time"
        ))

        dt_name <- "action.datetime"

        # default CDW/MBO names
    } else {
        varnames <- c(mbo_id, list(
            "order.id" = "Order Id",
            "order.datetime" = "Date and Time - Original (Placed)",
            "order" = "Mnemonic (Primary Generic) FILTER ON",
            "ingredient.dose" = "Order Strength Dose",
            "ingredient.unit" = "Order Strength Dose Unit",
            "route" = "Order Route",
            "freq" = "Frequency",
            "prn" = "PRN Indicator",
            "order.provider" = "Ordering Provider LIMITS",
            "order.provider.position" = "Ordering Provider Position LIMITS"
        ))

        dt_name <- "order.datetime"
    }

    x %>%
        assign_names(varnames, extras) %>%
        format_dates(dt_name) %>%
        assign_class(x, "order_detail")
}

#' @rdname set_edwr_class
#' @export
as.order_info <- function(x, extras = NULL) {
    if (missing(x)) x <- character()
    if (is.order_info(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw") {
        varnames <- c(edw_id, list(
            "order.id" = "Source Order ID",
            "detail.datetime" = "Detail Date & Time",
            "detail" = "Detail Display Value",
            "detail.descr" = "Field Description"
        ))
    }

    x %>%
        assign_names(varnames, extras) %>%
        format_dates("detail.datetime") %>%
        assign_class(x, "order_info")
}

#' @rdname set_edwr_class
#' @export
as.order_timing <- function(x, extras = NULL) {
    if (missing(x)) x <- character()
    if (is.order_timing(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw") {
        varnames <- list(
            "order.id" = "Source Order ID",
            "order" = "Order Catalog Mnemonic",
            "order.unit" = "Person Location- Nurse Unit (Order)",
            "order.datetime" = "Order Date & Time",
            "request.datetime" = "Order Request Date & Time",
            "complete.datetime" = "Order Complete Date & Time",
            "complete.person" = "Complete Personnel",
            "discontinue.datetime" = "Order Discontinue Date & Time",
            "cancel.datetime" = "Order Cancel Date & Time",
            "cancel.person" = "Order Personnel - Cancel",
            "cancel.reason" = "Order Cancel Reason"
        )
    }

    x %>%
        assign_names(varnames, extras) %>%
        format_dates(c("order.datetime",
                       "request.datetime",
                       "complete.datetime",
                       "discontinue.datetime",
                       "cancel.datetime")) %>%
        assign_class(x, "order_timing")
}

#' @rdname set_edwr_class
#' @export
as.order_verify <- function(x, extras = NULL) {
    if (missing(x)) x <- character()
    if (is.order_verify(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw") {
        varnames <- c(edw_id, list(
            "order.id" = "Clinical Event Order ID",
            "event.id" = "Event ID",
            "med.datetime" = event_dt,
            "med" = event_nm
        ))

        # default CDW/MBO names
    } else {
        varnames <- c(mbo_id, list(
            "med" = "Mnemonic (Primary Generic) FILTER ON",
            "order.datetime" = "Date and Time - Original (Placed)",
            "start.datetime" = "Date and Time - Order Start",
            "verify.datetime" = "Date and Time - Pharmacist Review",
            "action.type" = "Order Action Type",
            "order.location" = "Nurse Unit (Order)",
            "order.id" = "Order Id",
            "order.parent.id" = "Parent Order Id"
        ))
    }

    x %>%
        assign_names(varnames, extras) %>%
        dplyr::mutate_at("med", stringr::str_to_lower) %>%
        format_dates(c("order.datetime", "start.datetime", "verify.datetime")) %>%
        assign_class(x, "order_verify")
}

#' @rdname set_edwr_class
#' @export
as.output <- function(x, extras = NULL) {
    # inherits from events class
    if (missing(x)) x <- character()
    if (is.output(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw") {
        varnames <- c(edw_id, list(
            "output.datetime" = event_dt,
            "output" = event_nm,
            "output.result" = event_val
        ))

        # default CDW/MBO names
    } else {
        varnames <- c(mbo_id, list(
            "output.datetime" = "Date and Time - Performed",
            "output" = event_nm,
            "output.result" = event_val,
            "output.result.units" = "Clinical Event Result Units",
            "output.location" = "Nurse Unit (Event)",
            "event.id" = "Event Id",
            "event.parent.id" = "Parent Event Id"
        ))
    }

    x %>%
        assign_names(varnames, extras) %>%
        dplyr::mutate_at("output.result", stringr::str_to_lower) %>%
        format_dates("output.datetime") %>%
        assign_class(x, "output")
}

#' @rdname set_edwr_class
#' @export
as.pain_scores <- function(x, extras = NULL) {
    if (missing(x)) x <- character()
    if (is.events(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw") {
        varnames <- c(edw_id, list(
            "event.datetime" = event_dt,
            "event" = event_nm,
            "event.result" = event_val,
            "event.location" = "Nurse Unit of Clinical Event",
            "order.id" = "Clinical Event Order ID"
        ))

        # default CDW/MBO names
    } else {
        varnames <- c(mbo_id, list(
            "event.datetime" = "Date and Time - Scheduled OR Given On",
            "event" = event_nm,
            "event.result" = event_val,
            "event.result.units" = "Clinical Event Result Units",
            "event.location" = "Nurse Unit (Event)",
            "event.id" = "Event Id",
            "event.parent.id" = "Parent Event Id",
            "order.id" = "Clinical Event Order Id"
        ))
    }

    x %>%
        assign_names(varnames, extras) %>%
        dplyr::mutate_at("event", stringr::str_to_lower) %>%
        format_dates("event.datetime") %>%
        assign_class(x, "events")
}

#' @rdname set_edwr_class
#' @export
as.patients <- function(x, extras = NULL) {
    if (missing(x)) stop("Missing object")
    if (is.patients(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw") {
        varnames <- c(edw_id, list(
            "age" = "Age- Years (Visit)",
            "discharge.datetime" = "Discharge Date & Time",
            "visit.type" = "Encounter Type",
            "facility" = "Person Location- Facility (Curr)"
        ))
    # default CDW/MBO names
    } else {
        varnames <- c(mbo_id, list(
            "age" = "Age- Years (At Admit)",
            "discharge.datetime" = "Date and Time - Discharge",
            "visit.type" = "Encounter Class Subtype",
            "facility" = "Facility (Curr)"
        ))
    }

    x %>%
        assign_names(varnames, extras) %>%
        dplyr::mutate_at("age", as.numeric) %>%
        format_dates("discharge.datetime") %>%
        assign_class(x, "patients")
}

#' @rdname set_edwr_class
#' @export
as.problems <- function(x, extras = NULL) {
    if (missing(x)) stop("Missing object")
    if (is.problems(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw") {
        varnames <- c(edw_id, list(
            "problem" = "Problem - Description",
            "classification" = "Problem Classification",
            "confirm" = "Problem Confirmation Status",
            "free.text" = "Problem Free Text",
            "severity" = "Problem Severity",
            "active" = "Problem Source Active Indicator",
            "onset.datetime" = "Problem Onset Date & Time",
            "life.cycle.datetime" = "Problem Life Cycle Date & Time",
            "life.cycle" = "Problem Life Cycle"
        ))
        # default CDW/MBO names
    }

    x %>%
        assign_names(varnames, extras) %>%
        format_dates(c("onset.datetime", "life.cycle.datetime")) %>%
        assign_class(x, "problems")
}

#' @rdname set_edwr_class
#' @export
as.procedures <- function(x, extras = NULL) {
    if (missing(x)) stop("Missing object")
    if (is.procedures(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw") {
        varnames <- c(edw_id, list(
            "proc.date" = "Procedure Date and Time",
            "proc.code" = "Procedure Code",
            "proc.source" = "Procedure Code Source Vocabulary",
            "proc.seq" = "Procedure Code Sequence"
        ))

        # default CDW/MBO names
    } else {
        varnames <- c(mbo_id, list(
            "proc.date" = "Procedure Date and Time",
            "proc.code" = "Procedure Code",
            "proc.source" = "Diagnosis Code Source Vocabulary",
            "proc.seq" = "Procedure Code Sequence"
        ))
    }

    x %>%
        assign_names(varnames, extras) %>%
        format_dates("proc.date") %>%
        assign_class(x, "procedures")
}

#' @rdname set_edwr_class
#' @export
as.radiology <- function(x, extras = NULL) {
    if (missing(x)) stop("Missing object")
    if (is.radiology(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw") {
        varnames <- c(edw_id, list(
            "rad.datetime" = event_dt,
            "rad.type" = event_nm
        ))
        # default CDW/MBO names
    }

    x %>%
        assign_names(varnames, extras) %>%
        format_dates("rad.datetime") %>%
        assign_class(x, "radiology")
}

#' @rdname set_edwr_class
#' @export
as.services <- function(x, extras = NULL) {
    if (missing(x)) x <- character()
    if (is.services(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw") {
        varnames <- c(edw_id, list(
            "start.datetime" = "Medical Service Begin Date & Time",
            "end.datetime" = "Medical Service End Date & Time",
            "service" = "Medical Service"
            # "service.from" = "Previous Medical Service"
        ))
        # default CDW/MBO names
    }

    x %>%
        assign_names(varnames, extras) %>%
        dplyr::mutate_at("service", dplyr::na_if, y = "") %>%
        format_dates(c("start.datetime", "end.datetime")) %>%
        assign_class(x, "services")
}

#' @rdname set_edwr_class
#' @export
as.surgery_times <- function(x, extras = NULL) {
    if (missing(x)) x <- character()
    if (is.surgery_times(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw") {
        varnames <- c(
            edw_id,
            list(
                "surgery_start" = "Start Date/Time",
                "surgery_stop" = "Stop Date/Time",
                "room_in" = "Patient In Room Date/Time",
                "room_out" = "Patient Out Room Date/Time",
                "recovery_in" = "Patient In Recovery Date/Time",
                "recovery_out" = "Patient Out Recovery Date/Time"
            )
        )
    }

    x %>%
        assign_names(varnames, extras) %>%
        format_dates(
            c(
                "surgery_start",
                "surgery_stop",
                "room_in",
                "room_out",
                "recovery_in",
                "recovery_out"
            )
        ) %>%
        assign_class(x, "surgery_times")
}

#' @rdname set_edwr_class
#' @export
as.surgeries <- function(x, extras = NULL) {
    if (missing(x)) stop("Missing object")
    if (is.surgeries(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw") {
        varnames <- c(
            edw_id,
            list(
                "asa.class" = "ASA Class Description",
                "add.on" = "Add On Indicator",
                "surgery" = "Procedure",
                "primary.proc" = "Primary Procedure Indicator",
                "surg.start.datetime" = "Start Date/Time",
                "surg.stop.datetime" = "Stop Date/Time",
                "surg.type" = "Surgical Case Specialty"
            )
        )
    }

    x %>%
        assign_names(varnames, extras) %>%
        dplyr::mutate_at(
            c("add.on", "primary.proc"),
            dplyr::funs(!!parse_expr(". == 1"))
        ) %>%
        format_dates(c("surg.start.datetime", "surg.stop.datetime")) %>%
        assign_class(x, "surgeries")
}

#' @rdname set_edwr_class
#' @export
as.uop <- function(x, extras = NULL) {
    # inherits from events class
    if (missing(x)) x <- character()
    if (is.uop(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)
    if (!is.events(x)) x <- as.events(x, extras)

    # default EDW names
    if (attr(x, "data") == "edw") {
        varnames <- list(
            "uop.datetime" = "event.datetime",
            "uop" = "event",
            "uop.result" = "event.result"
        )
    }

    x %>%
        rename(!!!varnames) %>%
        assign_class(x, "uop")
}

#' @rdname set_edwr_class
#' @export
as.vent_settings <- function(x, extras = NULL) {
    # inherits from events class
    if (missing(x)) x <- character()
    if (is.vent_settings(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)
    if (!is.events(x)) x <- as.events(x, extras)

    # default EDW names
    if (attr(x, "data") == "edw") {
        varnames <- list(
            "vent.datetime" = "event.datetime",
            "vent.event" = "event",
            "vent.result" = "event.result"
        )
    }

    x %>%
        rename(!!!varnames) %>%
        assign_class(x, "vent_settings")
}

#' @rdname set_edwr_class
#' @export
as.vent_times <- function(x, extras = NULL) {
    if (missing(x)) x <- character()
    if (is.vent_times(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw") {
        varnames <- c(edw_id, list(
            "vent.datetime" = "Clinical Event Date Result",
            "vent.event" = event_nm
        ))

        # default CDW/MBO names
    } else {
        varnames <- c(mbo_id, list(
            "vent.datetime" = "Date and Time - Performed",
            "vent.event" = event_nm,
            "vent.location" = "Nurse Unit (Event)",
            "event.id" = "Event Id",
            "event.parent.id" = "Parent Event Id",
            "order.id" = "Order Id",
            "order.parent.id" = "Parent Order Id"
        ))
    }

    x %>%
        assign_names(varnames, extras) %>%
        dplyr::mutate_at("vent.event", stringr::str_to_lower) %>%
        format_dates("vent.datetime") %>%
        assign_class(x, "vent_times")
}

#' @rdname set_edwr_class
#' @export
as.visits <- function(x, extras = NULL) {
    if (missing(x)) x <- character()
    if (is.visits(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw") {
        varnames <- c(edw_id, list(
            "arrival.datetime" = "Arrival Date & Time",
            "admit.datetime" = "Admit Date & Time",
            "discharge.datetime" = "Discharge Date & Time",
            "visit.type" = "Encounter Type",
            "admit.source" = "Admit Source",
            "admit.type" = "Admit Type",
            "facility" = "Person Location- Facility (Curr)",
            "nurse.unit.admit" = "Person Location- Nurse Unit (Admit)"
        ))

        # default CDW/MBO names
    } else {
        varnames <- c(mbo_id, list(
            "arrival.datetime" = "Date and Time - Arrival",
            "admit.datetime" = "Date and Time - Admit",
            "discharge.datetime" = "Date and Time - Discharge",
            "visit.type" = "Encounter Class Type",
            "visit.subtype" = "Encounter Class Subtype",
            "admit.source" = "Admit Source",
            "admit.type" = "Admit Type",
            "facility" = "Facility (Curr)",
            "nurse.unit" = "Nurse Unit (Curr)",
            "nurse.unit.admit" = "Nurse Unit (Inpatient Admit) LIMITS"
        ))
    }

    x %>%
        assign_names(varnames, extras) %>%
        format_dates(c("arrival.datetime", "admit.datetime", "discharge.datetime")) %>%
        assign_class(x, "visits")
}

#' @rdname set_edwr_class
#' @export
as.vitals <- function(x, extras = NULL) {
    # inherits from events class
    if (missing(x)) x <- character()
    if (is.vitals(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw") {
        varnames <- c(edw_id, list(
            "vital.datetime" = event_dt,
            "vital" = event_nm,
            "vital.result" = event_val,
            "vital.result.units" = "Clinical Event Result Units"
        ))

        # default CDW/MBO names
    } else {
        if (sum(stringr::str_detect(names(x), "Date and Time - Performed")) >= 1) {
            dt <- "Date and Time - Performed"
        } else {
            dt <- "Date and Time - Scheduled OR Given On"
        }

        varnames <- c(mbo_id, list(
            "vital.datetime" = dt,
            "vital" = "Clinical Event",
            "vital.result" = "Clinical Event Result",
            "vital.result.units" = "Clinical Event Result Units",
            "vital.location" = "Nurse Unit (Event)",
            "vital.id" = "Event Id",
            "vital.parent.id" = "Parent Event Id"
        ))
    }

    x %>%
        assign_names(varnames, extras) %>%
        dplyr::mutate_at("vital", stringr::str_to_lower) %>%
        dplyr::mutate_at("vital.result", as.numeric) %>%
        format_dates("vital.datetime") %>%
        assign_class(x, "vitals")
}

#' @rdname set_edwr_class
#' @export
as.warfarin <- function(x, extras = NULL) {
    # inherits from events class
    if (missing(x)) x <- character()
    if (is.warfarin(x)) return(x)
    if (!is.tbl_edwr(x)) x <- as.tbl_edwr(x)

    # default EDW names
    if (attr(x, "data") == "edw") {
        varnames <- c(edw_id, list(
            "warfarin.datetime" = event_dt,
            "warfarin.event" = event_nm,
            "warfarin.result" = event_val
        ))

        # default CDW/MBO names
    } else {
        varnames <- c(mbo_id, list(
            "warfarin.datetime" = "Date and Time - Performed",
            "warfarin.event" = event_nm,
            "warfarin.result" = event_val
        ))
    }

    x %>%
        assign_names(varnames, extras) %>%
        dplyr::mutate_at("warfarin.event", stringr::str_to_lower) %>%
        format_dates("warfarin.datetime") %>%
        assign_class(x, "warfarin")
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
is.meds_admin <- function(x) inherits(x, "meds_admin")

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
is.md <- function(x) inherits(x, "md")

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
is.order_verify <- function(x) inherits(x, "order_verify")

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
