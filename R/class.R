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

# create classes which inherit from edwr class; each class represents a
# different type of data from EDW (labs, meds, demographics, etc.); in general,
# these functions will rename variables to desired names, remove duplicate rows,
# convert names (of labs, meds, etc.) to lower case (to avoid case-sensitive
# matching errors when working with the data), parse date/time values into
# POSIXct vectors; any extra columns contained in the csv file will be left
# unchanged

#' @rdname set_edwr_class
#' @export
as.blood <- function(x) {
    # inherits from events class
    if (missing(x)) x <- character()
    if (is.blood(x)) return(x)
    if (!is.edwr(x)) x <- as.edwr(x)
    if (!is.events(x)) x <- as.events(x)

    prods <- c("Cryo(.*)" = "cryo",
               "FFP(.*)" = "ffp",
               "(P)?RBC(.*)" = "prbc",
               "Platelet(.*)" = "platelet")

    df <- rename_(.data = x, .dots = list(
        "blood.datetime" = "event.datetime",
        "blood.prod" = "event",
        "blood.type" = "event.result"
    )) %>%
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
    # inherits from events class
    if (missing(x)) x <- character()
    if (is.icu_assess(x)) return(x)
    if (!is.edwr(x)) x <- as.edwr(x)
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
    # inherits from events class
    if (missing(x)) x <- character()
    if (is.labs(x)) return(x)
    if (!is.edwr(x)) x <- as.edwr(x)
    if (!is.events(x)) x <- as.events(x)

    df <- rename_(.data = x, .dots = list(
        "lab.datetime" = "event.datetime",
        "lab" = "event",
        "lab.result" = "event.result"
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
    # inherits from events class
    if (missing(x)) x <- character()
    if (is.measures(x)) return(x)
    if (!is.edwr(x)) x <- as.edwr(x)
    if (!is.events(x)) x <- as.events(x)

    df <- rename_(.data = x, .dots = list(
        "measure.datetime" = "event.datetime",
        "measure" = "event",
        "measure.result" = "event.result",
        "measure.units" = "`Clinical Event Result Units`"
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
    # inherits from meds_sched class
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
as.mrn <- function(x) {
    if (missing(x)) stop("Missing object")
    if (is.mrn(x)) return(x)
    if (!is.edwr(x)) x <- as.edwr(x)

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
    if (!is.edwr(x)) x <- as.edwr(x)

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
as.order_by <- function(x) {
    if (missing(x)) x <- character()
    if (is.order_by(x)) return(x)
    if (!is.edwr(x)) x <- as.edwr(x)

    df <- rename_(.data = x, .dots = c(val.pie, list(
        "order" = "`Order Catalog Mnemonic`",
        "order.unit" = "`Person Location- Nurse Unit (Order)`",
        "provider" = "`Action Provider`",
        "provider.role" = "`Action Provider Position`",
        "action.datetime" = "`Order Action Date & Time`",
        "action.type" = "`Action Type`",
        "action.provider" = "`Action Personnel`",
        "action.provider.role" = "`Action Personnel Position`",
        "action.comm" = "`Action Communication Type`"
    ))) %>%
        dplyr::distinct_() %>%
        mutate_(.dots = set_names(
            x = list(~format_dates(action.datetime)),
            nm = list("action.datetime")
        ))

    after <- match("order_by", class(x), nomatch = 0L)
    class(df) <- append(class(x), "order_by", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.order_detail <- function(x) {
    if (missing(x)) x <- character()
    if (is.order_detail(x)) return(x)
    if (!is.edwr(x)) x <- as.edwr(x)

    df <- rename_(.data = x, .dots = c(val.pie, list(
        "order.id" = "`Source Order ID`",
        "order" = "`Order Catalog Mnemonic`",
        "ingredient" = "`Ingredient Catalog Short Desc`",
        "ingredient.dose" = "`Dose Strength - Order`",
        "ingredient.unit" = "`Dose Strength - Unit`",
        "ingredient.freetext" = "`Dose Freetext`",
        "order.unit" = "`Person Location- Nurse Unit (Order)`",
        "action.type" = "`Action Type`",
        "action.datetime" = "`Order Action Date & Time`"
    ))) %>%
        dplyr::distinct_() %>%
        mutate_(.dots = set_names(
            x = list(~format_dates(action.datetime)),
            nm = list("action.datetime")
        ))

    after <- match("order_detail", class(x), nomatch = 0L)
    class(df) <- append(class(x), "order_detail", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.patients <- function(x) {
    if (missing(x)) stop("Missing object")
    if (is.patients(x)) return(x)
    if (!is.edwr(x)) x <- as.edwr(x)

    df <- rename_(.data = x, .dots = c(val.pie, list(
        "age" = "`Age- Years (Visit)`",
        "discharge.datetime" = "`Discharge Date & Time`",
        "visit.type" = "`Encounter Type`",
        "facility" = "`Person Location- Facility (Curr)`"
    ))) %>%
        dplyr::distinct_() %>%
        readr::type_convert(col_types = readr::cols(pie.id = "c")) %>%
        mutate_(.dots = set_names(
            x = list(~format_dates(discharge.datetime)),
            nm = list("discharge.datetime")
        ))

    after <- match("patients", class(x), nomatch = 0L)
    class(df) <- append(class(x), "patients", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.problems <- function(x) {
    if (missing(x)) stop("Missing object")
    if (is.problems(x)) return(x)
    if (!is.edwr(x)) x <- as.edwr(x)

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
as.procedures <- function(x) {
    if (missing(x)) stop("Missing object")
    if (is.procedures(x)) return(x)
    if (!is.edwr(x)) x <- as.edwr(x)

    df <- rename_(.data = x, .dots = c(val.pie, list(
        "proc.date" = "`Procedure Date and Time`",
        "proc.code" = "`Procedure Code`",
        "proc.source" = "`Procedure Code Source Vocabulary`"
    ))) %>%
        dplyr::distinct_() %>%
        mutate_(.dots = set_names(
            x = list(~format_dates(proc.date)),
            nm = "proc.date"
        ))

    after <- match("procedures", class(x), nomatch = 0L)
    class(df) <- append(class(x), "procedures", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.radiology <- function(x) {
    if (missing(x)) stop("Missing object")
    if (is.radiology(x)) return(x)
    if (!is.edwr(x)) x <- as.edwr(x)

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
as.surgeries <- function(x) {
    if (missing(x)) stop("Missing object")
    if (is.surgeries(x)) return(x)
    if (!is.edwr(x)) x <- as.edwr(x)

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
    if (!is.edwr(x)) x <- as.edwr(x)
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
    if (!is.edwr(x)) x <- as.edwr(x)
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
as.vitals <- function(x) {
    # inherits from events class
    if (missing(x)) x <- character()
    if (is.vitals(x)) return(x)
    if (!is.edwr(x)) x <- as.edwr(x)
    if (!is.events(x)) x <- as.events(x)

    df <- rename_(.data = x, .dots = list(
        "vital.datetime" = "event.datetime",
        "vital" = "event",
        "vital.result" = "event.result"
    ))

    after <- match("vitals", class(x), nomatch = 0L)
    class(df) <- append(class(x), "vitals", after = after)
    df
}

#' @rdname set_edwr_class
#' @export
as.warfarin <- function(x) {
    # inherits from events class
    if (missing(x)) x <- character()
    if (is.warfarin(x)) return(x)
    if (!is.edwr(x)) x <- as.edwr(x)
    if (!is.events(x)) x <- as.events(x)

    df <- rename_(.data = x, .dots = list(
        "warfarin.datetime" = "event.datetime",
        "warfarin.event" = "event",
        "warfarin.result" = "event.result"
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
#' @keywords internal
is.edwr <- function(x) inherits(x, "edwr")

#' @rdname is.edwr
#' @export
is.blood <- function(x) inherits(x, "blood")

#' @rdname is.edwr
#' @export
is.charges <- function(x) inherits(x, "charges")

#' @rdname is.edwr
#' @export
is.demographics <- function(x) inherits(x, "demographics")

#' @rdname is.edwr
#' @export
is.diagnosis <- function(x) inherits(x, "diagnosis")

#' @rdname is.edwr
#' @export
is.encounters <- function(x) inherits(x, "encounters")

#' @rdname is.edwr
#' @export
is.events <- function(x) inherits(x, "events")

#' @rdname is.edwr
#' @export
is.icu_assess <- function(x) inherits(x, "icu_assess")

#' @rdname is.edwr
#' @export
is.id <- function(x) inherits(x, "id")

#' @rdname is.edwr
#' @export
is.labs <- function(x) inherits(x, "labs")

#' @rdname is.edwr
#' @export
is.locations <- function(x) inherits(x, "locations")

#' @rdname is.edwr
#' @export
is.measures <- function(x) inherits(x, "measures")

#' @rdname is.edwr
#' @export
is.meds_cont <- function(x) inherits(x, "meds_cont")

#' @rdname is.edwr
#' @export
is.meds_freq <- function(x) inherits(x, "meds_freq")

#' @rdname is.edwr
#' @export
is.meds_home <- function(x) inherits(x, "meds_home")

#' @rdname is.edwr
#' @export
is.meds_sched <- function(x) inherits(x, "meds_sched")

#' @rdname is.edwr
#' @export
is.mpp <- function(x) inherits(x, "mpp")

#' @rdname is.edwr
#' @export
is.mrn <- function(x) inherits(x, "mrn")

#' @rdname is.edwr
#' @export
is.order_by <- function(x) inherits(x, "order_by")

#' @rdname is.edwr
#' @export
is.order_detail <- function(x) inherits(x, "order_detail")

#' @rdname is.edwr
#' @export
is.patients <- function(x) inherits(x, "patients")

#' @rdname is.edwr
#' @export
is.problems <- function(x) inherits(x, "problems")

#' @rdname is.edwr
#' @export
is.procedures <- function(x) inherits(x, "procedures")

#' @rdname is.edwr
#' @export
is.radiology <- function(x) inherits(x, "radiology")

#' @rdname is.edwr
#' @export
is.services <- function(x) inherits(x, "services")

#' @rdname is.edwr
#' @export
is.surgeries <- function(x) inherits(x, "surgeries")

#' @rdname is.edwr
#' @export
is.uop <- function(x) inherits(x, "uop")

#' @rdname is.edwr
#' @export
is.vent_settings <- function(x) inherits(x, "vent_settings")

#' @rdname is.edwr
#' @export
is.vent_times <- function(x) inherits(x, "vent_times")

#' @rdname is.edwr
#' @export
is.visits <- function(x) inherits(x, "visits")

#' @rdname is.edwr
#' @export
is.vitals <- function(x) inherits(x, "vitals")

#' @rdname is.edwr
#' @export
is.warfarin <- function(x) inherits(x, "warfarin")
