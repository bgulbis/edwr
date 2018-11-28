# read_data.R

#' Read data from csv files
#'
#' \code{read_data} takes a directory and file name and reads in all matching
#' csv files and binds them together into a data frame
#'
#' This function takes a directory and file name and reads in all matching csv
#' files and binds them together into a data frame using \code{read_csv} from
#' \code{readr}. The resulting data frame has a class of \code{edwr}, and can
#' then be passed to one of the edwr-related class types for variable renaming
#' and type setting (example: \code{as.labs}).
#'
#' The following table lists the edwr-related class which corresponds to each
#' EDW Standard Project Queries.
#'
#' \tabular{lcl}{ \strong{edwr Class Name} \tab --- \tab \strong{EDW Query
#' Name}\cr blood \tab \tab Blood Products\cr charges \tab \tab Charges - [All,
#' Department Prompt]\cr demographics \tab \tab Demographics\cr diagnosis \tab
#' \tab Diagnosis Codes (ICD-9/10-CM) - All\cr drg \tab \tab DRG Codes - All\cr
#' encounters \tab \tab Encounters - by Person ID\cr events \tab \tab Clinical
#' Events - Prompt\cr icu_assess \tab \tab ICU Assessments (CAM-ICU, GCS,
#' RASS)\cr id \tab \tab Identifiers - by FIN\cr \tab \tab Identifiers - by
#' MRN\cr \tab \tab Identifiers - by PowerInsight Encounter Id\cr labs \tab \tab
#' Labs - ABG\cr \tab \tab Labs - CBC\cr \tab \tab Labs - Chemistry\cr \tab \tab
#' Labs - Coags\cr \tab \tab Labs - HIT\cr \tab \tab Labs - LFTs\cr \tab \tab
#' Labs - Pregnancy\cr \tab \tab Labs - Renal\cr \tab \tab Urine Drug Screen\cr
#' \tab \tab Vomiting Output\cr locations \tab \tab Location History\cr measures
#' \tab \tab Measures (Height and Weight)\cr meds_cont \tab \tab Medications -
#' Inpatient Continuous - [All, Prompt]\cr meds_freq \tab \tab Medications -
#' Inpatient Intermittent with Frequency - [All, Prompt]\cr meds_home \tab \tab
#' Medications - Home and Discharge - [All, Prompt]\cr meds_sched \tab \tab
#' Medications - Inpatient Intermittent - [All, Prompt]\cr mpp \tab \tab MPP's
#' Ordered - [All, Order Prompt]\cr mrn \tab \tab Identifiers - Person by MRN\cr
#' order_by \tab \tab Orders - Prompt\cr order_detail \tab \tab Orders - from
#' Clinical Event Id - Prompt\cr patients \tab \tab Patients - by CDM charge -
#' HealthQuest - [Location Prompt, System]\cr \tab \tab Patients - by
#' ICD-9/10-CM Diagnosis Code\cr \tab \tab Patients - by Medication\cr \tab \tab
#' Patients - by Unit Admission\cr problems \tab \tab Problem List\cr procedures
#' \tab \tab Procedure Codes (ICD-9-CM/ICD-10-PCS) - All\cr radiology \tab \tab
#' Radiology Reports\cr services \tab \tab Service History\cr surgeries \tab
#' \tab Surgeries\cr uop \tab \tab Urine output\cr vent_settings \tab \tab
#' Ventilator Data - Settings\cr vent_times \tab \tab Ventilator Data - Start
#' and Stop\cr visits \tab \tab Visit Data\cr vitals \tab \tab Vitals\cr
#' warfarin \tab \tab Warfarin Information\cr }
#'
#' @param data.dir A character string with the name of the directory containing
#'   the data files
#' @param file.name A character string with name of data file or pattern to
#'   match
#' @param edw A boolean indicating if the data came from EDW; set to FALSE for
#' @param archive A boolean indicating if the data was extracted as a csv
#'   archive. If TRUE, will recursively search folders for csv files and will
#'   skip the first line for column headers
#' @return A data frame of class \code{tbl_edwr}
#'
#' @examples
#' x <- read_data(
#'   data.dir = paste0(system.file(package = "edwr", "extdata")),
#'   file.name = "demographics.csv"
#' )
#'
#' str(x)
#' class(x)
#'
#' y <- as.demographics(x)
#' class(y)
#'
#' @export
read_data <- function(data.dir, file.name, edw = TRUE, archive = FALSE) {
    if (archive) {
        recursive = TRUE
        skip = 0
    } else {
        recursive = FALSE
        skip = 0
    }

    # get list of files in specified directory and matching file name
    x <- list.files(
        data.dir,
        pattern = file.name,
        full.names = TRUE,
        recursive = recursive
    ) %>%
        purrr::map_df(
            readr::read_csv,
            col_types = readr::cols(.default = "c"),
            na = c("", "NA", "Unknown"),
            skip = skip
        ) %>%
        as.tbl_edwr()

    # set attribute to indicate source of data as EDW or MBO
    if (edw) {
        attr(x, "data") <- "edw"
    } else {
        attr(x, "data") <- "mbo"
    }

    attr(x, "archive") <- archive

    x
}

# column name assignments ------------------------------
col_names <- list(
    "admit.datetime" = "Date and Time - Admit",
    "age" = "Age- Years (At Admit)",
    "disposition" = "Discharge Disposition",
    "discharge.datetime" = "Date and Time - Discharge",
    "event.id" = "Med Event Id",
    "event.tag" = "Infusion Actions",
    "facility" = "Facility (Curr)",
    "fin" = "Financial Number",
    "fin" = "Formatted Financial Nbr",
    "freq" = "Frequency",
    "gender" = "Gender",
    "length.stay" = "LOS (Curr)",
    "med" = "Medication (Generic)",
    "med.datetime" = "Date and Time - Administration",
    "med.dose" = "Admin Dosage",
    "med.dose.units" = "Admin Dosage Unit",
    "med.location" = "Nurse Unit (Med)",
    "med.rate" = "Infusion Rate",
    "med.rate.units" = "Infusion Unit",
    "millennium.id" = "Encounter Identifier",
    "order" = "Mnemonic (Primary Generic) FILTER ON",
    "order.datetime" = "Date and Time - Original (Placed)",
    "order.id" = "Order Id",
    "order.location" = "Nurse Unit (Order)",
    "order.parent.id" = "Parent Order Id",
    "order.provider" = "Ordering Provider LIMITS",
    "order.provider.position" = "Ordering Provider Position LIMITS",
    "order.route" = "Order Route",
    "pie.id" = "PowerInsight Encounter Id",
    "prn" = "PRN Indicator",
    "race" = "Race",
    "route" = "Admin Route",
    "service" = "Medical Service",
    "service.start.datetime" = "Medical Service Begin Date & Time",
    "service.stop.datetime" = "Medical Service End Date & Time",
    "visit.type" = "Encounter Class Subtype"
)

#' Read data, automatically assign names and format dates
#'
#' @param data.dir A character string with the name of the directory containing
#'   the data files
#' @param file.name A character string with name of data file or pattern to
#'   match
#' @param recursive logical. Should the listing recurse into directories?
#' @param skip Number of lines to skip before reading data.
#' @param edw logical. TRUE if the data came from EDW, FALSE for MBO
#'
#' @return A data frame of class \code{tbl_edwr}
#' @export
read_data2 <- function(data.dir, file.name, edw = TRUE, recursive = FALSE, skip = 0) {

    if (edw) {
        my_locale <- readr::locale(tz = "US/Central")
    } else {
        my_locale <- readr::locale()
    }

    # get list of files in specified directory and matching file name
    x <- list.files(
        data.dir,
        pattern = file.name,
        full.names = TRUE,
        recursive = recursive
    ) %>%
        purrr::map_df(
            readr::read_csv,
            # col_types = readr::cols(.default = "c"),
            na = c("", "NA", "Unknown"),
            skip = skip,
            locale = my_locale
        ) %>%
        as.tbl_edwr() %>%
        distinct()

    y <- col_names %in% colnames(x)
    z <- "datetime"

    x <- x %>%
        rename(!!!col_names[y]) %>%
        mutate_at(
            dplyr::vars(dplyr::contains(z)),
            stringr::str_replace_all,
            pattern = "/",
            replacement = "-"
        ) %>%
        mutate_at(
            dplyr::vars(dplyr::contains(z)),
            readr::parse_datetime,
            locale = my_locale
        ) %>%
        mutate_at(
            dplyr::vars(dplyr::contains(z)),
            lubridate::with_tz,
            tzone = "US/Central"
        )

    # set attribute to indicate source of data as EDW or MBO
    if (edw) {
        attr(x, "data") <- "edw"
    } else {
        attr(x, "data") <- "mbo"
    }

    x
}

