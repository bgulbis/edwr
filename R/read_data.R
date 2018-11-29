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
    "action.comm" = "Action Communication Type",
    "action.comm" = "Order Communication Type",
    "action.datetime" = "Date and Time - Order Action",
    "action.datetime" = "Order Action Date & Time",
    "action.provider" = "Action Personnel",
    "action.provider" = "Order Action Personnel",
    "action.provider.role" = "Action Personnel Position",
    "action.provider.role" = "Order Action Personnel Position",
    "action.type" = "Action Type",
    "action.type" = "Order Action Type",
    "active" = "Problem Source Active Indicator",
    "admin.end.datetime" = "Date and Time - Administration End",
    "admit.datetime" = "Admit Date & Time",
    "admit.datetime" = "Date and Time - Admit",
    "admit.source" = "Admit Source",
    "admit.type" = "Admit Type",
    "add.on" = "Add On Indicator",
    "age" = "Age- Years (At Admit)",
    "age" = "Age- Years (Visit)",
    "arrival.datetime" = "Arrival Date & Time",
    "arrival.datetime" = "Date and Time - Arrival",
    "arrive.datetime" = "Date and Time - Nurse Unit Begin",
    "arrive.datetime" = "Location Arrival Date & Time",
    "asa.class" = "ASA Class Description",
    "attending" = "Attending Physician-All",
    "cancel.datetime" = "Order Cancel Date & Time",
    "cancel.person" = "Order Personnel - Cancel",
    "cancel.reason" = "Order Cancel Reason",
    "cdm.code" = "Cdm Code",
    "classification" = "Problem Classification",
    "code.source" = "Diagnosis Code Source Vocabulary",
    "complete.datetime" = "Order Complete Date & Time",
    "complete.person" = "Complete Personnel",
    "confirm" = "Problem Confirmation Status",
    "depart.datetime" = "Date and Time - Nurse Unit End",
    "depart.datetime" = "Location Depart Date & Time",
    "detail" = "Detail Display Value",
    "detail.datetime" = "Detail Date & Time",
    "detail.descr" = "Field Description",
    "diag.code" = "Diagnosis Code",
    "diag.seq" = "Diagnosis Code (Primary VS Non Primary)",
    "diag.seq" = "Diagnosis Code Sequence",
    "diag.type" = "Diagnosis Type",
    "disposition" = "Discharge Disposition",
    "discharge.datetime" = "Date and Time - Discharge",
    "discontinue.datetime" = "Order Discontinue Date & Time",
    "document.source" = "Med Documentation Source",
    "drg" = "DRG Code",
    "drg" = "Grouping Code",
    "drg.desc" = "DRG Description",
    "drg.priority" = "DRG Priority",
    "drg.source" = "Grouping Code Source Vocabulary",
    "drg.type" = "Grouping Code Type",
    "end.datetime" = "Medical Service End Date & Time",
    "event" = "Clinical Event",
    "event.datetime" = "Date and Time - Performed",
    "event.datetime" = "Date and Time - Scheduled OR Given On",
    "event.datetime" = "Clinical Event End Date/Time",
    "event.id" = "Event ID",
    "event.id" = "Med Event Id",
    "event.location" = "Nurse Unit (Event)",
    "event.location" = "Nurse Unit of Clinical Event",
    "event.parent.id" = "Parent Event Id",
    "event.result" = "Clinical Event Result",
    "event.result.units" = "Clinical Event Result Units",
    "event.tag" = "Event Tag",
    "event.tag" = "Infusion Actions",
    "facility" = "Facility (Curr)",
    "facility" = "Person Location- Facility (Curr)",
    "fin" = "Financial Number",
    "fin" = "Formatted Financial Nbr",
    "freq" = "Frequency",
    "freq" = "Parent Order Frequency Description",
    "free.text" = "Problem Free Text",
    "gender" = "Gender",
    "gender" = "Sex",
    "ingredient" = "Ingredient Catalog Short Desc",
    "ingredient.dose" = "Dose Strength - Order",
    "ingredient.dose" = "Order Strength Dose",
    "ingredient.dose" = "Order Volume Dose",
    "ingredient.freetext" = "Dose Freetext",
    "ingredient.unit" = "Dose Strength - Unit",
    "ingredient.unit" = "Order Strength Dose Unit",
    "ingredient.unit" = "Order Volume Dose Unit",
    "institution" = "Institution Desc",
    "lab" = "Lab Event (FILTER ON)",
    "lab.datetime" = "Date and Time - Nurse Draw",
    "lab.draw.location" = "Nurse Unit (Lab)",
    "lab.id" = "Lab Event Id",
    "lab.result" = "Lab Result",
    "lab.result.units" = "Lab Result Units",
    "length.stay" = "LOS (Actual)",
    "length.stay" = "LOS (Curr)",
    "life.cycle" = "Problem Life Cycle",
    "life.cycle.datetime" = "Problem Life Cycle Date & Time",
    "md.start" = "Date and Time - Phys Begin Effective",
    "md.stop" = "Date and Time - Phys End Effective",
    "med" = "Medication (Generic)",
    "med" = "Mnemonic (Primary Generic) FILTER ON",
    "med" = "Order Catalog Short Description",
    "med.datetime" = "Date and Time - Administration",
    "med.dose" = "Admin Dosage",
    "med.dose" = "Dosage Amount",
    "med.dose.units" = "Admin Dosage Unit",
    "med.dose.units" = "Dosage Unit",
    "med.location" = "Nurse Unit (Med)",
    "med.rate" = "Infusion Rate",
    "med.rate.units" = "Infusion Rate Unit",
    "med.rate.units" = "Infusion Unit",
    "med.type" = "Order Type",
    "med.type" = "Orig Orderable Type-Flag Desc",
    "millennium.id" = "Encounter Identifier",
    "millennium.id" = "Millennium Encounter ID",
    "mpp" = "MPP (which generated order)",
    "mrn" = "MRN- Organization",
    "nurse.unit" = "Nurse Unit (Curr)",
    "nurse.unit.admit" = "Person Location- Nurse Unit (Admit)",
    "nurse.unit.admit" = "Nurse Unit (Inpatient Admit) LIMITS",
    "onset.datetime" = "Problem Onset Date & Time",
    "order" = "Mnemonic (Primary Generic) FILTER ON",
    "order" = "Order Catalog Mnemonic",
    "order.datetime" = "Date and Time - Original (Placed)",
    "order.datetime" = "Order Date & Time",
    "order.id" = "Clinical Event Order ID",
    "order.id" = "Order Id",
    "order.id" = "Source Order ID",
    "order.location" = "Nurse Unit (Order)",
    "order.location" = "Person Location- Nurse Unit (Order)",
    "order.name" = "Order Catalog Mnemonic",
    "order.parent.id" = "Parent Order Id",
    "order.provider" = "Ordering Provider LIMITS",
    "order.provider.position" = "Ordering Provider Position LIMITS",
    "order.route" = "Order Route",
    "order.status" = "Order Department Status - Generic",
    "order.status" = "Order Status (Curr)",
    "person.id" = "Person ID",
    "pie.id" = "PowerInsight Encounter Id",
    "present.admit" = "Present on Admission",
    "prn" = "PRN Indicator",
    "problem" = "Problem - Description",
    "primary.proc" = "Primary Procedure Indicator",
    "proc.date" = "Procedure Date and Time",
    "proc.code" = "Procedure Code",
    "proc.seq" = "Procedure Code Sequence",
    "proc.source" = "Procedure Code Source Vocabulary",
    "provider" = "Action Provider",
    "provider.role" = "Action Provider Position",
    "race" = "Race",
    "recovery.in" = "Patient In Recovery Date/Time",
    "recovery.out" = "Patient Out Recovery Date/Time",
    "request.datetime" = "Order Request Date & Time",
    "room.in" = "Patient In Room Date/Time",
    "room.out" = "Patient Out Room Date/Time",
    "route" = "Admin Route",
    "route" = "Route of Administration - Short",
    "scan.patient" = "Scanned Armband (PPID)",
    "scan.med" = "Scanned Medication (PMID)",
    "scheduled.datetime" = "Date and Time - Scheduled",
    "service" = "Medical Service",
    "service.date" = "Service Date",
    "severity" = "Problem Severity",
    "start.datetime" = "Date and Time - Order Start",
    "start.datetime" = "Medical Service Begin Date & Time",
    "surgery" = "Procedure",
    "surgery.start.datetime" = "Start Date/Time",
    "surgery.stop.datetime" = "Stop Date/Time",
    "surgery.type" = "Surgical Case Specialty",
    "type.flag" = "Orig Orderable Type-Flag",
    "unit.from" = "Person Location - Nurse Unit (From)",
    "unit.to" = "Person Location - Nurse Unit (To)",
    "unit.name" = "Nurse Unit All",
    "verify.datetime" = "Date and Time - Pharmacist Review",
    "visit.subtype" = "Encounter Class Subtype",
    "visit.type" = "Encounter Class Type",
    "visit.type" = "Encounter Type",
    "visit.type.class" = "Encounter Type Class"
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

