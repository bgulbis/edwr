# read_data.R

#' Read data from csv files
#'
#' \code{read_data} takes a directory and file name and reads in all matching
#' csv files and binds them together into a data frame
#'
#' This function takes a directory and file name and reads in all matching csv
#' files and binds them together into a data frame using
#' \code{\link[readr]{read_csv}} from \code{readr}. The resulting data frame has
#' a class of \code{edwr}, and can then be passed to one of the edwr-related
#' class types for variable renaming and type setting (example: \code{as.labs}).
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
#'   MBO data
#'
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
read_data <- function(data.dir, file.name, edw = TRUE) {
    # get list of files in specified directory and matching file name
    x <- list.files(data.dir, pattern = file.name, full.names = TRUE) %>%
        purrr::map_df(
            readr::read_csv,
            col_types = readr::cols(.default = "c"),
            na = c("", "NA", "Unknown")
        ) %>%
        as.tbl_edwr()

    # set attribute to indicate source of data as EDW or MBO
    if (edw) {
        attr(x, "data") <- "edw"
    } else {
        attr(x, "data") <- "mbo"
    }

    x
}
