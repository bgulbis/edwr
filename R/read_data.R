#' Read data from csv files
#'
#' \code{read_data} takes a directory and file name and reads in all
#' matching csv files and binds them together into a data frame
#'
#' This function takes a directory and file name and reads in all matching csv
#' files and binds them together into a data frame using
#' \code{\link[readr]{read_csv}} from the readr package.
#'
#' Valid options for type include:
#'
#' \itemize{
#'   \item skip (read in columns without renaming)
#'   \item blood (blood product administration)
#'   \item charges (charges by CDM code)
#'   \item demographics (patient demographics)
#'   \item diagnosis (ICD-9/10-CM codes)
#'   \item encounters (all encounters by person)
#'   \item events (generic clinical events)
#'   \item home_meds (home / discharge medication lists)
#'   \item icu_assess (RASS, GCS, etc.)
#'   \item id (encounter ID, FIN, person ID)
#'   \item labs (lab results)
#'   \item measures (height, weight)
#'   \item meds_cont (continuous medication administration)
#'   \item meds_sched (intermittent medication administration)
#'   \item meds_freq (intermittent meds with order frequency)
#'   \item mpp (MPP used to order medication)
#'   \item mrn (medical record number)
#'   \item order_by (ordering provider data)
#'   \item order_detail (medication order data)
#'   \item patients (patients identified by CDM, diagnosis codes, or unit admission)
#'   \item problems (problem list from EMR)
#'   \item procedures9 (ICD-9 procedure codes)
#'   \item procedures10 (ICD-10 procedure codes)
#'   \item radiology (radiology orders)
#'   \item services (list of patient's primary medical services)
#'   \item surgeries (list of surgeries from surgery database)
#'   \item uop (urine output)
#'   \item vent_settings (FIO2, PaO2, etc.)
#'   \item vent_start (vent start/stop dates/times)
#'   \item visits (arrival, admit, discharge data)
#'   \item vitals (vital signs)
#'   \item warfarin (warfarin duration, indication, goal)
#' }
#'
#' @param data.dir A character string with the name of the directory containing
#'   the data files
#' @param file.name A character string with name of data file or pattern to
#'   match
#' @param type An optional character string indicating type of data being
#'   tidied; uses file.name if no value is provided
#' @param check.distinct An optional logical, calls
#'   \code{\link[dplyr]{distinct}} on the imported data if \code{TRUE}
#' @param include.pts If not NULL, the returned data frame will be limited to
#'   only the included patients
#'
#' @return A data frame, as an \code{edwr} class type if type is not skip
#'
#' @seealso \code{\link[readr]{read_csv}}
#'
#' @examples
#' x <- read_data(
#'   data.dir = paste0(system.file(package = "edwr", "extdata")),
#'   file.name = "test_demographics.csv",
#'   type = "demographics"
#' )
#'
#' str(x)
#'
#' @export
read_data <- function(data.dir,
                          file.name,
                          type = NULL,
                          check.distinct = TRUE,
                          include.pts = NULL) {

    # if type is NA, then set type to file.name
    if (is.null(type)) {
        type <- file.name
    }

    # get list of files is specified directory and matching file name
    raw <- list.files(data.dir, pattern = file.name, full.names = TRUE)

    exclude <- c("", "Unknown")
    raw.names <- list(id = "PowerInsight Encounter Id",
                      dt = "Clinical Event End Date/Time",
                      ev = "Clinical Event",
                      res = "Clinical Event Result")
    pt.id <- "pie.id"
    dots <- NULL
    nm <- NULL

    col_dt <- readr::col_datetime(format = "%Y/%m/%d %H:%M:%S")

    # set defaults which are used in multiple types
    col.raw <- c(raw.names$id, raw.names$dt, raw.names$ev, raw.names$res)
    col.names <- c(pt.id, "event.datetime", "event", "result")
    col.types <- readr::cols_only("c", col_dt, "c", "c")

    switch(type,
           skip = {
               # read in columns as they are without renaming
               read <- purrr::map_df(raw, readr::read_csv)
               return(read)
           },

           blood = {
               # use default columns
               col.names <- c(pt.id,
                              "blood.datetime",
                              "blood.prod",
                              "blood.type")
               prods <- c("Cryo(.*)" = "cryo",
                          "FFP(.*)" = "ffp",
                          "(P)?RBC(.*)" = "prbc",
                          "Platelet(.*)" = "platelet")

               dots <- list(~stringr::str_replace_all(blood.prod, prods))
               nm <- "blood.prod"
           },

           charges = {
               col.raw <- c(raw.names$id,
                            "Cdm Code",
                            "Service Date",
                            "Institution Desc")
               col.names <- c(pt.id,
                              "cdm.code",
                              "service.date",
                              "institution")
               col.types <- readr::cols_only("c", "c", col_dt, "c")
           },

           demographics = {
               col.raw <- c(raw.names$id,
                            "Age- Years (Visit)",
                            "Sex",
                            "Race",
                            "Discharge Disposition",
                            "LOS (Actual)",
                            "Encounter Type",
                            "Person ID",
                            "Person Location- Facility (Curr)")
               col.names <- c(pt.id,
                              "age",
                              "sex",
                              "race",
                              "disposition",
                              "length.stay",
                              "visit.type",
                              "person.id",
                              "facility")
               col.types <- readr::cols("c", "i", "c", "c", "c", "d", "c",
                                        "c", "c")
           },

           diagnosis = {
               col.raw <- c(raw.names$id,
                            "Diagnosis Code",
                            "Diagnosis Code Source Vocabulary",
                            "Diagnosis Type",
                            "Diagnosis Code Sequence",
                            "Present on Admission")
               col.names <- c(pt.id,
                              "diag.code",
                              "code.source",
                              "diag.type",
                              "diag.seq",
                              "present.admit")
               col.types <- readr::cols_only("c", "c", "c", "c", "c", "c")
           },

           encounters = {
               col.raw <- c("Person ID",
                            "Admit Date & Time",
                            raw.names$id,
                            "Encounter Type",
                            "Person Location- Facility (Curr)",
                            "Discharge Disposition")
               col.names <- c("person.id",
                              "admit.datetime",
                              pt.id,
                              "visit.type",
                              "facility",
                              "disposition")
               col.types <- readr::cols("c", col_dt, "c", "c", "c", "c")
           },

           events = {
               # use default columns
               col.names <- c(pt.id,
                              "event.datetime",
                              "event",
                              "event.result")
               dots <- list(~stringr::str_to_lower(event))
               nm <- "event"
           },

           home_meds = {
               col.raw <- c(raw.names$id,
                            "Order Catalog Short Description",
                            "Order Catalog Mnemonic",
                            "Orig Orderable Type-Flag Desc")
               col.names <- c(pt.id,
                              "med",
                              "order.name",
                              "med.type")
               col.types <- readr::cols("c", "c", "c", "c")
               dots <- list(~stringr::str_to_lower(med))
               nm <- "med"
           },

           icu_assess = {
               # use default columns
               col.names <- c(pt.id,
                              "assess.datetime",
                              "assessment",
                              "assess.result")
               dots <- list(~stringr::str_to_lower(assessment))
               nm <- "assessment"
           },

           id = {
               col.raw <- c(raw.names$id,
                            "Formatted Financial Nbr",
                            "Person ID")
               col.names <- c(pt.id,
                              "fin",
                              "person.id")
               col.types <- readr::cols("c", "c", "c")
           },

           labs = {
               # use default columns
               col.names <- c(pt.id,
                              "lab.datetime",
                              "lab",
                              "lab.result")
               dots <- list(~stringr::str_to_lower(lab))
               nm <- "lab"
           },

           locations = {
               col.raw <- c(raw.names$id,
                            "Location Arrival Date & Time",
                            "Location Depart Date & Time",
                            "Person Location - Nurse Unit (To)",
                            "Person Location - Nurse Unit (From)")
               col.names <- c(pt.id,
                              "arrive.datetime",
                              "depart.datetime",
                              "unit.to",
                              "unit.from")
               col.types <- readr::cols("c", col_dt, col_dt, "c", "c")
               dots <- list(~dplyr::na_if(unit.to, ""),
                            ~dplyr::na_if(unit.from, ""))
               nm <- list("unit.to", "unit.from")
           },

           measures = {
               col.raw <- c(raw.names$id,
                            raw.names$dt,
                            raw.names$ev,
                            raw.names$res,
                            "Clinical Event Result Units")
               col.names <- c(pt.id,
                              "measure.datetime",
                              "measure",
                              "measure.result",
                              "measure.units")
               col.types <- readr::cols("c", col_dt, "c", "d", "c")
           },

           meds_cont = {
               col.raw <- c(raw.names$id,
                            "Clinical Event Order ID",
                            "Event ID",
                            raw.names$dt,
                            raw.names$ev,
                            "Infusion Rate",
                            "Infusion Rate Unit",
                            "Route of Administration - Short",
                            "Event Tag")
               col.names <- c(pt.id,
                              "order.id",
                              "event.id",
                              "med.datetime",
                              "med",
                              "med.rate",
                              "med.rate.units",
                              "route",
                              "event.tag")
               col.types <- readr::cols("c", "c", "c", col_dt, "c", "d", "c",
                                        "c", "c")
               dots <- list(~stringr::str_to_lower(med),
                            ~dplyr::na_if(med.rate.units, ""))
               nm <- list("med", "med.rate.units")
           },

           meds_sched = {
               col.raw <- c(raw.names$id,
                            "Clinical Event Order ID",
                            raw.names$dt,
                            raw.names$ev,
                            "Dosage Amount",
                            "Dosage Unit",
                            "Route of Administration - Short",
                            "Event ID")
               col.names <- c(pt.id,
                              "med.datetime",
                              "med",
                              "med.dose",
                              "med.dose.units",
                              "med.route",
                              "event.id")
               col.types <- readr::cols_only("c", "c", col_dt, "c", "d", "c",
                                             "c", "c")
               dots <- list(~stringr::str_to_lower(med))
               nm <- "med"
           },

           meds_freq = {
               col.raw <- c(raw.names$id,
                            "Clinical Event Order ID",
                            raw.names$dt,
                            raw.names$ev,
                            "Dosage Amount",
                            "Dosage Unit",
                            "Route of Administration - Short",
                            "Parent Order Frequency Description",
                            "Event ID")
               col.names <- c(pt.id,
                              "med.datetime",
                              "med",
                              "med.dose",
                              "med.dose.units",
                              "med.route",
                              "freq",
                              "event.id")
               col.types <- readr::cols_only("c", "c", col_dt, "c", "d", "c",
                                             "c", "c", "c")
               dots <- list(~stringr::str_to_lower(med))
               nm <- "med"
           },

           mpp = {
               col.raw <- c(raw.names$id, "MPP (which generated order)")
               col.names <- c(pt.id, "mpp")
               col.types <- readr::cols("c", "c")
           },

           mrn = {
               col.raw <- c("MRN- Organization", "Person ID")
               col.names <- c("mrn", "person.id")
               col.types <- readr::cols("c", "c")
           },

           order_by = {
               col.raw <- c(raw.names$id,
                            "Order Catalog Mnemonic",
                            "Person Location- Nurse Unit (Order)",
                            "Action Provider",
                            "Action Provider Position",
                            "Order Action Date & Time",
                            "Action Type",
                            "Action Personnel",
                            "Action Personnel Position",
                            "Action Communication Type")
               col.names <- c(pt.id,
                              "order",
                              "order.unit",
                              "provider",
                              "provider.role",
                              "action.datetime",
                              "action.type",
                              "action.provider",
                              "action.provider.role",
                              "action.comm")
               col.types <- readr::cols("c", "c", "c", "c", "c", col_dt, "c",
                                        "c", "c", "c")
           },

           order_detail = {
               col.raw <- c(raw.names$id,
                            "Source Order ID",
                            "Order Catalog Mnemonic",
                            "Ingredient Catalog Short Desc",
                            "Dose Strength - Order",
                            "Dose Strength - Unit",
                            "Dose Freetext",
                            "Person Location- Nurse Unit (Order)",
                            "Order Action Date & Time",
                            "Action Type")
               col.names <- c(pt.id,
                              "order.id",
                              "order",
                              "ingredient",
                              "ingredient.dose",
                              "ingredient.unit",
                              "ingredient.freetext",
                              "order.unit",
                              "action.datetime",
                              "action.type")
               col.types <- readr::cols("c", "c", "c", "c", "d", "c", "c", "c",
                                        col_dt, "c")
           },

           patients = {
               col.raw <- c(raw.names$id,
                            "Discharge Date & Time",
                            "Age- Years (Visit)",
                            "Person Location- Facility (Curr)",
                            "Encounter Type")
               col.names <- c(pt.id,
                              "discharge.datetime",
                              "age",
                              "facility",
                              "visit.type")
               col.types <- readr::cols("c", col_dt, "i", "c", "c")
           },

           problems = {
               col.raw <- c(raw.names$id, "Problem - Description",
                            "Problem Classification",
                            "Problem Confirmation Status",
                            "Problem Free Text", "Problem Severity",
                            "Problem Source Active Indicator",
                            "Problem Onset Date & Time",
                            "Problem Life Cycle Date & Time",
                            "Problem Life Cycle")
               col.names <- c(pt.id, "problem", "classification", "confirm",
                              "free.text", "severity", "active",
                              "onset.datetime", "life.cycle",
                              "life.cycle.datetime")
               col.types <- readr::cols("c", "c", "c", "c", "c", "c", "c",
                                        col_dt, col_dt, "c")
           },

           procedures9 = {
               col.raw <- c(raw.names$id,
                            "Procedure Date and Time",
                            "ICD9 Procedure Code")
               col.names <- c(pt.id,
                              "proc.date",
                              "proc.code")
               col.types <- readr::cols("c", col_dt, "c")
           },

           procedures10 = {
               col.raw <- c(raw.names$id,
                            "Procedure Date and Time",
                            "ICD10 Procedure Code")
               col.names <- c(pt.id,
                              "proc.date",
                              "proc.code")
               col.types <- readr::cols("c", col_dt, "c")
           },

           radiology = {
               col.raw <- c(raw.names$id,
                            raw.names$dt,
                            raw.names$ev)
               col.names <- c(pt.id,
                              "rad.datetime",
                              "rad.type")
               col.types <- readr::cols("c", col_dt, "c")
           },

           services = {
               col.raw <- c(raw.names$id,
                            "Medical Service Begin Date & Time",
                            "Medical Service End Date & Time",
                            "Medical Service",
                            "Previous Medical Service")
               col.names <- c(pt.id,
                              "start.datetime",
                              "end.datetime",
                              "service",
                              "service.from")
               col.types <- readr::cols("c", col_dt, col_dt, "c", "c")
               dots <- list(~dplyr::na_if(service, ""),
                            ~dplyr::na_if(service.from, ""))
               nm <- list("service", "service.from")
           },

           surgeries = {
               col.raw <- c(raw.names$id,
                            "Priority",
                            "ASA Class Description",
                            "Add On Indicator",
                            "Procedure",
                            "Primary Procedure Indicator",
                            "Start Date/Time",
                            "Stop Date/Time")
               col.names <- c(pt.id,
                              "priority",
                              "asa.class",
                              "add.on",
                              "surgery",
                              "primary.proc",
                              "surg.start.datetime",
                              "surg.stop.datetime")
               col.types <- readr::cols_only("c", "c", "c", "c", "c", "c",
                                             col_dt, col_dt)
               dots <- list(~ifelse(add.on == 1, TRUE, FALSE),
                            ~ifelse(primary.proc == 1, TRUE, FALSE))
               nm <- c("add.on", "primary.proc")
           },

           uop = {
               # use default columns
               col.names <- c(pt.id,
                              "uop.datetime",
                              "uop.event",
                              "uop.result")
               dots <- list(~stringr::str_to_lower(uop.event))
               nm <- "uop.event"
           },

           vent_settings = {
               # use default columns
               col.names <- c(pt.id,
                              "vent.datetime",
                              "vent.event",
                              "vent.result")
               dots <- list(~stringr::str_to_lower(vent.event))
               nm <- "vent.event"
           },

           vent_start = {
               col.raw <- c(raw.names$id,
                            "Clinical Event Date Result",
                            raw.names$ev)
               col.names <- c(pt.id,
                              "vent.datetime",
                              "vent.event")
               col.types <- readr::cols("c", col_dt, "c")
               dots <- list(~stringr::str_to_lower(vent.event))
               nm <- "vent.event"
           },

           visits = {
               col.raw <- c(raw.names$id,
                            "Arrival Date & Time",
                            "Admit Date & Time",
                            "Discharge Date & Time",
                            "Encounter Type",
                            "Admit Source",
                            "Admit Type",
                            "Person Location- Facility (Curr)",
                            "Person Location- Nurse Unit (Admit)")
               col.names <- c(pt.id,
                              "arrival.datetime",
                              "admit.datetime",
                              "discharge.datetime",
                              "visit.type",
                              "admit.source",
                              "admit.type",
                              "facility",
                              "nurse.unit.admit")
               col.types <- readr::cols("c", col_dt, col_dt, col_dt, "c", "c",
                                        "c", "c", "c")
           },

           vitals = {
               # use default columns
               col.names <- c(pt.id,
                              "vital.datetime",
                              "vital",
                              "vital.result")
               dots <- list(~stringr::str_to_lower(vital))
               nm <- "vital"
           },

           warfarin = {
               # use default columns
               col.names <- c(pt.id,
                              "warfarin.datetime",
                              "warfarin.event",
                              "warfarin.result")
               dots <- list(~stringr::str_to_lower(warfarin.event))
               nm <- "warfarin.event"
           },

           stop("Invalid type")

    )

    # sets column names to be read in; read all matching files into a single
    # data_frame
    col.types$cols <- purrr::set_names(col.types$cols, col.raw)
    read <- purrr::map_df(raw, readr::read_csv, col_types = col.types)

    # convert vector of columns to a list of symbols which work with dplyr, then
    # rename columns to more convenient names
    col.raw <- purrr::map(col.raw, as.symbol)
    read <- dplyr::rename_(read, .dots = purrr::set_names(col.raw, col.names))

    # indicator to remove duplicates; defaults to TRUE but use FALSE for faster
    # read times with large data sets
    if (check.distinct == TRUE) {
        read <- dplyr::distinct_(read, .keep_all = TRUE)
    }

    # perform any necessary data mutations
    if (!is.null(dots)) {
        read <- dplyr::mutate_(read, .dots = purrr::set_names(dots, nm))
    }

    # limit to only patients included in study
    if (!is.null(include.pts)) {
        read <- dplyr::semi_join(read, include.pts, by = "pie.id")
    }

    # set class of object for future function calls
    class(read) <- c(type, class(read))

    read
}
