# warfarin_details.R

#' Make INR therapeutic ranges
#'
#' \code{make_inr_ranges} tidy INR ranges data
#'
#' This function takes a data frame with the goal INR range as input into the
#' EMR and returns a tidy data frame with the low and high goal values. It
#' tidies up string errors (typos, incorrect ranges, etc.) before returning the
#' values.
#'
#' @param x A data frame with INR ranges
#'
#' @return A data frame
#'
#' @export
make_inr_ranges <- function(x) {
    # make sure we are only working with INR range data, remove and empty values
    tidy <- dplyr::filter_(x, .dots = list(~warfarin.event == "inr range",
                                           ~warfarin.result != ""))

    fix_ranges <- function(y, z) {
        tidy <<- purrr::dmap_at(.d = tidy,
                                .at = "warfarin.result",
                                .f = stringr::str_replace_all,
                                pattern = stringr::regex(y, ignore_case = TRUE),
                                replacement = z)
    }

    find <- c("(INR|Goal)|-\\.|\\(.*\\)|=",
              "\\.\\.",
              "--|to|/",
              "[0-9\\.]+( )[0-9\\.]+",
              "[1-9\\.]+([0])[1-9\\.]+",
              "(>|above|greater[ than]?)[ ]?([0-9\\.]+)",
              "(<|below|less[ than]?)[ ]?([0-9\\.]+)",
              "^1.[5-9]$",
              "^2$",
              "^2.[1-4]$",
              "^2.5$",
              "^2.[6-9]$",
              "^3$",
              "^3.5$")

    replace <- c("", ".", "-", "-", "-", "\\2-3.5", "1.5-\\2",
                 "1.5-2", "1.5-2.5", "2-2.5", "2-3", "2.5-3", "2.5-3.5", "3-4")

    # perform string replacements to clean up inr ranges
    purrr::walk2(.x = find, .y = replace, .f = fix_ranges)

    # correct any goals like "200", which should be "2.0", or "25" = "2.5"
    fix_div <- function(y, n) {
        purrr::as_vector(purrr::map_if(y, ~ !is.na(.x) && .x >= n, ~ .x / n))
    }

    # separate the inr range into two columns, goal low and high
    tidy <- tidyr::extract_(data = tidy, col = "warfarin.result",
                            into = c("goal.low", "goal.high"),
                            regex = "([0-9\\.]+ ?)-( ?[0-9\\.]+)",
                            remove = FALSE, convert = TRUE) %>%
        purrr::dmap_at(.at = c("goal.low", "goal.high"), .f = fix_div, n = 100) %>%
        purrr::dmap_at(.at = c("goal.low", "goal.high"), .f = fix_div, n = 10) %>%
        dplyr::select_(.dots = list(quote(-warfarin.result),
                                    quote(-warfarin.event)))

    # keep original class
    class(tidy) <- class(x)
    tidy
}

#' Make warfarin indications
#'
#' \code{make_indications} tidy warfarin indication data
#'
#' This function takes a data frame with the raw warfarin indication as input
#' into the EMR and returns a tidy data frame with the indication categorized.
#' It tidies up non-standard indications before returning the values.
#'
#' @param x A data frame with warfarin indications
#'
#' @return A data frame
#'
#' @export
make_indications <- function(x) {
    # make sure we are only working with warfarin indication data, remove and
    # empty values
    tidy <- dplyr::filter_(x, .dots = list(
        ~warfarin.event == "warfarin indication",
        ~warfarin.result != ""
    ))

    # detect the matching pattern
    find_string <- function(x) {
        lazyeval::interp(
            ~stringr::str_detect(
                tidy$warfarin.result,
                stringr::regex(y, ignore_case = TRUE)),
            y = x)
    }

    # patterns to use for each indication
    find <- c("Atrial fibrillation|a(.*)?fib|a(.*)?flutter",
              "D-V-T|DVT(?!( prophylaxis))|VTE",
              "P-E|PE",
              "Heart valve \\(Mech/porc/bioprost\\)|valve|avr|mvr",
              "st(ro|or)ke|cva|ica|mca",
              "vad|hm[ ]?ii|heart( )?mate|heartware|syncardia|total artificial heart|tah",
              "throm|clot|emboli|occl",
              "malig|anti(.)?phos|lupus|apla|hypercoag|deficien|leiden|fvl|factor v",
              "prophylax")


    # substitute an alternate string for standard DVT and PE strings, at
    # facilitate identifying other types of thrombosis
    tidy <- purrr::dmap_at(.d = tidy,
                           .at = "warfarin.result",
                           .f = stringr::str_replace_all,
                           pattern = "Deep vein thrombosis",
                           replacement = "D-V-T") %>%
        purrr::dmap_at(.at = "warfarin.result",
                       .f = stringr::str_replace_all,
                       pattern = "Pulmonary embolism",
                       replacement = "P-E") %>%
        dplyr::mutate_(.dots = purrr::set_names(
            x = purrr::map(find, find_string),
            nm = c("afib", "dvt", "pe", "valve", "stroke", "vad", "thrombus",
                   "hypercoag", "prophylaxis")
        )) %>%
        # if none of the other indications were found, use "other"
        dplyr::mutate_(.dots = purrr::set_names(
            x = list(~dplyr::if_else(afib == FALSE &
                                         dvt == FALSE &
                                         pe == FALSE &
                                         valve == FALSE &
                                         stroke == FALSE &
                                         vad == FALSE &
                                         thrombus == FALSE &
                                         hypercoag == FALSE &
                                         prophylaxis == FALSE,
                                     TRUE, FALSE)),
            nm = "other"
        )) %>%
        dplyr::select_(.dots = list(quote(-warfarin.result),
                                    quote(-warfarin.event)))

    # keep original class
    class(tidy) <- class(x)
    tidy
}
