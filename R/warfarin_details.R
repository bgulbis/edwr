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
#' @examples
#' print(head(
#'   make_inr_ranges(warfarin)
#' ))
#'
#' @export
make_inr_ranges <- function(x) {
    warfarin.event <- sym("warfarin.event")
    warfarin.result <- sym("warfarin.result")

    # make sure we are only working with INR range data, remove and empty values
    tidy <- x %>%
        filter(
            !!warfarin.event == "inr range",
            !!warfarin.result != ""
        )

    fix_ranges <- function(y, z) {
        tidy <<- tidy %>%
            dplyr::mutate_at(
                "warfarin.result",
                stringr::str_replace_all,
                pattern = stringr::regex(y, ignore_case = TRUE),
                replacement = z
            )
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
    df <- tidy %>%
        tidyr::extract(
            col = !!warfarin.result,
            into = c("goal.low", "goal.high"),
            regex = "([0-9\\.]+ ?)-( ?[0-9\\.]+)",
            remove = FALSE,
            convert = TRUE
        ) %>%
        dplyr::mutate_at(
            c("goal.low", "goal.high"),
            fix_div,
            n = 100
        ) %>%
        dplyr::mutate_at(
            c("goal.low", "goal.high"),
            fix_div,
            n = 10
        ) %>%
        select(-!!warfarin.result, -!!warfarin.event)

    reclass(x, df)
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
#' @examples
#' print(head(
#'   make_indications(warfarin)
#' ))
#'
#' @export
make_indications <- function(x) {
    warfarin.event <- sym("warfarin.event")
    warfarin.result <- sym("warfarin.result")

    # make sure we are only working with warfarin indication data, remove and
    # empty values
    df <- x %>%
        filter(
            !!warfarin.event == "warfarin indication",
            !!warfarin.result != ""
        ) %>%

    # substitute an alternate string for standard DVT and PE strings, at
    # facilitate identifying other types of thrombosis
        dplyr::mutate_at(
            "warfarin.result",
            stringr::str_replace_all,
            pattern = "Deep vein thrombosis",
            replacement = "D-V-T"
        ) %>%
        dplyr::mutate_at(
            "warfarin.result",
            stringr::str_replace_all,
            pattern = "Pulmonary embolism",
            replacement = "P-E"
        ) %>%
        mutate(
            !!"afib" := stringr::str_detect(
                !!warfarin.result,
                "Atrial fibrillation|a(.*)?fib|a(.*)?flutter"
            ),
            !!"dvt" := stringr::str_detect(
                !!warfarin.result,
                "D-V-T|DVT(?!( prophylaxis))|VTE"
            ),
            !!"pe" := stringr::str_detect(
                !!warfarin.result,
                "P-E|PE"
            ),
            !!"valve" := stringr::str_detect(
                !!warfarin.result,
                "Heart valve \\(Mech/porc/bioprost\\)|valve|avr|mvr"
            ),
            !!"stroke" := stringr::str_detect(
                !!warfarin.result,
                "st(ro|or)ke|cva|ica|mca"
            ),
            !!"vad" := stringr::str_detect(
                !!warfarin.result,
                "vad|hm[ ]?ii|heart( )?mate|heartware|syncardia|total artificial heart|tah"
            ),
            !!"thrombus" := stringr::str_detect(
                !!warfarin.result,
                "throm|clot|emboli|occl"
            ),
            !!"hypercoag" := stringr::str_detect(
                !!warfarin.result,
                "malig|anti(.)?phos|lupus|apla|hypercoag|deficien|leiden|fvl|factor v"
            ),
            !!"prophylaxis" := stringr::str_detect(
                !!warfarin.result,
                "prophylax"
            )
        ) %>%

        # if none of the other indications were found, use "other"
        mutate(
            !!"other" := sum(
                !!sym("afib"),
                !!sym("dvt"),
                !!sym("pe"),
                !!sym("valve"),
                !!sym("stroke"),
                !!sym("vad"),
                !!sym("thrombus"),
                !!sym("hypercoag"),
                !!sym("prophylaxis"),
                na.rm = TRUE
            ) == 0
        ) %>%
        select(-!!warfarin.result, -!!warfarin.event)

    reclass(x, df)
}
