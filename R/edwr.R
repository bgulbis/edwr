#' Standardized Functions for Working with Data from the EDW
#'
#' \pkg{edwr} provides functions to facilitate working with data extracted from
#' the EDW
#'
#' This package provides standardized functions for reading, tidying, and
#' transforming data which was extracted from the Enterprise Data Warehouse
#' (EDW) using the standardized EDW queries.
#'
#' The main functions include: \code{\link{read_data}} for reading data from
#' csv files, and \code{\link{tidy_data}} for transforming the data into a tidy
#' data set.
#'
#' @docType package
#' @name edwr
#' @importFrom magrittr %>%
#' @importFrom dplyr arrange_
#' @importFrom dplyr filter_
#' @importFrom dplyr mutate_
#' @importFrom dplyr rename_
#' @importFrom dplyr select_
#' @importFrom dplyr summarise_
#' @importFrom dplyr anti_join
#' @importFrom dplyr inner_join
#' @importFrom dplyr full_join
#' @importFrom dplyr semi_join
#' @importFrom purrr set_names
NULL