#' Standardized Functions for Working with Data from the EDW
#'
#' \pkg{edwr} provides functions to facilitate working with data extracted from
#' the EDW
#'
#' This package provides standardized functions for reading, tidying, and
#' transforming data which was extracted from the Enterprise Data Warehouse
#' (EDW) using the standardized EDW queries.
#'
#' The main functions include:
#' \enumerate{
#'   \item \code{\link{read_data}} for reading data from csv files
#'   \item \code{\link{tidy_data}} for transforming data into a tidy data set
#'   \item \code{\link{summarize_data}} for aggregating and summarizing data
#' }
#'
#' @docType package
#' @name edwr
#' @importFrom magrittr %>%
#' @importFrom dplyr summarize
#' @importFrom dplyr summarize_
#' @importFrom purrr set_names
#' @importFrom rlang :=
#' @importFrom rlang enquo
#' @importFrom rlang parse_expr
#' @importFrom rlang quo
#' @importFrom rlang quos
#' @importFrom rlang sym
NULL