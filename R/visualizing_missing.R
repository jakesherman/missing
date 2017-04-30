###############################################################################
#
# visualizing_missing.R - visualizing missing data
#
# Dependencies: dplyr, magrittr, pander
#
###############################################################################

#' Provides a summary of the missing values in a data frame
#'
#' Given an object, prints out a summary table describing the number and
#' percent of missing data (NAs), both overall and by column (for data.frames,
#' matricies) or element (for lists).
#'
#' @param data A data frame.
#' @param ... Specification of variables to consider while dropping rows.
#'   If empty, consider all variables. Use bare variable names. Select all
#'    variables between x and z with \code{x:z}, exclude y with \code{-y}.
#'    For more options, see the \link[dplyr]{select} documentation.
#' @seealso \code{\link{missing_}} for a version that uses regular evaluation
#'   and is suitable for programming with.
#' @export
#' @examples
#'
#' Looking at the number of missing values in the mtcars dataset:
#' missing(mtcars)
missing <- function(data, ...) {
    relevant_cols <- unname(dplyr::select_vars(colnames(data), ...))
    missing_(data, relevant_cols)
}

#' Standard-evaluation version of \code{missing}.
#'
#' This is a S3 generic.
#'
#' @param data A data frame.
#' @param vars Character vector of variable names. If empty, all
#'    variables are considered while dropping rows.
#' @keywords internal
#' @export
missing_ <- function(data, vars) {
    UseMethod("missing_")
}

#' @export
missing_.data.frame <- function(data, vars) {

    # Determine if we are looking at a subset of data
    if (!is.character(vars)) {
        stop("`vars` is not a character vector.", call. = FALSE)
    } else if (length(vars) > 0) {
        data <- data %>%
            dplyr::select_(.dots = vars)
    }

    # Get metadata on data
    cols <- colnames(data)
    nrows <- nrow(data)
    ncols <- ncol(data)
    all_NAs <- sum(is.na(data))
    percent_NA <- as_percent(all_NAs, nrows * ncols)

    # Get the number of NAs by column
    col_NAs <- colSums(is.na(data))
    col_Non_NAs <- nrows - col_NAs
    percent_NAs <- paste0(round((col_NAs * 100) / nrows, 1), "%")

    # Initialize the table
    NA_table <- tibble(
        Column = cols,
        NAs = col_NAs,
        NonNAs = col_Non_NAs,
        PercentNA = percent_NAs
    )

    # Print the output
    cat("Source: data frame [", nrows, "x", ncols, "]\n")
    cat(percent_NA, "of all cells are NAs.\n")
    pander::pander(NA_table)
}
