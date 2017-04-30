###############################################################################
#
# count_missing.R - counting missing data, asserting specific numbers of NAs
#
# Dependencies: magrittr, purrr, tibble
#
###############################################################################

# Internal function: select particular columns from a data frame
select_cols <- function(data, vars = NULL) {
    if (!is.null(vars) & !is.character(vars)) {
        stop("`vars` is not a character vector.", call. = FALSE)
    } else if (length(vars) > 0) {
        data <- data %>%
            dplyr::select_(.dots = vars)
    }
    data
}

# Interal function: create a table of missing data by column
missing_by_col <- function(data, vars = NULL) {

    # Choose columns to show
    data <- data %>%
        select_cols(vars)

    # Compute vectors
    cols <- colnames(data)
    nrows <- nrow(data)
    data_types <- data %>%
        purrr::map(class) %>%
        purrr::map_chr(~ paste(.x, collapse = ", "))
    col_NAs <- data %>%
        is.na() %>%
        colSums()
    col_non_NAs <- nrows - col_NAs
    percent_NAs <- col_NAs / nrows

    # Create the tibble
    tibble::tibble(
        Column = cols,
        DataType = data_types,
        NAs = col_NAs,
        NonNAs = col_non_NAs,
        PercentNA = percent_NAs
    )
}

# Internal function: number of NAs, by column potentially
num_missing <- function(data, vars = NULL) {
    data %>%
        missing_by_col(vars) %$%
        sum(NAs)
}
