###############################################################################
#
# utils.R - misc utility functions
#
###############################################################################

# Returns (num)/(total) as a character percentage
as_percent <- function(num, total, dig = 1) {
    paste0(round((num * 100) / total, dig), "%")
}

# Randomly select #[num_NAs] indicies from [vector] and turn them into NAs
create_random_na_vector <- function(vector, num_NAs) {
    na_indicies <- sample(seq(length(vector)), num_NAs)
    vector[seq(length(vector)) %in% na_indicies] <- NA
    vector
}

# Given a dataframe [data], create a random number of NA values in each
# column not in [ignore_cols] with proportions ranging from the beginning
# value of [prop_na_range] to the last value of it
create_random_na <- function(data, prop_na_range = c(.25, .75),
                             ignore_cols = NULL, seed = NULL) {
    if (!is.null(seed)) {
        set.seed(seed)
    }
    data <- data
    cols <- names(data)
    if (!is.null(ignore_cols))
        cols <- cols[!cols %in% ignore_cols]
    col_props <- round(
        stats::runif(length(cols), prop_na_range[1],
              prop_na_range[2]) * nrow(data), 1)
    for (i in seq_along(cols)) {
        data[[cols[i]]] <- create_random_na_vector(
            data[[cols[i]]], col_props[i])
    }

    data
}

#' Pipe operator
#'
#' See \code{\link[magrittr]{\%>\%}} for more details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL
