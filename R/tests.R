###############################################################################
#
# Random tests done while working on the package. These are NOT unit tests.
#
###############################################################################

library("dplyr")
library("glue")
library("magrittr")
library("tibble")

# Load .R files to be tested
working_dir <- getwd()
files <- c("utils.R", "count_missing.R", "visualizing_missing.R")
for (file in files) {
    source(glue("{working_dir}/R/{file}"))
}

# Create a sample data frame that contains missing values
sample_data <- tibble::as_tibble(mtcars) %>%
    dplyr::mutate(car = rownames(mtcars)) %>%
    create_random_na(ignore_cols = c("car"), seed = 117)

# Take a look at the missing values in this table
sample_data_nas <- missing_by_col(sample_data)
total_missing <- num_missing(sample_data)
missing(sample_data)
