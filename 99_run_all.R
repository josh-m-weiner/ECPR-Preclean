# ==============================================================================
# Run All: Full cleaning pipeline + table generation
#
# Renders all Rmd files in order:
#   02_data_cleaning  ->  clean_data/data.RData
#   03_impute         ->  clean_data/data_imputed.RData
#   04_p1_t1          ->  output/04_p1_t1.html
#   05_p1_t2          ->  output/05_p1_t2.html
#   06_p2_t1          ->  output/06_p2_t1.html
# ==============================================================================

library(rmarkdown)

timestamp <- format(Sys.time(), "%Y-%m-%d_%H%M")

render("02_data_cleaning.Rmd",
       output_file = paste0("Data cleaning log ", timestamp, ".html"),
       output_dir = "output", quiet = TRUE)

render("03_impute.Rmd",
       output_file = paste0("Imputing missing vars log ", timestamp, ".html"),
       output_dir = "output", quiet = TRUE)

render("04_p1_t1.Rmd",
       output_file = paste0("Paper 1, Table 1 ", timestamp, ".html"),
       output_dir = "output", quiet = TRUE)

render("05_p1_t2.Rmd",
       output_file = paste0("Paper 1, Table 2 ", timestamp, ".html"),
       output_dir = "output", quiet = TRUE)

render("06_p2_t1.Rmd",
       output_file = paste0("Paper 2, Table 1 ", timestamp, ".html"),
       output_dir = "output", quiet = TRUE)

