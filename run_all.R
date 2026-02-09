# ==============================================================================
# Run All: Full cleaning pipeline + table generation
#
# Renders all Rmd files in order:
#   02_data_cleaning  ->  clean_data/data.RData
#   03_impute         ->  clean_data/data_imputed.RData
#   04_table1         ->  output/04_table1.html
#   05_table2         ->  output/05_table2.html
#   06_table3         ->  output/06_table3.html
# ==============================================================================

library(rmarkdown)

render("02_data_cleaning.Rmd", output_dir = "output", quiet = TRUE)

render("03_impute.Rmd", output_dir = "output", quiet = TRUE)

render("04_table1.Rmd", output_dir = "output", quiet = TRUE)

render("05_table2.Rmd", output_dir = "output", quiet = TRUE)

render("06_table3.Rmd", output_dir = "output", quiet = TRUE)
