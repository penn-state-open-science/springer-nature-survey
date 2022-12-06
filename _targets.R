# _targets.R

library(targets)
source("R/functions.R")
tar_option_set(packages = c("readxl", "dplyr"))
list(
  tar_target(raw_xlsx_file, "data/survey.xlsx", format = "file"),
  tar_target(survey, read_data(raw_xlsx_file)),
  tar_target(my_hist, plot_hist(cleaned_survey)),
  tar_target(cleaned_cols, rename_survey_cols(survey)),
  tar_target(cleaned_survey, clean_survey_all(survey)),
  tar_target(cleaned_saved, save_cleaned(cleaned_survey, "data/survey_clean.csv"))
)