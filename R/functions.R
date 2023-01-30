# R/functions.R

read_data <- function(fn) {
  if (!file.exists(fn)) {
    stop("File not found: '", fn, "'")
  } else {
    readxl::read_xlsx(fn)
  }
}

plot_hist <- function(df) {
  require(dplyr)
  require(ggplot2)
  
  df %>%
    group_by(., country) %>%
    summarise(., n_resp_from_country = n()) %>%
    arrange(., desc(n_resp_from_country)) %>%
    ggplot() +
    aes(x = n_resp_from_country) +
    geom_histogram(bins = 100)
}

make_cols_lowercase <- function(df) {
  names(df) <- tolower(names(df))
  df
}

rename_survey_cols <- function(survey) {
  survey <- make_cols_lowercase(survey)
  names(survey)[1] <- "job_title"
  names(survey)[3] <- "what_do_with_data"
  names(survey)[4] <- "generate_human_data"
  names(survey)[5] <- "file_size"
  names(survey)[6] <- "subject_area"
  names(survey)[7] <- "molecular_or_gene_data"
  names(survey)[8] <- "deposit_repo_always"
  names(survey)[9] <- "aware_should_deposit"
  names(survey)[10] <- "generate_other_types"
  names(survey)[11] <- "data_discoverable_import"
  names(survey)[12] <- "barrier_lack_of_time"
  names(survey)[13] <- "barrier_organizing_data"
  names(survey)[14] <- "barrier_which_repo"
  names(survey)[15] <- "barrier_copyright_licensing"
  names(survey)[16] <- "barrier_sharing_cost"
  names(survey)[17] <- "barrier_other"
  names(survey)[18] <- "interest_data_dep_service"
  names(survey)[19] <- "interest_data_discov_service"
  names(survey)[20] <- "have_data_to_deposit"
  names(survey)[21] <- "want_other_help"
  names(survey)[22] <- "want_help_with"
  names(survey)[23] <- "have_addtl_info"
  names(survey)[24] <- "addtl_info"
  survey
}

clean_what_do_with_data <- function(survey) {
  require(dplyr)
  
  survey %>%
    mutate(
      .,
      what_do_with_data = recode(
        what_do_with_data,
        "Submit files as supplementary information" = "submit_suppl"
      )
    ) %>%
    mutate(
      .,
      what_do_with_data = recode(what_do_with_data, "Deposit files in a repository" = "deposit_repo")
    ) %>%
    mutate(., what_do_with_data = tolower(what_do_with_data))
}

clean_generate_human_data <- function(survey) {
  require(dplyr)
  survey %>%
    mutate(., generate_human_data = as.logical(generate_human_data))
}

clean_file_size <- function(survey) {
  require(forcats)
  require(dplyr)
  
  ordered_file_sz <- forcats::fct_relevel(survey$file_size, c("<20Mb", "20Mb -100Mb", "100Mb - 1Gb", "1Gb - 5Gb", "5Gb - 20Gb", "20Gb - 50 Gb", ">50Gb"))
  
  survey %>%
    mutate(., file_size = ordered_file_sz)
}

clean_subject_area <- function(survey) {
  require(dplyr)
  require(stringr)
  
  survey %>%
    mutate(., subject_area = tolower(subject_area)) %>%
    mutate(., subject_area = stringr::str_replace(subject_area, " ", "_")) %>%
    mutate(., subject_area = dplyr::recode(subject_area, 
                                               "medical_sciences" = "med_sci")) %>%
    mutate(., subject_area = dplyr::recode(subject_area, 
                                               "biological_sciences" = "bio_sci")) %>%
    mutate(., subject_area = dplyr::recode(subject_area, 
                                               "earth_sciences" = "earth_sci")) %>%
    mutate(., subject_area = dplyr::recode(subject_area, 
                                               "other_sciences" = "oth_sci")) %>%
    mutate(., subject_area = dplyr::recode(subject_area, 
                                               "physical_sciences" = "phys_sci"))
}

clean_molecular_or_gene <- function(survey) {
  require(dplyr)
  survey %>%
    mutate(., molecular_or_gene_data = as.logical(molecular_or_gene_data))
}

clean_deposit_repo_always <- function(survey) {
  require(dplyr)
  survey %>%
    mutate(., deposit_repo_always = as.logical(deposit_repo_always))
}

clean_aware_should_deposit <- function(survey) {
  require(dplyr)
  survey %>%
    mutate(., aware_should_deposit = as.logical(aware_should_deposit))
}

clean_generate_other_types <- function(survey) {
  survey %>%
    mutate(., generate_other_types = as.logical(generate_other_types))
}

clean_barriers <- function(survey) {
  require(dplyr)
  require(stringr)
  
  lack_time <-
    stringr::str_detect(survey$barrier_lack_of_time, "Lack")
  org_data <-
    stringr::str_detect(survey$barrier_organizing_data, "Organising")
  dont_know_repo <-
    stringr::str_detect(survey$barrier_which_repo, "repository")
  copyright <-
    stringr::str_detect(survey$barrier_copyright_licensing, "copyright")
  costs <- stringr::str_detect(survey$barrier_sharing_cost, "Costs")
  
  survey %>%
    mutate(
      .,
      barrier_lack_of_time = if_else(lack_time, TRUE, FALSE, missing = FALSE),
      barrier_organizing_data = if_else(org_data, TRUE, FALSE, missing = FALSE),
      barrier_which_repo = if_else(dont_know_repo, TRUE, FALSE, missing = FALSE),
      barrier_copyright_licensing = if_else(copyright, TRUE, FALSE, missing = FALSE),
      barrier_sharing_cost = if_else(costs, TRUE, FALSE, missing = FALSE)
    )
}

clean_have_data_to_deposit <- function(survey) {
  require(dplyr)
  survey %>%
    mutate(., have_data_to_deposit = as.logical(have_data_to_deposit))
}

clean_want_other_help <- function(survey) {
  require(dplyr)
  survey %>%
    mutate(., want_other_help = as.logical(want_other_help))
}

clean_have_addtl_info <- function(survey) {
  require(dplyr)
  survey %>%
    mutate(., have_addtl_info = as.logical(have_addtl_info))
}

clean_survey_all <- function(survey) {
  survey %>%
    rename_survey_cols(.) %>%
    clean_what_do_with_data(.) %>%
    clean_generate_human_data(.) %>%
    clean_file_size(.) %>%
    clean_subject_area(.) %>%
    clean_molecular_or_gene(.) %>%
    clean_deposit_repo_always(.) %>%
    clean_aware_should_deposit(.) %>%
    clean_generate_other_types(.) %>%
    clean_barriers(.) %>%
    clean_have_data_to_deposit(.) %>%
    clean_want_other_help(.)
}

change_vars_to_logical <- function(data, vars) {
  require(dplyr)
  data %>% 
    mutate(., across({{vars}}, as.logical))
}

save_cleaned <- function(survey, fn) {
  require(readr)
  
  readr::write_csv(survey, "data/survey_clean.csv")
  message("Saved cleaned data file: '", fn, "'")
}

plot_by_sub_area <- function(data, var) {
  data %>%
    filter(., !is.na(subject_area), !is.na({{var}})) %>%
    ggplot() +
    aes(x = {{var}}, fill = subject_area) +
    facet_grid(rows = vars(subject_area)) +
    scale_x_continuous(breaks = 0:10) +
    theme(strip.text.y = element_blank()) +
    geom_bar()
}