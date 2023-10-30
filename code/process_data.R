# Author: Kurt Taylor

# Load data --------------------------------------------------------------------

# Source: https://covid19.sanger.ac.uk/lineages/raw?lineageView=1&lineages=B.1.1.7%2CB.1.617.2%2CB.1.1.529&colours=1%2C6%2C2
# N.B. Data includes B, Alpha, Delta and Omicron

data <- read.csv("raw/Estimated_Cases_in_England_2.csv")

data <- data %>%
  dplyr::mutate(date = as.Date(date),
         variant = ifelse(lineage == "B.1.1.7", "Alpha",
                          ifelse(lineage == "B.1.617.2", "Delta",
                                 ifelse(lineage == "B.1.1.529", "Omicron",
                                        ifelse(lineage == "B", "Wild type", NA))))) %>%
  # remove rows after Jan 2022
  dplyr::filter(date <= "2022-01-01") %>%
  dplyr::select(-c(lower,upper))

# ADD GOVERNMENT DATA FOR PRE-SEP-2020 ESTIMATES CASES -------------------------

# Source: https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=newCasesBySpecimenDate&format=csv
# N.B. new cases by specimen date in England

pre_sep_2020_dat <- read.csv("raw/nation_2022-05-17.csv")

pre_sep_2020_dat <- pre_sep_2020_dat %>%
  dplyr::mutate(date = as.Date(date)) %>%
  # start weeks on a Saturday like data above
  dplyr::filter(date >= "2020-02-01") %>%
  # make weeks
  dplyr::mutate(week = cut.Date(date, breaks = "7 days", labels = FALSE)) %>%
  # make weekly case variable
  dplyr::group_by(week) %>%
  dplyr::mutate(weekly_cases = sum(newCasesBySpecimenDate),
         date_week = min(date),
         variant = "Wild type",
         lineage = "unknown") %>%
  dplyr::ungroup() %>%
  dplyr::filter(date_week <= "2020-08-29")

# Format data so it can be rbinded to other data -------------------------------

pre_sep_2020_dat <- pre_sep_2020_dat %>%
  dplyr::distinct(week, .keep_all= TRUE) %>%
  dplyr::select(-c(areaCode, areaName, areaType, date, week, newCasesBySpecimenDate)) %>%
  dplyr::rename(value = weekly_cases, date = date_week) %>%
  # manually maniuplate other variants for consistency of data (assuming zero for all other variants in these data)
  dplyr::group_by(date) %>%
  dplyr::group_modify(~ dplyr::add_row(.x,.before=0)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(value = ifelse(is.na(value), 0, value),
         variant = ifelse(is.na(variant), "Alpha", variant),
         lineage = ifelse(is.na(lineage), "B.1.1.7", lineage)) %>%
  dplyr::group_by(date) %>%
  dplyr::group_modify(~ dplyr::add_row(.x,.before=0)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(value = ifelse(is.na(value), 0, value),
         variant = ifelse(is.na(variant), "Delta", variant),
         lineage = ifelse(is.na(lineage), "B.1.617.2", lineage)) %>%
  dplyr::group_by(date) %>%
  dplyr::group_modify(~ dplyr::add_row(.x,.before=0)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(value = ifelse(is.na(value), 0, value),
         variant = ifelse(is.na(variant), "Omicron", variant),
         lineage = ifelse(is.na(lineage), "B.1.1.529", lineage))

# Combine data -----------------------------------------------------------------

data <- rbind(data, pre_sep_2020_dat)

# Save data --------------------------------------------------------------------

write.csv(data, "data/variant_data.csv", row.names = FALSE)