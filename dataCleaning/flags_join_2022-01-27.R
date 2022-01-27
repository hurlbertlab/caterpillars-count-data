library(tidyverse)

full_checked <- read_csv('C:/Git/caterpillars-analysis-public/data/flagged_dataset_2022-01-21.csv')

new_flagged <- read_csv('dataCleaning/flagged_dataset_2022-01-26.csv')

only_checked <- full_checked %>% 
  filter(!is.na(flags) & !is.na(actionTaken) | status == 'remove') %>% 
  mutate(LocalDate = as.Date(LocalDate, format = '%m/%d/%Y'))

unchecked <- new_flagged %>% 
  filter(!arthID %in% only_checked$arthID)

full_set <- bind_rows(only_checked, unchecked)

write_csv(full_set, 'dataCleaning/flagged_dataset_2022-01-27.csv')
