### Update CC data in repo to newest version from backups
### Download ArthropodSighting, Plant, Site, and Survey data tables
### Run function from within caterpillars-count-data directory

library(rvest)
library(xml2)
library(tidyverse)
library(lubridate)

updateCatCountData <- function() {
  
  # Remove old data files
  
  oldfiles <- data.frame(filename = list.files()) %>%
    filter(grepl("ArthropodSighting.csv", filename) | grepl("Site.csv", filename) | grepl("Plant.csv", filename) | grepl("Survey.csv", filename))
  
  unlink(oldfiles$filename)
  
  # Download most recent files from Caterpillars Count backup site

  webpage_url <- "https://caterpillarscount.unc.edu/backups/"

  webpage <- xml2::read_html(webpage_url)


  links <- rvest::html_table(webpage)[[1]] %>% 
    tibble::as_tibble(.name_repair = "unique") %>%
    mutate(text_date = word(Name, sep = "_", 1),
         file_type = word(Name, sep = "_", 2),
         date = as.Date(text_date, format = "%Y-%m-%d"))

  newest_data <- links %>%
    filter(grepl("Arthropod", file_type) | grepl("Site.csv", file_type) | grepl("Plant", file_type) | grepl("Survey", file_type)) %>%
    filter(date == max(date)) %>%
    mutate_at(c("Name"), ~ifelse(grepl("Arthropod", file_type), paste0(date, "_", "ArthropodSighting.csv"), Name)) %>%
    select(-Size) %>%
    distinct()

  download.file(paste0(webpage_url, newest_data$Name), newest_data$Name) 
  
  }

