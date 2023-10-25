### Update CC data in repo to newest version from backups
### Download ArthropodSighting, Plant, Site, and Survey data tables
### Run function from within caterpillars-count-data directory

library(rvest)
library(xml2)
library(tidyverse)
library(lubridate)

updateCatCountData <- function(updateExpertNames = FALSE) {
  
  # Remove old data files (be sure to add any .csv files that shouldn't be deleted here)
  
  oldfiles <- data.frame(filename = list.files()) %>%
    filter(grepl(".csv", filename), 
           !filename %in% c("arthropod_length_weight_regressions.csv", 
                            "classified_expert_identifications.csv", 
                            "taxon_ranks.csv"))
  
  unlink(oldfiles$filename)
  
  # Download most recent files from Caterpillars Count backup site

  webpage_url <- "https://caterpillarscount.unc.edu/backups/"

  webpage <- xml2::read_html(webpage_url)


  links <- rvest::html_table(webpage)[[1]] %>% 
    tibble::as_tibble(.name_repair = "unique") %>%
    mutate(text_date = word(Name, sep = "_", 1),
         file_type = word(Name, sep = "_", 2),
         date = as.Date(text_date, format = "%Y-%m-%d"))

  recent_date = max(links$date, na.rm = TRUE)
  
  base_filenames = c("ArthropodQuizQuestions", "CachedResult", "CronJobStatus", "DisputedIdentification",
                     "Download", "ExpertIdentification", "ManagerRequest", "SiteUserPreset", "SiteUserValidation",
                     "TemporaryExpertIdentificationChangeLog", "VirtualSurveyScore", "ArthropodSighting", "Plant",
                     "Site", "Survey")
  filenames = paste0(recent_date, "_", base_filenames, ".csv")
  
  for(f in filenames) {
    download.file(paste0(webpage_url, f), f, timeout = 120) 
  }
  
  if (updateExpertNames) {
    
    source('expert_id_taxon_names.r')
    
    probNames = updateExpertClassification()
    
    return(probNames)
    
  }
  
}

