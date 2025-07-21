# Script for updating user and site level participation in published studies using CC! data
library(dplyr)
library(lubridate)



# (1) Filter the full dataset down to the set of years, sites, dates, etc specific to a given publication

# E.g., for Hurlbert et al. 2019
pubData = fullDataset %>%
  filter(Name %in% c("NC Botanical Garden", "Prairie Ridge Ecostation"),
         Year %in% 2015:2016,
         julianday %in% 130:213)


# (2) Run this function which will update 2 files:
# -- userPublicationInfo.csv, a table summarizing the number of surveys by user that were used in the publication
# -- sitePublicationInfo.csv, a table summarizing the number of surveys by site that were used in the publication

# pubID is the ID number corresponding to the publication being summarized (from the publications.csv file)

updatePublicationInfo = function(pubData, pubID) {
  
  newUserInfo = pubData %>%
    distinct(ID, UserFKOfObserver) %>%
    count(UserFKOfObserver) %>%
    rename(nSurveys = n) %>%
    mutate(pubID = pubID)
  
  newSiteInfo = pubData %>%
    distinct(ID, SiteFK) %>%
    count(SiteFK) %>%
    rename(nSurveys = n) %>%
    mutate(pubID = pubID)
  
  oldUserInfo = read.csv(list.files()[grepl("userPublicationInfo", list.files())])
  oldSiteInfo = read.csv(list.files()[grepl("sitePublicationInfo", list.files())])
  
  userInfo = rbind(oldUserInfo, newUserInfo)
  siteInfo = rbind(oldSiteInfo, newSiteInfo)
  
  write.csv(userInfo, paste0(Sys.Date(), "_userPublicationInfo.csv"), row.names = F)
  write.csv(siteInfo, paste0(Sys.Date(), "_sitePublicationInfo.csv"), row.names = F)
  
}