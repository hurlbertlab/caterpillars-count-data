# This script is the workflow for identifying individual survey branches that have not been id'ed by site managers where Plant Species name is blank (NA). In those cases, the branch is potentially identifiable to family, genus, or species either because
#  1) users entered a plant species name while conducting a survey, or
#  2) arthropod photos from that survey branch might allow the inference of plant species

library(dplyr)
library(stringr)

source('plantSpecies/cleanNamesThruITIS.r')

# 1. Read in existing table of inferred names and Official Plant List
inferredNamesFiles = list.files("plantSpecies")[grepl("inferredPlantNames", list.files("plantSpecies"))]
inferredNames = read.csv(paste0("plantSpecies/", inferredNamesFiles[length(inferredNamesFiles)]))

officialPlantFiles = list.files("plantSpecies")[grepl("officialPlantList", list.files("plantSpecies"))]
officialPlantList = read.csv(paste0("plantSpecies/", officialPlantFiles[length(officialPlantFiles)]))


# 2. Read raw data files
# This is necessary because PhotoURL and user-specified plant names are in 'surveys' and 'ArthropodSightings' tables
sites = read.csv(list.files()[grepl("Site.csv", list.files())], header = TRUE, stringsAsFactors = FALSE)

surveys = read.csv(list.files()[grepl("Survey.csv", list.files())], header = TRUE, stringsAsFactors = FALSE)

plants = read.csv(list.files()[grepl("Plant.csv", list.files())], header = TRUE, stringsAsFactors = FALSE)

ArthropodSighting = read.csv(list.files()[grepl("ArthropodSighting.csv", list.files())], header = TRUE, stringsAsFactors = FALSE)


# 3. Filtering plants to where Species is N/A. These include species which we may have evaluated previously, but it's possible that new photos or new user-entered names have been added since, so we take all of it.
unidentifiedBranches <- plants %>% 
  filter(Species == "N/A") 


# 4. Filtering surveys to find where PlantSpecies has a name entered by a user at least once
userIdentifiedBranches <- surveys %>%
  filter(!PlantSpecies %in% c("N/A","","Hello","Dvt","Dvz","Dvt","N/a","Tree","Unknown","Unknown, will take picture")) %>%
  select(UserFKOfObserver, PlantSpecies, PlantFK) %>%
  left_join(unidentifiedBranches, by = c('PlantFK' = 'ID')) %>%
  filter(Species == "N/A") %>%
  group_by(PlantFK) %>%
  summarize(PlantSpecies = paste(PlantSpecies, collapse = ", ")) %>%
  left_join(plants, by = c('PlantFK' = 'ID')) %>%
  left_join(surveys, by = c('PlantFK', 'PlantSpecies')) %>%
  left_join(sites, by = c('SiteFK' = 'ID')) %>%
  filter(Name != "Example Site") %>%
  select(Name, Region, PlantFK, PlantSpecies) %>%
  rename('UserSuggestedName' = 'PlantSpecies')

# 5. All survey branches without a Species name where an arthropod photo has been taken
allBranchesWithPhotos <- surveys %>%
  filter(ObservationMethod == "Visual") %>% 
  left_join(ArthropodSighting, by = c('ID' = 'SurveyFK')) %>%
  left_join(plants, by = c('PlantFK' = 'ID')) %>%
  filter(Species == "N/A") %>%
  left_join(sites, by = c('SiteFK' = 'ID')) %>%
  filter(PhotoURL != "",
         Name != "Example Site") %>% 
  group_by(PlantFK, Name, Region) %>%
  summarize(PhotoURL = paste(PhotoURL, collapse = ", ")) %>%
  select(Name, Region, PlantFK, PhotoURL)

# 6. Join to get one dataframe with both user-entered names as well as photos
plantsToIdentify = full_join(userIdentifiedBranches, allBranchesWithPhotos, by = c('Name', 'Region', 'PlantFK')) %>% 
  arrange(Name, PlantFK)

# 7. Identify new branches with either user-entered names or photos that have not been examined before.
# - If there is only a single unique UserSuggestedName, then make that the InferredName
# - If there is only one UserSuggestedName, NameConfidence = 2
# - If there is only a single unique UserSuggestedName and it occurs more than once, NameConfidence = 3

newPlantsToIdentify = plantsToIdentify %>%
  filter(!PlantFK %in% inferredNames$PlantFK) %>%
  rowwise() %>%
  mutate(InferredName = ifelse(length(unique(unlist(str_split(UserSuggestedName, ", ")))) == 1, 
                               unique(unlist(str_split(UserSuggestedName, ", "))), NA),
         NameConfidence = case_when(length(unlist(str_split(UserSuggestedName, ", "))) == 1 & !is.na(UserSuggestedName) ~ 2,
                                    length(unlist(str_split(UserSuggestedName, ", "))) > 1 & 
                                      length(unique(unlist(str_split(UserSuggestedName, ", ")))) == 1 ~ 3,
                                    .default = NA),
         Notes = NA,
         New = 'Y')
         
# 8. Check branches that have been previously examined for which NameConfidence < 3 to see whether there are new user-entered names or photos by comparing the number of characters in the UserSuggestedNames and PhotoURL fields (if new names or photos have been added, the number will be larger)
oldPlantsToIdentify = plantsToIdentify %>%
  filter(PlantFK %in% inferredNames$PlantFK) %>%
  mutate(ncharNamesNew = nchar(UserSuggestedName, keepNA = F),
         ncharPhotoNew = nchar(PhotoURL, keepNA = F)) %>%
  left_join(inferredNames, by = c('Name', 'Region', 'PlantFK')) %>%
  mutate(ncharNamesOld = nchar(UserSuggestedName.y, keepNA = F),
         ncharPhotoOld = nchar(PhotoURL.y, keepNA = F),
         UserSuggestedName = ifelse(ncharNamesNew > ncharNamesOld, UserSuggestedName.x, UserSuggestedName.y),
         PhotoURL = ifelse(ncharPhotoNew > ncharPhotoOld, PhotoURL.x, PhotoURL.y),
         New = ifelse((ncharNamesNew > ncharNamesOld | ncharPhotoNew > ncharPhotoOld) & NameConfidence < 3, 'Y', 'N')) %>%
  select(Name, Region, PlantFK, UserSuggestedName, PhotoURL, InferredName, NameConfidence, Notes, New)


# 9. Combine new and old plants into one dataframe
newInferredNames = rbind(oldPlantsToIdentify, newPlantsToIdentify)

# 10. Join in official sciName from Official Plant List, then write to file
inferredSciNames = left_join(newInferredNames, officialPlantList[, c('userPlantName', 'sciName')], 
                             by = c('InferredName' = 'userPlantName')) %>%
  rename(InferredSciName = sciName)

write.csv(inferredSciNames, paste("plantSpecies/inferredPlantNames_", Sys.Date(), ".csv", sep = ""), row.names = F)

# 11. Examine each record where New == 'Y' manually (e.g. in Excel), fill in the inferred name if there's agreement 
# (complete agreement is handled automatically, but in cases where e.g. 3 out of 4 UserSuggestedNames all agree
# or better, then perhaps NameConfidence = 2), and assign a confidence rating based on user agreement.
# 1 is the least confident meaning there is no clear consensus among user-entered names, 
# 2 could mean only one name ever entered, or that it is identifiable to genus but not species from photos,
#   or that there is 75-99% agreement.
# 3 is the most confident with all entries agreeing multiple times, or photos support id.


# 12. Re-read in manually edited file, and then find inferred names that did not match in the userPlantName field of officialPlantList, and attempt to match with ITIS.
inferredNamesFiles = list.files("plantSpecies")[grepl("inferredPlantNames", list.files("plantSpecies"))]
inferredSciNames = read.csv(paste0("plantSpecies/", inferredNamesFiles[length(inferredNamesFiles)]))

unmatchedNames = inferredSciNames$InferredName[!is.na(inferredSciNames$InferredName) & is.na(inferredSciNames$InferredSciName)]

if (length(unmatchedNames) > 0) {
  matchedNames = cleanNamesThruITIS(unmatchedNames)
  
  for (n in unmatchedNames) {
    inferredSciNames$InferredSciName[inferredSciNames$InferredName == n] = matchedNames$sciName[matchedNames$Species == n]
  }
}

# If the unmatched names refer to good taxonomic concepts, be sure to add them to the bottom of officialPlantList
matchedNames$notes = NA
matchedNames$isConifer = NA
matchedNames = cbind(data.frame(Species = unmatchedNames), matchedNames)
names(matchedNames)[1:2] = c('userPlantName', 'cleanedPlantName')

officialPlantList = rbind(officialPlantList, matchedNames)
write.csv(officialPlantList, paste("plantSpecies/officialPlantList", Sys.Date(), ".csv", sep = ""), row.names = F)

# If the unmatched names don't match in ITIS and you can figure out what they are referring to, modify the InferredName
# value for that record so that it will be recognized by ITIS (e.g. a more commonly used name, or the scientific name).
# Then re-run from #12 on.

# For any unmatched names that don't match in ITIS and you can't figure out what taxonomic entity they are referring to,
# simply leave InferredSciName as NA but add a note that the name is ambiguous.

write.csv(inferredSciNames, paste("plantSpecies/inferredPlantNames_", Sys.Date(), ".csv", sep = ""), row.names = F)
