# Workflow that 1) checks for any new plant species names not on our official plant species list, 2) matches those names with ITIS, 3) flags names that do not match for manual inspection and correction, and 4) updates official plant species list with new user-inputted names.

# NOTE: Still need a way to efficiently deal with ambiguous common names that could refer to different taxonomic concepts depending on where you are, e.g. "scrub oak", "ironwood", etc.


## VARIABLE NAME KEY ##

# userPlantName = the user inputted name for a plant (might be the scientific name, different common names, a genus, name with typos, etc.)
# cleanedPlantName = a version of the plant name that contains only the scientific name, genus or common name without "spp.", "?", etc.
# sciName = the official Integrated Taxonomic Information System (ITIS, itis.gov) recognized scientific name (rarely may be a name not recognized by ITIS, but by some other )
# ITIS_ID = the taxon ID number from ITIS 


# Load libraries
library(dplyr)
library(taxize)
library(stringr)

source('plantSpecies/cleanNamesThruITIS.r')

## WORKFLOW ##

# 1. Read in latest Plants.csv file from the caterpillars-count-data repo
plants = read.csv(list.files()[grepl("Plant.csv", list.files())], header = TRUE, stringsAsFactors = FALSE)


# 2. Read in latest Official Plant List
officialPlantListFiles = list.files('plantSpecies')[str_detect(list.files('plantSpecies'), 'officialPlantList')]
mostRecentOfficialPlantList = officialPlantListFiles[length(officialPlantListFiles)]
officialPlantList = read.csv(paste0('plantSpecies/', mostRecentOfficialPlantList), header = T)


# 3. Find new names not in userPlantName of officialPLantList
# Below isn't a true representation of new species bc the names are not "clean" they still have spp., etc.
new_species <- plants %>% 
  rename(userPlantName = Species) %>%
  distinct(userPlantName) %>%
  # select rerun sciName entries that are NOT (!) in sciName from cleaned list
  filter(!userPlantName %in% officialPlantList$userPlantName) 

# IF THERE ARE NEW SPECIES (nrow(new_species) > 0), MOVE FORWARD. 

# OTHERWISE STOP -- NO NEW NAMES TO CLEAN.

write.csv(new_species, paste("plantSpecies/newSpecies_", Sys.Date(), ".csv", sep = ""), row.names = F)


# 4. Run new entries through ITIS with custom function.
#    NOTE: This function is interactive and may request user input to select among naming options.

cleanedNewNames = cleanNamesThruITIS(new_species$userPlantName)


# 5. Separate out results that did match in ITIS, and append the matched results to officialPlantList and save file.
#    (rename "Species" as "userPlantName", add notes, isConifer, and cleanedPlantName = userPlantName)

matched_new_species <- filter(cleanedNewNames, !is.na(cleanedNewNames$itis_id)) %>%
  rename(userPlantName = Species) %>%
  mutate(cleanedPlantName = userPlantName,
         isConifer = NA,
         notes= NA) %>%
  select(userPlantName, cleanedPlantName, sciName, genus, itis_id, rank, notes, isConifer)

officialPlantList <- rbind(officialPlantList, matched_new_species)


# 6.  For results that didn't match in ITIS, write to a file and examine manually in Excel (as .csv)
#     If no results that didn't match, skip to #11.

unmatched_new_species <- filter(cleanedNewNames, is.na(cleanedNewNames$itis_id)) %>% 
  rename(userPlantName = Species) %>%
  mutate(cleanedPlantName = NA,
         isConifer = NA,
         notes= NA) %>%
  select(userPlantName, cleanedPlantName, sciName, genus, itis_id, rank, notes, isConifer)

write.csv(unmatched_new_species, paste0("plantSpecies/unmatched_new_species_", Sys.Date(), ".csv"), row.names = F)    


# 7. Manually go through all rows in unmatched_new_species...csv file and fill in a taxonomically valid cleanedPlantName.
# -- Use resolver.globalnames.org to find potential synonymns or other authorities that recognize the name (put in notes).
# -- After manually fixing all entries, then read in .csv as a dataframe which will have the original userPlantName and a new cleanedPlantName.
# -- If research does not yield a valid taxonomic name, leave cleanedPlantName as NA.

listOfUnmatchedFiles = list.files('plantSpecies')[str_detect(list.files('plantSpecies'), '^unmatched_new_species')]
mostRecentUnmatchedFile = listOfUnmatchedFiles[length(listOfUnmatchedFiles)]

manually_matched_new_species <- read.csv(paste0('plantSpecies/', mostRecentUnmatchedFile))


# 8. Run the cleanedPlantName column of that dataframe through cleanNamesThruITIS(), rename "Species" as "cleanedPlantName" and join the results back to the original manually created dataframe that includes both userPlantName and cleanedPlantName by cleanedPlantName.

manually_matched_names_to_clean = manually_matched_new_species$cleanedPlantName[!is.na(manually_matched_new_species$cleanedPlantName)]

cleanedManuallyEnteredNames = cleanNamesThruITIS(unique(manually_matched_names_to_clean)) 

manuallyCleanedRecordsWithITIS = left_join(manually_matched_new_species[, c('userPlantName', 'cleanedPlantName')], 
                                           cleanedManuallyEnteredNames, by = c('cleanedPlantName' = 'Species')) %>%
  mutate(isConifer = NA,
         notes = NA) %>%
  select(userPlantName, cleanedPlantName, sciName, genus, itis_id, rank, notes, isConifer)


# 9. Manually examine names that still don't match and correct where possible. Remaining unmatched names will simply be NA.

# 10. Add all new userPlantName names to officialPlantList (after possibly running through the previous steps a couple of times)

officialPlantList = rbind(officialPlantList, manuallyCleanedRecordsWithITIS)


# 11. Write the updated officialPlantList to file
write.csv(officialPlantList, paste("plantSpecies/officialPlantList", Sys.Date(), ".csv", sep = ""), row.names = F)
