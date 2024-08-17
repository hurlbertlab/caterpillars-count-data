# Define function that takes a vector of species names and checks each one with ITIS, returning a dataframe with the name, sciName, itis_id, and rank.

# Load libraries
library(taxize)
library(stringr)

cleanNamesThruITIS = function(speciesList) {
  
  plantList = data.frame(Species = speciesList,
                         sciName = NA, 
                         genus = NA,
                         itis_id = NA,    
                         rank = NA)
  
  for (i in 1:nrow(plantList)) {
    
    print(paste(i, "of", nrow(plantList)))
    
    if (!is.na(speciesList[i]) & nchar(speciesList[i]) >= 3) {  # for names that are at least 3 characters and not NA, try to match
      
      hierarchy = classification(speciesList[i], db = 'itis', accepted = TRUE)[[1]]
      
      # class is logical if taxonomic name does not match any existing names
      if (!is.null(nrow(hierarchy))) {
        plantList$sciName[i] = hierarchy$name[nrow(hierarchy)]
        plantList$itis_id[i] = hierarchy$id[nrow(hierarchy)]
        plantList$rank[i] = hierarchy$rank[nrow(hierarchy)]
        plantList$genus[i] = ifelse(plantList$rank[i] == 'genus', plantList$sciName[i],
                                    ifelse(plantList$rank[i] == 'species', word(plantList$sciName[i], 1), NA))
        
      } # end if there's a match
    } # end if name should be searched
  } # end for loop
  return(plantList)
}
