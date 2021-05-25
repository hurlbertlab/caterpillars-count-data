# This script automatically matches plant species names provided by users


library(dplyr)
library(taxize)

sites = read.csv(list.files()[grep('Site.csv', list.files())], header = TRUE, stringsAsFactors = FALSE)
plants = read.csv(list.files()[grep('Plant.csv', list.files())], header = TRUE, stringsAsFactors = FALSE)
plantspp = data.frame(plantName = unique(plants$Species)) %>%
  mutate(plantName = as.character(plantName),
         cleanedPlantName = case_when(
           plantName == "Hydrangeas" ~ "Hydrangea",
           plantName == "Blueberry vaccinium sp." ~ "Vaccinium",
           plantName == "Box elder (acer negundo)" ~ "Acer negundo",
           plantName == "Boxelder maple" ~ "Acer negundo",
           plantName == "American witch-hazel" ~ "witch-hazel",
           plantName == "Burr oak" ~ "Bur oak",
           plantName == "Bush honeysuckle (caprifoliaceae family)" ~ "Bush honeysuckle",
           plantName == "California-laurel" ~ "California laurel",
           plantName == "Crab apple sp" ~ "Malus",
           plantName == "Eastern sweet shrub" ~ "Eastern sweetshrub",
           plantName == "Hop hornbeam" ~ "Hophornbeam",
           plantName == "Hop-hornbeam" ~ "Hophornbeam",
           plantName == "Pear tree" ~ "Pear",
           plantName == "Red osier dogwood" ~ "Redosier dogwood",
           plantName == "Red-osier dogwood" ~ "Redosier dogwood",
           plantName == "Red osier dogwood (cornus sericea)" ~ "Redosier dogwood",
           plantName == "Sweet gum" ~ "Sweetgum",
           plantName == "Simplocos tinctoria" ~ "Symplocos tinctoria",
           plantName == "Symplocos tinctotria" ~ "Symplocos tinctoria",
           plantName == "Virburnum" ~ "Viburnum",
           plantName %in% c("", "8", "9", "N/A", "Unknown") ~ "NA",
           TRUE ~ plantName),
         cleanedPlantName = str_replace(cleanedPlantName, " spp.$", ""),
         cleanedPlantName = str_replace(cleanedPlantName, " spp$", ""),
         cleanedPlantName = str_replace(cleanedPlantName, " sp$", ""),
         cleanedPlantName = str_replace(cleanedPlantName, " sp.$", ""),
         cleanedPlantName = str_replace(cleanedPlantName, " species$", "")
         ) 


# Finding matching taxonomic entity in ITIS
plantList = data.frame(cleanedName = unique(plantspp$cleanedPlantName[plantspp$cleanedPlantName != "NA"]), 
                          sciName = NA, 
                          itis_id = NA)

for (i in 1:nrow(plantList)) {
  
  print(paste(i, "of", nrow(plantList), "\n"))
  
  hierarchy = classification(plantList$cleanedName[i], db = 'itis', accepted = TRUE)[[1]]
  
  # class is logical if taxonomic name does not match any existing names
  if (!is.null(nrow(hierarchy))) {
    if ('species' %in% hierarchy$rank) {
      plantList$sciName[i] = hierarchy$name[hierarchy$rank == 'species']
      plantList$itis_id[i] = hierarchy$id[hierarchy$rank == 'species']
    } else if ('genus' == hierarchy$rank[nrow(hierarchy)]) {
      plantspp$sciName[i] = paste(hierarchy$name[hierarchy$rank == 'genus'], 'spp.')
      plantspp$itis_id[i] = hierarchy$id[hierarchy$rank == 'genus']
    }
  }
}





plants2 = left_join(plants, sites[, c('ID', 'Region')], by = c('SiteFK' = 'ID')) %>%
  distinct(Species, Region) %>% arrange(Species, Region)

multiStateSpp = plants2 %>%
  count(Species) %>%
  filter(n > 1)

states = c()
for (s in multiStateSpp$Species) {
  states = c(states, paste(plants2$Region[plants2$Species == s], collapse = ', '))
}

# Get list of regions each species is found in
plantSppRegions = plants2 %>%
  filter(! Species %in% multiStateSpp$Species) %>%
  rbind(data.frame(Species = multiStateSpp$Species, Region = states)) %>%
  arrange(Species)







plantspp2 = full_join(plantspp, plantSppRegions, by = c('plantName' = 'Species'))

write.table(plantspp2, 'z:/projects/caterpillarscount/fia by state/cc_plantlist_20190226.txt', sep = '\t', row.names = F)


allplants = left_join(fia, plantspp, by = 'lower') %>%
  select(commonName, lower, sciName) %>%
  rbind(cc_only)

write.table(allplants, 'z:/projects/caterpillarscount/fia by state/fia_plus_cc_plantlist.txt', sep = '\t', row.names = F)


#fia = read.table('z:/projects/caterpillarscount/FIA by state/tree_ids.txt', header= T, sep = '\t')
#fia$lower = tolower(fia$commonName)
