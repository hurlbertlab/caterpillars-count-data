# Functions for working with and analyzing Caterpillars Count! data
library(dplyr)
library(maps)
library(sp)
library(maptools)
options(dplyr.summarise.inform = FALSE)

###################################
# Function for substituting values based on a condition using dplyr::mutate
# Modification of dplyr's mutate function that only acts on the rows meeting a condition
mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}


#########################################
# Get county name from lat-longs
# From https://stackoverflow.com/questions/13316185/r-convert-zipcode-or-lat-long-to-county
# Note: had to remove proj4string references

# The single argument to this function, pointsDF, is a data.frame in which:
#   - column 1 contains the longitude in degrees (negative in the US)
#   - column 2 contains the latitude in degrees

latlong2county <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per county
  require(sp)
  require(maps)
  require(maptools)
  
  counties <- maps::map('county', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(counties$names, ":"), function(x) x[1])
  counties_sp <- map2SpatialPolygons(counties, IDs=IDs)
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF)
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, counties_sp)
  
  # Return the county names of the Polygons object containing each point
  countyNames <- sapply(counties_sp@polygons, function(x) x@ID)
  countyNames[indices]
}




# Function for doing basic QA/QC check on Caterpillars Count! dataset

qaqc = function(dataToCheck,               # subset of fullDataset dataframe (i.e. new data) that requires checking
                totalArthsMax = 10,        # flag surveys where the total number of arthropods exceeds this value
                arthDiversityMax = 4,      # flag surveys where the total number of arthropod types exceeds this value
                rareArthDiversityMax = 3,  # flag surveys where the total number of rare arthropod types exceeds this value
                numberLeavesMin = 5,       # flag surveys where number of leaves is less than this value
                numberLeavesMax = 400,     # flag surveys where number of leaves falls exceeds this value
                leafLengthMax = 30,        # flag surveys where leaf length exceeds this value
                write = TRUE
) {
  
  arthQAQC = dataToCheck %>%
    mutate(arthFlagLength = case_when(
      Group == 'ant' & Length > 17 ~ 'antLength',
      Group == 'aphid' & Length > 10 ~ 'aphidLength',
      Group == 'bee' & Length > 25 ~ 'beeLength',
      Sawfly == 1 & Length > 50 ~ 'sawflyLength',
      Group == 'beetle' & Length > 20 ~ 'beetleLength',
      Group == 'caterpillar' & Length > 50 ~ 'caterpillarLength',
      Group == 'daddylonglegs' & Length > 15 ~ 'daddylonglegsLength',
      Group == 'fly' & Length > 20 ~ 'flyLength',
      Group == 'grasshopper' & Length > 20 ~ 'grasshopperLength',
      Group == 'leafhopper' & Length > 20 ~ 'leafhopperLength',
      Group == 'moths' & Length > 30 ~ 'mothLength',
      Group == 'other' & Length > 25 ~ 'otherLength',
      Group == 'spider' & Length > 20 ~ 'spiderLength',
      Group == 'truebugs' & Length > 25 ~ 'truebugLength',
      Group == 'unidentified' & Length > 25 ~ 'unidLength',
      TRUE ~ ''),
      arthFlagNum = case_when(
        Group == 'ant' & Quantity > 50 ~ 'antNum',
        Group == 'aphid' & Quantity > 50 ~ 'aphidNum',
        Group == 'bee' & Quantity > 6 ~ 'beeNum',
        Sawfly == 1 & Quantity > 20 ~ 'sawflyNum',
        Group == 'beetle' & Quantity > 10 ~ 'beetleNum',
        Group == 'caterpillar' & Quantity > 6 ~ 'caterpillarNum',
        Group == 'daddylonglegs' & Quantity > 6 ~ 'daddylonglegsNum',
        Group == 'fly' & Quantity > 6 ~ 'flyNum',
        Group == 'grasshopper' & Quantity > 6 ~ 'grasshopperNum',
        Group == 'leafhopper' & Quantity > 6 ~ 'leafhopperNum',
        Group == 'moths' & Quantity > 6 ~ 'mothNum',
        Group == 'other' & Quantity > 6 ~ 'otherNum',
        Group == 'spider' & Quantity > 6 ~ 'spiderNum',
        Group == 'truebugs' & Quantity > 6 ~ 'truebugNum',
        Group == 'unidentified' & Quantity > 6 ~ 'unidNum',
        TRUE ~ ''
      )
    )
    
  survQAQC = dataToCheck %>%
    group_by(ID, NumberOfLeaves, AverageLeafLength) %>%
    summarize(totalArthAbund = sum(Quantity[!Group %in% c('ant', 'aphid')], na.rm = TRUE),
              totalArthDiv = n_distinct(Group[!is.na(Group)]),
              rareArthDiv = n_distinct(Group[Group %in% c('truebugs', 'grasshopper', 'daddylonglegs', 'bee', 'moths')])) %>%
    ungroup() %>%
    mutate(survFlag1 = ifelse(totalArthAbund > totalArthsMax, 'totalArthAbund', ''),
           survFlag2 = ifelse(totalArthDiv > arthDiversityMax, paste(survFlag1, 'totalArthDiv'), survFlag1),
           survFlag3 = ifelse(NumberOfLeaves > numberLeavesMax | NumberOfLeaves < numberLeavesMin, paste(survFlag2, 'numLeaves'), survFlag2),
           survFlag4 = ifelse(AverageLeafLength > leafLengthMax, paste(survFlag3, 'leafLength'), survFlag3),
           survFlag5 = trimws(survFlag4)) %>%
    dplyr::select(ID, survFlag5) 
  
  finalQAQC = arthQAQC %>%
    left_join(survQAQC, by = 'ID') %>%
    # if there is an arthropod flag for abundance, then no need to report the totalArthAbund flag
    mutate(survFlags = ifelse(arthFlagNum != "", trimws(gsub('totalArthAbund', '', survFlag5)), survFlag5),
           flags = trimws(paste(arthFlagNum, arthFlagLength, survFlags)),
           status = ifelse(flags == '', 'ok', 'check'),
           actionTaken = NA) %>%
    dplyr::select(ID:cell, flags, status, actionTaken)
    

  # Optionally write the qa/qc'ed dataset to a file
  if (write) {
    write.csv(finalQAQC, paste0('dataCleaning/flagged_dataset_', Sys.Date(), '.csv'), row.names = F)
  }
  return(survQAQC)
}
