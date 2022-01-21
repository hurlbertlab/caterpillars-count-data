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
  
  survQAQC = dataToCheck %>%
    mutate(arthFlag = case_when(
      Group == 'ant' & Length > 15 | Group == 'ant' & Quantity > 50 ~ 'ant',
      Group == 'aphid' & Length > 10 | Group == 'aphid' & Quantity > 50 ~ 'aphid',
      Group == 'bee' & Length > 25 | Group == 'bee' & Quantity > 6 ~ 'bee',
      Group == 'beetle' & Length > 20 | Group == 'beetle' & Quantity > 6 ~ 'beetle',
      Group == 'caterpillar' & Length > 50 | Group == 'caterpillar' & Quantity > 6 ~ 'caterpillar',
      Group == 'daddylonglegs' & Length > 15 | Group == 'daddylonglegs' & Quantity > 6 ~ 'daddylonglegs',
      Group == 'fly' & Length > 15 | Group == 'fly' & Quantity > 6 ~ 'fly',
      Group == 'grasshopper' & Length > 20 | Group == 'grasshopper' & Quantity > 6 ~ 'grasshopper',
      Group == 'leafhopper' & Length > 20 | Group == 'leafhopper' & Quantity > 6 ~ 'leafhopper',
      Group == 'moths' & Length > 30 | Group == 'moths' & Quantity > 6 ~ 'moths',
      Group == 'other' & Length > 25 | Group == 'other' & Quantity > 6 ~ 'other',
      Group == 'spider' & Length > 15 | Group == 'spider' & Quantity > 6 ~ 'spider',
      Group == 'truebugs' & Length > 25 | Group == 'truebugs' & Quantity > 6 ~ 'truebugs',
      Group == 'unidentified' & Length > 25 | Group == 'unidentified' & Quantity > 6 ~ 'unidentified',
      TRUE ~ ''
    )) %>%
    group_by(ID, NumberOfLeaves, AverageLeafLength) %>%
    summarize(arthFlags = trimws(paste(arthFlag, collapse = " ")),
              totalArthAbund = sum(Quantity[!Group %in% c('ant', 'aphid')], na.rm = TRUE),
              totalArthDiv = n_distinct(Group[!is.na(Group)]),
              rareArthDiv = n_distinct(Group[Group %in% c('truebugs', 'grasshopper', 'daddylonglegs', 'bee', 'moths')])) %>%
    mutate(survFlag1 = ifelse(totalArthAbund > totalArthsMax, paste(arthFlags, 'totalArthAbund'), arthFlags),
           survFlag2 = ifelse(totalArthDiv > arthDiversityMax, paste(survFlag1, 'totalArthDiv'), survFlag1),
           survFlag3 = ifelse(NumberOfLeaves > numberLeavesMax | NumberOfLeaves < numberLeavesMin, paste(survFlag2, 'numLeaves'), survFlag2),
           survFlag4 = ifelse(AverageLeafLength > leafLengthMax, paste(survFlag3, 'leafLength'), survFlag3),
           flags = trimws(survFlag4),
           status = ifelse(flags == '', 'ok', 'check')) %>%
    ungroup() %>%
    dplyr::select(ID, flags, status) %>%
    right_join(dataToCheck, by = 'ID') %>%
    dplyr::select(ID, UserFKOfObserver:cell, flags, status) %>%
    mutate(actionTaken = NA)
  
  # Optionally write the qa/qc'ed dataset to a file
  if (write) {
    write.csv(survQAQC, paste0('dataCleaning/flagged_dataset_', Sys.Date(), '.csv'), row.names = F)
  }
  return(survQAQC)
}
