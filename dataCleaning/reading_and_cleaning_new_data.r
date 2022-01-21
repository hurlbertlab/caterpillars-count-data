# Reading in Caterpillars Count! database files and performing QA/QC on any new data.
library(dplyr)
library(lubridate)
library(raster)
library(sf)
#library(rvest)
#library(xml2)
#library(stringr)
#library(rgdal)
#library(dggridR)


source('dataCleaning/reading_and_cleaning_functions.r')

# Slope and intercept parameters for power function length-weight regressions for different arthropod groups
massregs = read.csv('arthropod_length_weight_regressions.csv')

# Read in hex grid data
hex <- st_read("hexgrid_materials/hex_grid_crop.shp")


# Read in data files

sites = read.csv(list.files()[grepl("Site.csv", list.files())])
                 
surveys = read.csv(list.files()[grepl("Survey.csv", list.files())])

plants = read.csv(list.files()[grepl("Plant.csv", list.files())])

arths = read.csv(list.files()[grepl("ArthropodSighting.csv", list.files())]) %>%
  rename(Group = "UpdatedGroup", BeetleLarva = "UpdatedBeetleLarva", Sawfly = "UpdatedSawfly") %>%
  left_join(massregs, by = 'Group') %>%
  mutate(Biomass_mg = Quantity*a_constant*Length^b_exponent, 
         Photo = ifelse(PhotoURL == "", 0, 1)) %>%
  dplyr::select(ID:BeetleLarva, Biomass_mg, Photo)

surveys$LocalDate = as.Date(surveys$LocalDate, format = "%Y-%m-%d")
surveys$Year = as.numeric(format(surveys$LocalDate, "%Y"))
surveys$julianday = yday(surveys$LocalDate)
surveys$julianweek = 7*floor(surveys$julianday/7) + 4


# Median green up date for 2001-2017 based on MODIS MCD12Q2 v006
# downloaded from USANPN.org gridded products
greenup = raster("https://raw.githubusercontent.com/hurlbertlab/caterpillars-analysis-public/master/data/env/inca_midgup_median_nad83_02deg.tif")

sites$medianGreenup = round(raster::extract(greenup, sites[, c('Longitude', 'Latitude')]))

# Manually get median green up for Currituck Banks and Sault College which fall just outside of raster cells
sites$medianGreenup[sites$Name == "Currituck Banks Reserve"] = 
  round(mean(unlist(raster::extract(greenup, data.frame(longitude = sites$Longitude[sites$Name == "Currituck Banks Reserve"], 
                                                        latitude = sites$Latitude[sites$Name == "Currituck Banks Reserve"]),
                                    buffer = 3000)), na.rm = TRUE))

sites$medianGreenup[sites$Name == "Sault College"] = 
  round(mean(unlist(raster::extract(greenup, data.frame(longitude = sites$Longitude[sites$Name == "Sault College"], 
                                                        latitude = sites$Latitude[sites$Name == "Sault College"]),
                                    buffer = 7000)), na.rm = TRUE))

# One of the Acadia NP sites falls just off the raster coverage, assign it same value as its neighbor:
sites$medianGreenup[sites$Name == "Acadia NP - Alder"] = sites$medianGreenup[sites$Name == "Acadia NP - Sundew"]

# Add county info based on lat-longs
sites$county = latlong2county(sites[, c('Longitude', 'Latitude')])

# Canadian sites and a few other exceptions need to be added manually
sites$county = case_when(sites$Name == 'Sault College' ~ 'ontario,algoma',
                         sites$Name %in% c('RVCC', 'Beare Swamp in Rouge Park') ~ 'ontario,toronto',
                         sites$Name == 'Linda Loring Nature Foundation' ~ 'massachusetts,nantucket',
                         sites$Name %in% c('Acadia NP - Alder', 'Acadia NP - Sundew') ~ 'maine,hancock',
                         sites$Name == 'Wye Marsh Wildlife Centre' ~ 'ontario,simcoe',
                         sites$Name == 'Riverbend Park' ~ 'virginia,fairfax',
                         !is.na(sites$county) ~ sites$county)

# Join eBird county codes. As new CC! sites arise in new counties, they will need to be added manually
# to this countyCodes.txt file (could request full table from eBird...)
countyCodes = read.table('https://raw.githubusercontent.com/hurlbertlab/caterpillars-analysis-public/master/data/countyCodes.txt', sep = '\t', header = T)
sites2 = left_join(sites, countyCodes, by = 'county')

# Number of weeks per site per year per hex cell
hexcells <- sites %>% 
  st_as_sf(coords = c("Longitude", "Latitude")) %>%
  st_set_crs("+proj=longlat +datum=WGS84 +no_defs") %>%
  st_intersection(hex)   

sites3 = left_join(sites2, hexcells[, c('Name', 'cell')], by = 'Name')

# Note there are still a few sites with no greenup data including 
#   RVCC, Beare Swamp in Rouge Park, and Wye Marsh Wildlife Centre


############################################################################################
# Read in most recent cleaned_dataset, and identify survey ID's that are not in that dataset,
#   and join together all of the arth, plant, and site data to that dataframe

cleanedData = read.csv(paste('dataCleaning/', list.files('dataCleaning')[grepl('cleaned_dataset', list.files('dataCleaning'))], sep = ''))

newData = surveys %>%
  filter(!ID %in% cleanedData$ID) %>%
  dplyr::select(ID, UserFKOfObserver, PlantFK, LocalDate, julianday, julianweek, Year, ObservationMethod, Notes, WetLeaves, 
                PlantSpecies, NumberOfLeaves, AverageLeafLength, HerbivoryScore) %>%
  left_join(arths[, names(arths) != "PhotoURL"], by = c('ID' = 'SurveyFK')) %>%
  left_join(plants, by = c('PlantFK' = 'ID')) %>%
  left_join(sites3[, c('ID', 'Name', 'Latitude', 'Longitude', 'Region', 'medianGreenup', 'ebirdCounty', 'cell')], by = c('SiteFK' = 'ID')) %>% 
  mutate_cond(is.na(Quantity), Quantity = 0, Group) %>%
  mutate_cond(is.na(Biomass_mg), Biomass_mg = 0, Group) %>%
  rename(surveyNotes = Notes.x, bugNotes = Notes.y, arthID = ID.y) %>%
  filter(Name != "Example Site")

# The qaqc function will write a new file called 'flagged_dataset_YYYY-MM-DD.csv'.
qaqc(newData)

# Once all flagged records have been examined manually and corrected if necessary, these records should then be appended to
# the most recent 'cleaned_dataset' file and saved with the current date.