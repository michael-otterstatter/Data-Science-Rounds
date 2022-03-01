########################################################################################
# BCCDC BIOSTATS: GEOSPATIAL ANALYSIS IN R - PART 1
#
# Purpose: Illustration of the process of from spatial data to finished map
# This script was written using R version 3.6.1 (2019-07-05) -- "Action of the Toes"
#
# Note: The R code below can be collapsed or expanded into sections for easy navigation:
#    collapse code - Alt+O
#    expand code - Shift+Alt+O
#
# Author: Michael Otterstatter
#
# Created: Sep 6, 2019
#
# Last modified: 
# 
########################################################################################





#### Setup ####

# If using this script for the first time, certain R functions (organized into 'packages')
#   may need to be installed on your computer.  Provided you have an internet connection, the
#   following setup will retrieve and install the necessary functions:

install.packages("checkpoint") # functions to ensure reproducibility


# Set working directory and checkpoint to ensure reproducibility:
setwd("O:/BCCDC/Groups/Analytics_Resources/Training/Biostats/Sessions/Sep 6 2019 - geospatial analysis 1")

library(checkpoint) # Load checkpoint library 
checkpoint("2019-07-01") # set checkpoint date


# load R libraries with key functions for data manipulation and spatial data analysis
library(tidyverse) # efficient data manipulation tools
library(sf)  # tools for working with spatial (vector) data
library(tmap) # tools for generating thematic maps



# specify data source and destination filepaths and filenames
source_url_stcn <- "http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/2016"
source_url_attr <- "http://www12.statcan.gc.ca/census-recensement/2016/geo/ref/gaf/files-fichiers"

download_dir_census <- "O:/BCCDC/Groups/Analytics_Resources/Training/Biostats/Sessions/Sep 6 2019 - geospatial analysis 1/census_2016"

source_file_pr <- "lpr_000b16a_e.zip"
source_file_db <- "ldb_000b16a_e.zip"
source_file_attr <- "2016_92-151_XBB_csv.zip"

shape_file_pr <- "lpr_000b16a_e.shp"
shape_file_db <- "ldb_000b16a_e.shp"

attr_file_db <- "2016_92-151_XBB.csv"




# 1. Import and Examine Data

# download and unzip source files 

# *NOTE* these files have already been downloaded and unzipped in our Biostats folder, so skip to reading the files
#   the code for downloading and unzipping is shown just as an example
###################################################################################################################

# shapefile for provinces
download.file(url = paste(source_url_stcn, source_file_pr, sep = "/"),
              destfile = paste(download_dir_census, source_file_pr, sep = "/"))
unzip(zipfile = paste(download_dir_census, source_file_pr, sep = "/"), exdir = download_dir_census)

# shapefile for dissemination blocks
download.file(url = paste(source_url_stcn, source_file_db, sep = "/"),
              destfile = paste(download_dir_census, source_file_db, sep = "/"))
unzip(zipfile = paste(download_dir_census, source_file_db, sep = "/"), exdir = download_dir_census)

###################################################################################################################



# read province and dissemination block shapefiles from destination directory
pr_census_2016 <- st_read(dsn = paste(download_dir_census, shape_file_pr, sep = "/"), 
                          stringsAsFactors = FALSE)
db_census_2016 <- st_read(dsn = paste(download_dir_census, shape_file_db, sep = "/"), 
                          stringsAsFactors = FALSE)

# Note the details provided by R when reading shapefiles, including 
#   the driver used to read the file (ESRI Shapefile)
#   the structure of the file (sf file with 13 rows ('features') and 6 columns ('fields'))
#   the spatial extent of the data, given by the dimensions of the bounding box ('bbox')
#   the unqiue EPSG code that identifies the coordinate reference system used (here, no code is provided) 
#   the coordinate reference system (CRS) given by the 'proj4string', which includes
#       the projection used (lcc or 'Lambert conformal conic' projection)
#       the origin (at_0=63.390675 +lon_0=-91.86666666666666 )
#       the datum used (North American Datum of 1983 'NAD83')
#       the distance units used (meters 'm')

  # Reading layer `lpr_000b16a_e' from data source `C:\Users\Michael\Google Drive\CPAC geospatial analysis training\R\examples\census_2016\lpr_000b16a_e.shp' 
  #     using driver `ESRI Shapefile'
  # Simple feature collection with 13 features and 6 fields
  # geometry type:  MULTIPOLYGON
  # dimension:      XY
  # bbox:           xmin: 3689439 ymin: 659338.9 xmax: 9015737 ymax: 5242179
  # epsg (SRID):    NA
  # proj4string:    +proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-91.86666666666666 +
  #   x_0=6200000 +y_0=3000000 +datum=NAD83 +units=m +no_defs


# note the structure of sf objects, with each row representing a 'feature' that has spatial information
#   stored in the 'geometry' column (e.g., lon/lat or x/y coordinates), and often additional information
#   stored in columns
pr_census_2016


# we could also get the coordinate reference system information (if available in the file) using st_crs()
st_crs(pr_census_2016)

# if needed we could transform coordinate reference system to a different one using st_transform()
pr_census_2016_wgs84 <- st_transform(pr_census_2016, crs = "+proj=latlong")
st_crs(pr_census_2016_wgs84)

# available CRS values can be found by typing  
st_proj_info()


# spatial objects such as boundary files are often highly detailed and slow to process
#   unecessary detail can be reduced with st_simplify() making processing faster
object.size(pr_census_2016) # 61 MB

pr_census_2016_simple <- st_simplify(pr_census_2016, dTolerance = 500)

object.size(pr_census_2016_simple) # 3 MB (still looks good, but accuracy depends on your specific use)


# datasets with coordinates can be converted to spatial data objects using st_as_sf()
my_locations <- tibble(name = c("Palliser Hotel", "BCCDC"), 
                       lon = c(-114.064961, -123.118354), 
                       lat = c(51.044286, 49.260714))



# note lat and lon values from google maps use crs WGS84, so this must be specified
my_locations_sf <- st_as_sf(my_locations, coords = c("lon", "lat"), crs = 4326)

 
# accurate measurements of distance, area, direction, etc. requires careful choice of
#   projection system

# distance measured from unprojected data: 676.253 km 
st_distance(my_locations_sf)

# compared to distance from Lambert conformal conic projection: 643.788 km (32 km less)
st_distance(st_transform(my_locations_sf, crs = 102009))



# Basic Plotting

# Base R plot function

# subset data to include only province name and geometry information
ca_plotting_data <- pr_census_2016_simple %>%
  select(PRENAME, geometry)

# base R plot is easy but not very flexible
ca_plotting_data %>%
  plot(col = "grey", reset = FALSE)

# add layers
ca_plotting_data %>%
  filter(PRENAME == "Alberta") %>%
  plot(add = TRUE, col = "white")

# plot location points (note, these must be transformed to the same CRS as our map,
#   otherwise they will appear in the wrong location)
my_locations_sf %>%
  st_transform(crs = st_crs(ca_plotting_data)) %>%
  plot(add = TRUE, col = "blue", cex = 1)



# R package tmap

# Using the tmap package, we can create the same map as a series of layers, without creating a new
#   dataset 
pr_census_2016_simple %>%
  mutate(pr_colours = ifelse(PRENAME == 'Alberta', "white", "grey")) %>% # create a new variable to plot provinces by colour
  tm_shape() +
  tm_polygons(col = "pr_colours", title = "") +
  tm_shape(my_locations_sf) + # identify additional dataset with points of interest
  tm_dots(col = "name", size = 0.5, palette = "RdYlBu",  title = "Locations of interest")
 



# working with census boundaries

# dissemination block file is 929 MB!
object.size(db_census_2016)

# could simplify but need to consider if this affects accuracy
db_census_2016_simple <- st_simplify(db_census_2016, preserveTopology = TRUE, dTolerance = 500)
object.size(db_census_2016_simple)


# compare dissemination blocks in the simplified file...
db_census_2016_simple %>%
  filter(CDNAME == "Nanaimo") %>%
  tm_shape() +
  tm_borders()

# ...versus the original file  
db_census_2016 %>%
  filter(CDNAME == "Nanaimo") %>%
  tm_shape() +
  tm_borders()


# examine map interactively in context using background street maps or topographical maps by switching to view mode in tmap
tmap_mode("view")

db_census_2016 %>%
  filter(CDNAME == "Nanaimo") %>%
  tm_shape() +
  tm_borders()







