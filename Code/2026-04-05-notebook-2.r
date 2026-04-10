#identify directory
my_wd <-getwd()
my_wd

#view available files
my_files <- dir(my_wd)
my_files


#load grand list data
data <- read.csv("Stamford_CAMA_2025.csv")

#confirm grand list loaded
head(data)

#load DPLYR
library(dplyr)

#Set CPACE compatible use codes
use_codes <- list("Com Condo Option", "Comm Condo  MDL-06", "Commercial  MDL-00", "Commercial  MDL-06", "Commercial  MDL-94", "Commercial  MDL-96", "Exempt Condo", "Exmpt 490  MDL-00", "Exmpt Cm Cond OP", "Exmpt Cm Condo", "Exmpt Comm  MDL-00", "Exmpt Comm  MDL-06", "Exmpt Comm  MDL-94", "Exmpt Comm  MDL-96", "Exmpt Ind", "Ind Condo  MDL-00", "Ind Condo  MDL-06", "Industrial  MDL-00", "Industrial  MDL-94", "Industrial  MDL-96")

#import geotags from GEOCODIO
geotags <- read.csv("Stamford addresses geotagged.csv")

#confirm Geotag loading
head (geotags)

#join geotags with grand list data
geotags_unique <- geotags %>% 
    distinct(Property.Address, .keep_all = TRUE)
inner_joined <- inner_join(data, geotags_unique, by = "Property.Address")

#copy joined data for CPACE specific table
cpace_eligible_data <- inner_joined 

#confirm CPACE load
head (cpace_eligible_data)

#confirm headers in cpace table
names(cpace_eligible_data)

#filter CPACE table
cpace_eligible_data <- cpace_eligible_data %>% 
  filter(`State.Use.Description` %in% use_codes)


#confirm cpace table data
head (cpace_eligible_data)

write.csv(filtered_data, "stamford_CPACE_potential.csv", row.names = FALSE)

#populate residential table
smarte_eligible_data <- inner_joined 

#create residential filter
resi_use_codes <- list("Single Family")

#filter residential data
smarte_eligible_data <- smarte_eligible_data %>% 
  filter(`State.Use.Description` %in% resi_use_codes)

write.csv(smarte_eligible_data, "stamfords_smarte_potential.csv", row.names = FALSE)

#Load tools for mapping
library(terra)
library(ggplot2)
library(sf)
library(ggspatial)


#load adaptive capcity maps
map_data <- st_read("adaptiveCapacity.shp")

names (map_data)

#create CPACE mapping table
cpace_eligible_map <- cpace_eligible_data %>%
    select ('Property.Address','Geocodio.Longitude','Geocodio.Latitude')


#confirm new table
head (cpace_eligible_map)

#convert to shapefile
CPACE_map_SF <- st_as_sf(cpace_eligible_map, coords = c("Geocodio.Longitude", "Geocodio.Latitude"), crs = 4326)

#transform from NAD83 to 4326
map_data_4326 <- st_transform(map_data, crs = 4326)

bbox_all <- st_bbox(CPACE_map_SF)
    

st_crs(CPACE_map_SF)

# Plot the map with buildings
ggplot() +
    geom_sf(data = map_data_4326, aes(fill = geomeanACN), color = NA, alpha = 0.6) +  # Plot the map
    geom_sf(data = CPACE_map_SF , color = 'orange', size = 3) +  # Plot buildings
    coord_sf(xlim = c(-73.632537, -73.490542), 
           ylim = c(41.01755, 41.175326)) +
    #coord_sf(xlim = c(bbox_all["xmin"], bbox_all["xmax"]), 
           #ylim = c(bbox_all["ymin"], bbox_all["ymax"])) +
  #annotation_map_tile(type = "osm") +  # adds OpenStreetMap tiles
    #coord_sf(xlim = c(-73.632537, -73.490542), 
           #ylim = c(41.01755, 41.175326)) +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = "Map of CPACE Eligible Buildings",
       x = "Longitude",
       y = "Latitude") +
  theme(legend.position = "none")

