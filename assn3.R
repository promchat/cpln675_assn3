## cpln 675 calgary

library(tidyverse)
library(sf)
library(raster)

library(terra)

#set wd
setwd("C:/Users/cchue/Documents/Penn MUSA/Spring/Land Use/cpln675_assignment3/midTermProject_Data")

#city limits and fishnet
calgary <- read_sf("CALGIS_CITYBOUND_LIMIT")

cal_grid <- st_make_grid(calgary, cellsize = 1000) # 1 KM
cal_grid <- cal_grid[calgary] %>%            # clips the grid to the boundary file
  st_sf() %>%
  mutate(uniqueID = as.character(rownames(.)))

cal_grid_centroid <- st_centroid(cal_grid)




#inundation / dependent var


cal_inun <- raster("inundation/prj.adf")
inundation_sf <- rasterToPolygons(cal_inun, fun=function(x){x==1}) %>%
  st_as_sf() %>% 
  st_transform(st_crs(calgary)) %>% 
  st_union() %>% st_sf()

cal_grid$inun <- st_intersects(cal_grid, inundation_sf, sparse = F)

ggplot()+
  geom_sf(data = cal_grid, aes(fill = inun))

#dem
cal_dem <- raster("calgarydem/prj.adf")

plot(cal_dem)

cal_dem_sf <- as.polygons(rast(cal_dem)) %>% st_as_sf() %>% st_transform(st_crs(cal_grid)) 

cal_grid1 <- cal_grid_centroid %>% st_join(cal_dem_sf) %>% 
  st_drop_geometry() %>% 
  full_join(., cal_grid) %>% st_sf()

ggplot(cal_grid1)+
  geom_sf(aes(fill = COUNT), color = NA)

