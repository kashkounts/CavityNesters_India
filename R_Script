# Install and load required packages
needed_packages <- c("tidyverse", "sf", "raster", "rgdal", "letsR")
new.packages<-needed_packages[!(needed_packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(needed_packages, require, character.only = TRUE)

# Clean global environment
rm(list=ls()); gc()

## Distribution ranges of birds have been obtained from BirdLife International
## Read all distribution range Multipolygon file
range1 <- read_sf("data/bird_ranges_1/bird_ranges_1_6000.shp")
range2 <- read_sf("data/bird_ranges_2/bird_ranges_6001_12000.shp")
range3 <- read_sf("data/bird_ranges_3/bird_ranges_12000.shp")

##Combine into a single multipolygon file
rangeglobal <- rbind(range1, range2, range3)

##Filter only confirmed (Presence = 1), native (Origin == 1) & breeding ranges 
##(Season == 1)
rangeglobal <- rangeglobal %>% 
  filter(presence == 1 & seasonal == 1 & origin == 1)

## List of cavity-nesting species in India has been obtained 
## from Patel et al. (2021)
## Read list of cavity-nesting birds in India
cavnest <- read_csv("data/India_CavityNesters.csv")
colnames(cavnest)[1] <- "sno" 
cavnestbirds <- cavnest %>% 
  dplyr::select(-sno) %>% 
  filter(Class == "Aves")
cavnestbirds <- cavnestbirds %>% 
  mutate(sci_name = paste(Genera, Species, sep = " ")) %>% 
  distinct(sci_name, .keep_all = TRUE)

## Subset the list of cavity-nesters in India from the larger global database
cavnest_ranges <- left_join(cavnestbirds, rangeglobal, by = c("sci_name" = "sci_name"))

## Subset the list of only excavators
cavnest_excav_ranges <- cavnest_ranges %>% 
  distinct(sci_name, .keep_all = TRUE) %>% 
  filter(`Primary/Secondary Cavity User` == "Primary") %>% 
  drop_na(fid)

## Subset list of only secondary cavity nesters
cavnest_second_ranges <- cavnest_ranges %>% 
  distinct(sci_name, .keep_all = TRUE) %>% 
  filter(`Primary/Secondary Cavity User` == "Secondary") %>% 
  drop_na(fid)




## Write the Cavity Nesters Multipolygon file as an ESRI shapefile
st_write(cavnest_ranges, "data/cavnest_ranges.shp", append = F)
st_write(cavnest_excav_ranges, "data/cavnest_exacav_ranges.shp", append = F)
st_write(cavnest_second_ranges, "data/cavnest_second_ranges.shp", append = F)

## Now load this shapefile using st_read as a Large SpatialPolygon
cavnest_ranges_poly = st_read("data/cavnest_ranges.shp", stringsAsFactors=FALSE)
cavnest_excav_poly = st_read("data/cavnest_exacav_ranges.shp", stringsAsFactors = F)
cavnest_second_poly = st_read("data/cavnest_second_ranges.shp", stringsAsFactors = F)

## Subset list of SCNs on which data is not available whether they are 
## obligate or facultative cavity nesters
oblg_noinfo <- cavnest_second_poly %>%
  filter(is.na(Oblgt.F))


## Setting CRS
st_crs(cavnest_ranges_poly) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
st_crs(cavnest_excav_poly) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
st_crs(cavnest_second_poly) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
st_crs(oblg_noinfo) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  #geographical, datum WGS84

# This function creates presence-absence matrix and adds a species richness value to it.
## change resol to adjust the grid size (decimal degree to Kilometers)
## Warning: This function can take large amount of time to complete, depending on the dataset size, extent and resol

## Creating a model for excavators

PAM_model <-lets.presab(cavnest_excav_poly, xmn = 68, xmx = 98, 
                        ymn = 4, ymx = 38,
                        resol = 0.20,
                        remove.cells = T, remove.sp = T, show.matrix = FALSE,
                        crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"), 
                        cover = 0, count = T)

## Plot the richness map
plot(PAM_model$Richness_Raster)

#Export the richness raster as a tiff
raster::writeRaster(PAM_model$Richness_Raster, "excav_richness.tiff", overwrite = T)

## Creating a model for SCNs

PAM_model2 <-lets.presab(cavnest_second_poly, xmn = 68, xmx = 98, 
                        ymn = 4, ymx = 38,
                        resol = 0.20,
                        remove.cells = T, remove.sp = T, show.matrix = FALSE,
                        crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"), 
                        cover = 0, count = T)

## Plot the richness map
plot(PAM_model2$Richness_Raster)

#Export the richness raster as a tiff
raster::writeRaster(PAM_model2$Richness_Raster, "second_richness.tiff", overwrite = T)

## Creating a model for missing info species

PAM_model3 <-lets.presab(oblg_noinfo, xmn = 68, xmx = 98, 
                        ymn = 4, ymx = 38,
                        resol = 0.20,
                        remove.cells = T, remove.sp = T, show.matrix = FALSE,
                        crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"), 
                        cover = 0, count = T)

## Plot the richness map
plot(PAM_model3$Richness_Raster)


