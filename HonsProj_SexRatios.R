pacman::p_load(sf, tidyverse, terra, raster, mapview, sp, leaflet)

pa <- read.csv("Data/occ_sub.csv")

# Create a simple feature object

pa_sf <- st_as_sf(pa, coords = c("coords.x1", "coords.x2"), crs = 4326)

pa_sf
plot(pa_sf$geometry)

#Read in and plot Aulax sp.

aulax_sf <- pa_sf[pa_sf$Genus_code == "AU",]
plot(aulax_sf$geometry)

#Read in and plot Leucadendron sp.

leucadendron_sf <- pa_sf[pa_sf$Genus_code == "LD",]
plot(leucadendron_sf$geometry)

#Load fire shape file

fire <- st_read("Data/All_Fires_20_21_gw/All_Fires_20_21_gw.shp")

#Filter out older dates, I want between 2010 - 2021

fire_2010_2021 <- filter(fire, between(YEAR, 2010, 2021))

#Filter out older dates, I want between 2000 - 2021

fire_2000_2021 <- filter(fire, between(YEAR, 2000, 2021))

#disable S2 geometry library to fix error code: Error in wk_handle.wk_wkb(wkb, s2_geography_writer(oriented = oriented,  : 
# Loop 0 is not valid: Edge 174 has duplicate vertex with edge 177
sf_use_s2(F) 

#Join aulax and fires
summary(aulax_sf$X)
aulax_fires_sf <- aulax_sf[fire_2010_2021,]
summary(aulax_fires_sf$X)

#Join leucadendron and fires
summary(leucadendron_sf)
leucadendron_fires_sf <- leucadendron_sf[fire_2010_2021,]
summary(leucadendron_fires_sf$X)

#create data frame showing aulax data points that are NOT found in aulax_fires
aulax_no_fires_sf <- aulax_sf[!aulax_sf$X %in% aulax_fires_sf$X, ]

#create data frame showing leucadendron data points that are NOT found in leucadendron_fires
leucadendron_no_fires_sf <- leucadendron_sf[!leucadendron_sf$X %in% leucadendron_fires_sf$X, ]

#plot aulax_no_fires, leucadendron_no_fires and fire_2010_2021 using ggplot
ggplot() +
  geom_sf(data = aulax_no_fires_sf, color = "green") +
  geom_sf(data = leucadendron_no_fires_sf, color = "blue") +
  geom_sf(data = fire_2010_2021, color = "red") +
  theme_minimal()

#plot aulax_no_fires, leucadendron_no_fires and fire_2010_2021 using mapview
mapview(aulax_no_fires_sf, col.regions = "green") + 
  mapview(leucadendron_no_fires_sf, col.regions = "blue") + 
  mapview(fire_2010_2021, col.regions = "red")

#plot aulax_no_fires, leucadendron_no_fires and fire_2000_2021 using mapview
mapview(aulax_no_fires_sf, col.regions = "green") + 
  mapview(leucadendron_no_fires_sf, col.regions = "blue") + 
  mapview(fire_2000_2021, col.regions = "purple")
