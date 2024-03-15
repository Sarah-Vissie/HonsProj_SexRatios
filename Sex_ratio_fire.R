#Find sites without fire for species of interest
install.packages("pacman")
pacman::p_load(sf, tidyverse, terra, raster, mapview)

# Load the data
pa <- read.csv("/Users/michaelcramer/Library/CloudStorage/Dropbox/Dropbox_GIS_resources/Protea_Atlas/occ_sub.csv")

# Create a simple feature object
pa_sf <- st_as_sf(pa, coords = c("coords.x1", "coords.x2"), crs = 4326)

pa_sf
plot(pa_sf$geometry)

aulax_sf <- pa_sf[pa_sf$Genus_code == "AU",]
plot(aulax_sf$geometry)

#Load fire layer raster
fire_num <- raster("/Users/michaelcramer/Library/CloudStorage/Dropbox/Dropbox_GIS_resources/Fires/Fire_num.tif")

plot(fire_num)

#Extract the fire number for each point
aulax_sf$fire_num <- extract(fire_num, aulax_sf)

names(aulax_sf)

plot(aulax_sf[aulax_sf$fire_num == 0,]$geometry)

Provinces_sf <- st_read("/Users/michaelcramer/Library/CloudStorage/Dropbox/Dropbox_GIS_resources/Protea_Atlas/Provinces/Provinces.shp")


#Plot the aulax_sf points coloured by species using ggplot
ggplot() +
  geom_sf(data = aulax_sf[aulax_sf$fire_num == 0,], aes(colour = Genus_species)) +
  geom_sf(data = Provinces_sf[Provinces_sf$PROVINCE == "WESTERN CAPE", ], fill = NA, colour = "black") +
  scale_colour_brewer(palette = "Set1") +
  theme_minimal()

mapview(aulax_sf[aulax_sf$fire_num == 0,], zcol="Genus_species") + crop(fire_num, extent(aulax_sf[aulax_sf$fire_num == 0,]))

unique(pa_sf$Genus_code)
leuc_sf <- pa_sf[pa_sf$Genus_code == "LD",]
plot(leuc_sf$geometry)

#Extract the fire number for each point
leuc_sf$fire_num <- extract(fire_num, leuc_sf)


plot(leuc_sf[leuc_sf$fire_num == 0,]$geometry)

#Plot the aulax_sf points coloured by species using ggplot
ggplot() +
  geom_sf(data = leuc_sf[leuc_sf$fire_num == 0,], aes(colour = Genus_species)) +
  geom_sf(data = Provinces_sf[Provinces_sf$PROVINCE == "WESTERN CAPE", ], fill = NA, colour = "black") +
  scale_colour_brewer(palette = "Set1") +
  theme_minimal()

mapview(leuc_sf[leuc_sf$fire_num == 0,], zcol="Genus_species")

#Test the power required ti find a sex ratio that deviates from 1:1
library(pwr)

for(p2 in seq(0.05, 0.45, 0.05)) {
  p1=0.5
  h <- sqrt((2 * (asin(sqrt(p1)) - asin(sqrt(p2))))^2)
  power_result <- pwr.p.test(h = h, power = 0.80, sig.level = 0.05)
  print(power_result)
  if(p2==0.05) {output <- data.frame(p1=p1, p2=p2, h=h, n=round(power_result$n,0))} else {output <- rbind(output, data.frame(p1=p1, p2=p2, h=h, n=round(power_result$n,0)))}
}

#This is the result. p2 is the assumed sex ratio of the second group. h is the effect size. n is the sample size required to find a significant difference between the two groups.
output
