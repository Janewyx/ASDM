## Codes for producing visualisations specifically for my proof of concept presentation on the project.

library(raster)
library(rasterVis)
library(ggplot2)
library(RColorBrewer)

dem <- raster("canada_dem.asc")
asp <- raster("canada_asp.asc")
temp <- raster("bio06_2010.asc")
tree <- raster("Can_VegTreed_20.tif")
iniDist <- raster("iniDist.asc")
dem_bar <- raster("dembar.asc")
temp_bar <- raster("tempbar.asc")
hs <- raster("hsmap.asc")

## plotting acquired data
demplot <- gplot(dem) +
  geom_raster(aes(fill = value), na.rm = TRUE) +
  scale_fill_gradient(low = "white", high = "black", guide = FALSE, na.value = "transparent") +
  theme_void() +
  theme(panel.background = element_rect(fill = "black"))
plot(demplot)

aspplot <- gplot(asp) +
  geom_raster(aes(fill = value), na.rm = TRUE) +
  scale_fill_gradient(low = "white", high = "black", guide = FALSE, na.value = "transparent") +
  theme_void() +
  theme(panel.background = element_rect(fill = "black"))
plot(aspplot)

tempplot <- gplot(temp) +
  geom_raster(aes(fill = value), na.rm = TRUE) +
  scale_fill_gradient(low = "white", high = "grey30", guide = FALSE, na.value = "transparent") +
  theme_void() +
  theme(panel.background = element_rect(fill = "black"))
plot(tempplot)

treeplot <- gplot(tree) +
  geom_raster(aes(fill = value), na.rm = TRUE) +
  scale_fill_gradient(low = "white", high = "black", guide = FALSE, na.value = "transparent") +
  theme_void() +
  theme(panel.background = element_rect(fill = "black"))
plot(treeplot)

ggsave("spread_m/plots/dem.png", demplot, width = 4, height = 3)
ggsave("spread_m/plots/asp.png", aspplot, width = 4, height = 3)
ggsave("spread_m/plots/temp.png", tempplot, width = 4, height = 3)
ggsave("spread_m/plots/tree.png", treeplot, width = 4, height = 3)

## visualising distribution
distplot <- gplot(iniDist) +
  geom_raster(aes(fill = value), na.rm = TRUE) +
  scale_fill_gradient(high = "white", low = "#b2b3b6", guide = FALSE, na.value = "transparent") +
  theme_void() +
  theme(panel.background = element_rect(fill = "black"))
plot(distplot)

ggsave("spread_m/plots/distplot.png", distplot, width = 4, height = 3)

## MigClim model viz
dembarplot <- gplot(dem_bar) +
  geom_raster(aes(fill = value), na.rm = TRUE) +
  scale_fill_gradient(high = "white", low = "#7fc97f", guide = FALSE, na.value = "transparent") +
  theme_void() +
  theme(panel.background = element_rect(fill = "black"))
plot(dembarplot)

tempbarplot <- gplot(temp_bar) +
  geom_raster(aes(fill = value), na.rm = TRUE) +
  scale_fill_gradient(high = "white", low = "#7fc97f", guide = FALSE, na.value = "transparent") +
  theme_void() +
  theme(panel.background = element_rect(fill = "black"))
plot(tempbarplot)

hsplot <- gplot(hs) +
  geom_raster(aes(fill = value), na.rm = TRUE) +
  scale_fill_gradient(high = "white", low = "grey30", guide = FALSE, na.value = "transparent") +
  theme_void() +
  theme(panel.background = element_rect(fill = "black"))
plot(hsplot)

ggsave("spread_m/plots/dembarplot.png", dembarplot, width = 4, height = 3)
ggsave("spread_m/plots/dembarplot.png", dembarplot, width = 4, height = 3)
ggsave("spread_m/plots/hsplot.png", hsplot, width = 4, height = 3)
