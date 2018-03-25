## Codes for producing visualisations specifically for my proof of concept presentation on the project.

library(raster)
library(rasterVis)
library(ggplot2)
library(RColorBrewer)
library(ggsn)
library(rgdal)

occur <- read.csv("occurrence.csv")

asia <- readOGR("Asia.shp")
dem <- raster("canada_dem.asc")
asp <- raster("canada_asp.asc")
temp <- raster("bio06_2010.asc")
temp2 <- raster("bio05.asc")
HII <- raster("canadahii.asc")
tree <- raster("Can_VegTreed_20.tif")
iniDist <- raster("iniDist.asc")
dem_bar <- raster("dembar.asc")
temp_bar <- raster("tempbar.asc")
hs <- raster("hsmap.asc")
A1Bcol <- raster("A1Bcol.asc")
A2col <- raster("A2col.asc")

## plotting occurrence records from Asia
asia <- fortify(asia, region = "")

occplot <- ggplot(asia) +
  geom_polygon()

## plotting acquired data for model inputs
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

temp2plot <- gplot(temp2) +
  geom_raster(aes(fill = value), na.rm = TRUE) +
  scale_fill_gradient(low = "white", high = "grey30", guide = FALSE, na.value = "transparent") +
  theme_void() +
  theme(panel.background = element_rect(fill = "black"))
plot(temp2plot)

HIIplot <- gplot(HII) +
  geom_raster(aes(fill = value), na.rm = TRUE) +
  scale_fill_gradient(low = "white", high = "grey10", guide = FALSE, na.value = "transparent") +
  theme_void() +
  theme(panel.background = element_rect(fill = "black"))
plot(HIIplot)

treeplot <- gplot(tree) +
  geom_raster(aes(fill = value), na.rm = TRUE) +
  scale_fill_gradient(low = "white", high = "black", guide = FALSE, na.value = "transparent") +
  theme_void() +
  theme(panel.background = element_rect(fill = "black"))
plot(treeplot)

ggsave("spread_m/plots/dem.png", demplot, width = 4, height = 3)
ggsave("spread_m/plots/asp.png", aspplot, width = 4, height = 3)
ggsave("spread_m/plots/temp.png", tempplot, width = 4, height = 3)
ggsave("spread_m/plots/temp2.png", temp2plot, width = 4, height = 3)
ggsave("spread_m/plots/HII.png", HIIplot, width = 4, height = 3)
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
  scale_fill_gradient(high = "white", low = "#386cb0", guide = FALSE, na.value = "transparent") +
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
ggsave("spread_m/plots/tempbarplot.png", tempbarplot, width = 4, height = 3)
ggsave("spread_m/plots/hsplot.png", hsplot, width = 4, height = 3)

## scale bar specifications
xmin <- xmin(A1Bcol)
xmax <- xmax(A1Bcol)
ymin <- ymin(A1Bcol)
ymax <- ymax(A1Bcol)

## plotting colonised cells during simulation
## creating palette for plot labels
pal_col <- c("grey90", "grey50", rev(brewer.pal(6, "Accent")), "transparent")

A1Bcol_1 <- gplot(A1Bcol) +
  geom_raster(aes(fill = factor(value)), na.rm = TRUE) +
  scale_fill_manual(values = pal_col, name = "Potential for Species dispersion (A1B)",
                    labels = c("No longer suitable", "Not occupied", "Currently occupied",  "Colonised from 2050", 
                               "Colonised from 2070", "Colonised from 2090", "Colonised from 2100", "Potential colonies", "")) +
  coord_cartesian(xlim = c(-94, -50), ylim = c(37, 65)) +
  scalebar(x.min = -83, x.max = -50, y.min = 39, y.max = 65,
           dist = 500, dd2km = TRUE, model = "WGS84", st.size=4, height=0.015, location = "bottomleft") +
  north(x.min = -93, x.max = -50, y.min = 37 - 1, y.max = 65, symbol = 4,
        location = "topright", scale = 0.06) +
  theme_void() +
  theme(text = element_text(size = 12, colour = "white"),
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_blank(),
        panel.background = element_rect(fill = "transparent"))
plot(A1Bcol_1)

A2col_1 <- gplot(A2col) +
  geom_raster(aes(fill = factor(value)), na.rm = TRUE) +
  scale_fill_manual(values = pal_col, name = "Potential for Species dispersion (A2)",
                    labels = c("No longer suitable", "Not occupied", "Currently occupied",  "Colonised from 2050",
                               "Colonised from 2070", "Colonised from 2090", "Colonised from 2100", "Potential colonies", "")) +
  coord_cartesian(xlim = c(-94, -50), ylim = c(37, 65)) +
  scalebar(x.min = -83, x.max = -50, y.min = 39, y.max = 65,
           dist = 500, dd2km = TRUE, model = "WGS84", st.size=4, height=0.015, location = "bottomleft") +
  north(x.min = -93, x.max = -50, y.min = 37 - 1, y.max = 65, symbol = 4,
        location = "topright", scale = 0.06) +
  theme_void() +
  theme(text = element_text(size = 12, colour = "white"),
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_blank(),
        panel.background = element_rect(fill = "transparent"))
plot(A2col_1)

inset <- ggplotGrob(gplot(Canada) +
                      geom_raster(aes(fill = factor(value)),  na.rm = TRUE) +
                      scale_fill_manual(values = "grey59", labels = c("", ""), guide = FALSE) +
                      theme_void() +
                      annotate("rect", xmin = -94,xmax = -50, ymin = 37, ymax = 65, alpha = 0.2))
# theme(panel.border = element_rect(colour = "black", fill = "transparent")))
plot(inset)

## plotting inset maps
A1Bcol_plot <- A1Bcol_1 +
  annotation_custom(grob = inset, xmin = -62, xmax = -50, ymin = 38, ymax = 44)
plot(A1Bcol_plot)

A2col_plot <- A2col_1 +
  annotation_custom(grob = inset, xmin = -62, xmax = -50, ymin = 38, ymax = 44)
plot(A2col_plot)

ggsave("spread_m/plots/A1Bcolonised.pdf", A1Bcol_plot, width = 8, height = 6)
ggsave("spread_m/plots/A2colonised.pdf", A2col_plot, width = 8, height = 6)