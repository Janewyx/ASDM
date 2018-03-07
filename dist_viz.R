## Code for species distribution map visualisations

library(reshape2) #table to long format
library(ggplot2) #plotting
library(RColorBrewer) #map palette
library(raster) #reading in and processing raster data
library(rasterVis) #2D visualisation of raster
library(ggsn) #scale bar and north arrow on map

## reading in distribution risk category raster cell counts
risk <- read.csv("cell_count.csv")

## reading in all species distribution rasters
## modelled present distribution
pres <- raster("spread_m/Canada_present1.tif")
## modelled distribution in future years under two climate scenarios
rlist=list.files(path = "spread_m/", pattern="tif")
for(i in rlist) { 
  assign(unlist(strsplit(i, "[.]"))[1], raster(i)) 
} 


## getting raster information from present map layer
pres
ratify(pres)

## creating a raster stack for plotting
s <- stack(rlist)

## using WGS84 projection
crs(pres) <- crs("+init=epsg:4326")
crs(s) <- crs("+init=epsg:4326")

## extracting colours from http://colorbrewer2.org/
pal <- brewer.pal(5, "RdYlGn")[4:1]

## creating facet plot labellers
labs <- c("2030", "2050", "2070", "2090", "2100")
labellers <- function(variable,value){
  return(labs[value])
}

## scale bar specifications
xmin <- xmin(s)
xmax <- xmax(s)
ymin <- ymin(s)
ymax <- ymax(s)

## converting .csv table to long format
risk_long <- melt(risk, id.var = "Risk", variable.name = "Time", value.name = "Count")

## plotting current distribution
pres_plot <- gplot(pres) +
  geom_raster(aes(fill = factor(value)), na.rm = TRUE) +
  scale_fill_manual(values = pal,
                    # name = "Potential for Species Distribution (A1B)",
                    labels = c("No Risk", "Low Risk", "Moderate Risk", "High Risk", "")) +
  scalebar(x.min = xmin, x.max = xmax, y.min = ymin+5, y.max = ymax, anchor = c(x=-105, y=45),
           dist = 500, dd2km = TRUE, model = "WGS84", st.size=4, height=0.015, location = "bottomleft") +
  north(x.min = xmin, x.max = xmax, y.min = ymin - 1, y.max = ymax, symbol = 4, 
        location = "topright", scale = 0.06) +
  theme_void() +
  theme(text = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_blank()
        # legend.direction = "horizontal",
        # legend.position = "bottom"
  )
plot(pres_plot)

## plotting distribution map for A1B scenario
A1B_plot <- gplot(s[[1:5]]) +
  geom_raster(aes(fill = factor(value)), na.rm = TRUE) +
  facet_wrap(~variable, labeller = labellers) +
  coord_equal() +
  scale_fill_manual(values = pal,
                    # name = "Potential for Species Distribution (A1B)",
                    labels = c("No Risk", "Low Risk", "Moderate Risk", "High Risk", "")) +
  theme_void() +
  theme(text = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.position = "bottom"
  )
plot(A1B_plot)


## plotting distribution map for A2 scenario
A2_plot <- gplot(s[[6:10]]) +
  geom_raster(aes(fill = factor(value)), na.rm = TRUE) +
  facet_wrap(~variable, labeller = labellers) +
  coord_equal() +
  scale_fill_manual(values = pal,
                    labels = c("No Risk", "Low Risk", "Moderate Risk", "High Risk", ""), 
                    name = "Potential for Species Distribution (A2)") +
  theme_void() +
  theme(text = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.direction = "horizontal",
        legend.text = element_text(size = 12),
        legend.position = "bottom")
plot(A2_plot)

## plotting risk category visualisation
risk_plot <- ggplot(risk_long, aes(Risk, Count, group = Time, colour = Time)) +
  geom_line() +
  scale_y_log10()
plot(risk_plot)

ggsave("spread_m/plots/pres_plot.tiff", pres_plot, width = 10, height = 6)
ggsave("spread_m/plots/A1B_plot.tiff", A1B_plot, width = 10, height = 6)
ggsave("spread_m/plots/A2_plot.tiff", A2_plot, width = 10, height = 6)
