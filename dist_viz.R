## Code for modelling the distribution and spread of forest invasive species 
## with pre-processed species distribution data from Vivek Srivastava for the
## present period, 2030, 2050, 2070, 2090, and 2100 under climate scenarios A1B 
## and A2 in Canada.
## Created as a part of GEM 599 proof of concept project by Jane Wang under the 
## supervision of Vivek Srivastava, Feb 16, 2018

library(ggplot2)
library(RColorBrewer) #map palette
library(raster) #reading in and processing raster data
library(rasterVis) #2D visualisation of raster

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

## plotting distribution map for A1B scenario
A1B_plot <- gplot(s[[1:5]]) +
  geom_raster(aes(fill = factor(value)), na.rm = TRUE) +
  facet_wrap(~variable, labeller = labellers) +
  coord_equal() +
  scale_fill_manual(values = pal,
                    labels = c("No Risk", "Low Risk", "Moderate Risk", "High Risk", ""), 
                    name = "Potential for species Distribution (A1B)") +
  # ggsn::scalebar(x.min = xmin, x.max = xmax, y.min = ymin, y.max = ymax, dist = 5000,
                 # facet.var = "variable") +
  theme_void() +
  theme(legend.direction = "horizontal",
        legend.position = "bottom")
plot(A1B_plot)
# A1B_map <- A1B_plot +
  # scalebar(data = s[[1:5]], dist = 1000)

## plotting distribution map for A2 scenario
A2_plot <- gplot(s[[6:10]]) +
  geom_raster(aes(fill = factor(value)), na.rm = TRUE) +
  facet_wrap(~variable, labeller = labellers) +
  coord_equal() +
  scale_fill_manual(values = pal,
                    labels = c("No Risk", "Low Risk", "Moderate Risk", "High Risk", ""), 
                    name = "Potential for species Distribution (A2)") +
  theme_void() +
  theme(text = element_text(size = 12),
        legend.direction = "horizontal",
        legend.position = "bottom")
plot(A2_plot)

ggsave("spread_m/plots/A1B_plot.png", A1B_plot, width = 10, height = 6)
ggsave("spread_m/plots/A2_plot.png", A2_plot, width = 10, height = 6)