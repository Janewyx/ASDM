## Code for modelling the distribution and spread of forest invasive species 
## with pre-processed species distribution data from Vivek Srivastava for the
## present period, 2030, 2050, 2070, 2090, and 2100 under climate scenarios A1B 
## and A2 in Canada.
## Created as a part of GEM 599 proof of concept project by Jane Wang under the 
## supervision of Vivek Srivastava, Feb 16, 2018

setwd("Users/jane/Documents/Caffeine/MGEM/GEM_599/AGM/data/")

library(ggplot2)
library(RColorBrewer) #map palette
library(raster) #reading in and processing raster data
library(rasterVis) #2D visualisation of raster

## reading in all AGM distribution rasters
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

## plotting distribution map for A1B scenario
gplot(s[[1:5]]) +
  geom_raster(aes(fill = factor(value)), na.rm = TRUE) +
  facet_wrap(~variable, labeller = labellers) +
  coord_equal() +
  scale_fill_manual(values = pal,
                    labels = c("No Risk", "Low Risk", "Moderate Risk", "High Risk", ""), 
                    name = "Potential for AGM\nDistribution(A1B)") +
  theme_void()

## plotting distribution map for A2 scenario
gplot(s[[6:10]]) +
  geom_raster(aes(fill = factor(value)), na.rm = TRUE) +
  facet_wrap(~variable, labeller = labellers) +
  coord_equal() +
  scale_fill_manual(values = pal,
                    labels = c("No Risk", "Low Risk", "Moderate Risk", "High Risk", ""), 
                    name = "Potential for AGM\nDistribution(A2)") +
  theme_void()
