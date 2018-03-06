## Code for plotting MigClim spread modelling results

library(ggplot2) #plotting
library(raster) #reading in and processing raster data
library(rasterVis) #2D visualisation of raster
library(RColorBrewer) #map palette
library(ggsn) #scale bar and north arrow on map

## reading in MigClim modelled results in .asc
A1Bdisp <- raster("A1B_MigClimTest/A1B_MigClimTest_raster.asc")
A2disp <- raster("A2_MigClimTest/A2_MigClimTest_raster.asc")

## reclassifying raster values for plotting colonised and occupied maps
# classifying colonised cells during simulation; once occupied, no longer available all assigned -1
A1Bcol <- reclassify(A1Bdisp, c(-600,-1,-1, 200,220,200, 300,320,300, 400,420,400, 500,520,500))
A2col <- reclassify(A2disp, c(-600,-1,-1, 200,220,200, 300,320,300, 400,420,400, 500,520,500))
# classifying once occupied cells; colonised cells during simulation all assigned 0.5 
A1Bocc <- reclassify(A1Bdisp, c(-600,-500,500, -499,-400,400, -399,-300,300, -299,-200,200, -199,-100,100,
                                100,600,0.5))
A2occ <- reclassify(A2disp, c(-600,-500,500, -499,-400,400, -399,-300,300, -299,-200,200, -199,-100,100,
                                100,600,0.5))
Canada <- reclassify(A1Bdisp, c(-600,30000,0))
## scale bar specifications
xmin <- xmin(A1Bcol)
xmax <- xmax(A1Bcol)
ymin <- ymin(A1Bcol)
ymax <- ymax(A1Bcol)

## plotting colonised cells during simulation
## creating palette for plot labels
pal_col <- c("#999999", "black", rev(brewer.pal(6, "Accent")), "transparent")
pal_occ <- c("black", "#999999", rev(brewer.pal(6, "Accent"))[1], brewer.pal(8, "Set1")[4:8],
             rev(brewer.pal(6, "Accent"))[6], "transparent")
pal_occ <- c("black", "#999999", rev(brewer.pal(6, "Accent"))[1], rev(brewer.pal(6, "Set3")[2:6]),
             rev(brewer.pal(6, "Accent"))[6], "transparent")

A1Bcol_1 <- gplot(A1Bcol) +
  geom_raster(aes(fill = factor(value)), na.rm = TRUE) +
  scale_fill_manual(values = pal_col, name = "Potential for Species dispersion (A1B)",
                    labels = c("Once occupied", "Not occupied", "Currently occupied",  "Colonised from 2050", 
                               "Colonised from 2070", "Colonised from 2090", "Colonised from 2100", "Potential colonies", "")) +
  coord_cartesian(xlim = c(-94, -50), ylim = c(37, 65)) +
  scalebar(x.min = -83, x.max = -50, y.min = 39, y.max = 65,
           dist = 500, dd2km = TRUE, model = "WGS84", st.size=4, height=0.015, location = "bottomleft") +
  north(x.min = -93, x.max = -50, y.min = 37 - 1, y.max = 65, symbol = 12,
        location = "topright", scale = 0.06) +
  theme_void() +
  theme(text = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_blank())
plot(A1Bcol_1)

A2col_1 <- gplot(A2col) +
  geom_raster(aes(fill = factor(value)), na.rm = TRUE) +
  scale_fill_manual(values = pal_col, name = "Potential for Species dispersion (A2)",
                   labels = c("Once occupied", "Not occupied", "Currently occupied",  "Colonised from 2050",
                              "Colonised from 2070", "Colonised from 2090", "Colonised from 2100", "Potential colonies", "")) +
  coord_cartesian(xlim = c(-94, -50), ylim = c(37, 65)) +
  scalebar(x.min = -83, x.max = -50, y.min = 39, y.max = 65,
           dist = 500, dd2km = TRUE, model = "WGS84", st.size=4, height=0.015, location = "bottomleft") +
  north(x.min = -93, x.max = -50, y.min = 37 - 1, y.max = 65, symbol = 12,
        location = "topright", scale = 0.06) +
  theme_void() +
  theme(text = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_blank())
plot(A2col_1)

inset <- ggplotGrob(gplot(Canada) +
  geom_raster(aes(fill = factor(value)),  na.rm = TRUE) +
  scale_fill_manual(values = "black", labels = c("", ""), guide = FALSE) +
  theme_void() +
  annotate("rect", xmin = -94,xmax = -50, ymin = 37, ymax = 65, alpha = 0.25))
  # theme(panel.border = element_rect(colour = "black", fill = "transparent")))
plot(inset)


## plotting inset maps
A1Bcol_plot <- A1Bcol_1 +
  annotation_custom(grob = inset, xmin = -62, xmax = -50, ymin = 38, ymax = 44)
plot(A1Bcol_plot)

A2col_plot <- A2col_1 +
  annotation_custom(grob = inset, xmin = -62, xmax = -50, ymin = 38, ymax = 44)
plot(A2col_plot)



## plotting cells that were occupied but no longer suitable during the study years
A1Bocc_plot <- gplot(A1Bocc) +
  geom_raster(aes(fill = factor(value)), na.rm = TRUE) +
  scale_fill_manual(values = pal_occ, name = "Potential for Species dispersion (A1B)",
                    labels = c("Not occupied", "Occupied during simulation", "Currently occupied",  "Occupied in 2030", 
                              "Occupied in 2050", "Occupied in 2070", "Occupied in 2090", "Occupied in 2100", "Potential colonies", "")) +
  scalebar(x.min = xmin, x.max = xmax, y.min = ymin+5, y.max = ymax, anchor = c(x=-105, y=45),
           dist = 500, dd2km = TRUE, model = "WGS84", st.size=4, height=0.015, location = "bottomleft") +
  north(x.min = xmin, x.max = xmax, y.min = ymin - 1, y.max = ymax, symbol = 12, 
        location = "topright", scale = 0.06) +
  theme_void() +
  theme(text = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_blank())
plot(A1Bocc_plot)

A2occ_plot <- gplot(A2occ) +
  geom_raster(aes(fill = factor(value)), na.rm = TRUE) +
  scale_fill_manual(values = pal_occ, name = "Potential for Species dispersion (A2)",
                    labels = c("Not occupied", "Occupied during simulation", "Currently occupied",  "Occupied in 2030", 
                               "Occupied in 2050", "Occupied in 2070", "Occupied in 2090", "Occupied in 2100", "Potential colonies", "")) +
  scalebar(x.min = xmin, x.max = xmax, y.min = ymin+5, y.max = ymax, anchor = c(x=-105, y=45),
           dist = 500, dd2km = TRUE, model = "WGS84", st.size=4, height=0.015, location = "bottomleft") +
  north(x.min = xmin, x.max = xmax, y.min = ymin - 1, y.max = ymax, symbol = 12, 
        location = "topright", scale = 0.06) +
  theme_void() +
  theme(text = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_blank())
plot(A2occ_plot)

ggsave("spread_m/plots/A1Bcolonised.png", A1Bcol_plot, width = 8, height = 6)
ggsave("spread_m/plots/A2colonised.png", A2col_plot, width = 8, height = 6)
ggsave("spread_m/plots/A1Boccupied.png", A1Bocc_plot, width = 10, height = 6)
ggsave("spread_m/plots/A2occupied.png", A2occ_plot, width = 10, height = 6)
