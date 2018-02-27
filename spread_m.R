## Code for spread data pre-processing and modelling of the species using MigClim package

library(MigClim)
library(raster)

######################################################
################### lOADING DATA #####################
######################################################

## reading in forest land cover data at 250m (original) and 20km converted from the original data;
## DEM; and temperature files
# treed <- raster("canadawide_forest/land_cover/NFI_MODIS250m_kNN_LandCover_VegTreed_v0.tif")
treed_20 <- raster("Can_VegTreed_20.tif")
dem <- raster("canada_dem.asc")
temp_A1B <- raster("bio06_A1B_2030.asc")
temp_A2 <- raster("bio06_A2_2030.asc")

## reading in species distribution data for present and future scenarios
pres <- raster("Canada2010.asc")
rlist=list.files(pattern="asc")
for(i in rlist) { 
  assign(unlist(strsplit(i, "[.]"))[1], raster(i)) 
} 

## creating a raster stack of future distributions (under A1B and A2 scenarios)
A1B <- stack(rlist[1:5])
A2 <- stack(rlist[6:10])

## changing the projection to WGS84
treed_20 <- projectRaster(treed_20, crs = "+init=epsg:4326", res = res(pres), method = "bilinear")
crs(dem) <- crs("+init=epsg:4326")
crs(pres) <-crs("+init=epsg:4326")
crs(A1B) <- crs("+init=epsg:4326")
crs(A2) <- crs("+init=epsg:4326")
crs(temp_A1B) <- crs("+init=epsg:4326")
crs(temp_A2) <- crs("+init=epsg:4326")

## resampling allows two raster to merge, but still can't join the dataframes of the rasters
resample(pres, treed_20)

## matching resolution of treed layer (250m) to the species distribution layers of 20km
# fact <- 20000/250
# treed_20 <- aggregate(treed, fact = fact)

## saving 20km resolution tree cover raster file
# writeRaster(treed_20, "Can_VegTreed_20.tif")

######################################################
##################### DATA PREP ######################
######################################################

## present distribution classified into 0 and 1
iniDist <- reclassify(pres, c(0,0.25,0, 0.25,1,1))
plot(iniDist)

## converting raster files to dataframe for modelling input
a <- rasterToPoints(iniDist)

## future distributions classified into 0 to 1000 for habitat suitability maps
A1BDist <- round(A1B*1000, 0)
A2Dist <- round(A2*1000,0)

## converting to dataframe for modelling input
b <- rasterToPoints(A1BDist)
c <- rasterToPoints(A2Dist)

## reclassifying values into 1 (barrier) and 0 (not barrier) to create barrier files
tree_bar <- reclassify(treed_20, c(0,5,1, 5,100,0))
dem_bar <- reclassify(dem, c(0,2000,0, 2000,6000,1))
temp_A1B_bar <- reclassify(temp_A1B, c(-50,-29,1, -29,10,0))
temp_A2_bar <- reclassify(temp_A2, c(-50,-29,1, -29,10,0))

## converting rasters to dataframes 
treebar_df <- rasterToPoints(tree_bar)
dem_df <- rasterToPoints(dem_bar)
dem_df <- as.data.frame(dem_df)
temp_A1B_df <- rasterToPoints(temp_A1B_bar)
temp_A1B_df <- as.data.frame(temp_A1B_df)
temp_A2_df <- rasterToPoints(temp_A2_bar)
temp_A2_df <- as.data.frame(temp_A2_df)
names(dem_df) <- c("x", "y", "dem_bar")
names(temp_A1B_df) <- c("x", "y", "temp_bar")
names(temp_A2_df) <- c("x", "y", "temp_bar")

## merging all dataframes
A1B_df <- merge(a, b)
A1B_df <- merge(A1B_df, dem_df)
A1B_df <- merge(A1B_df, temp_A1B_df)
A2_df <- merge(a, c)
A2_df <- merge(A2_df, dem_df)
A2_df <- merge(A2_df, temp_A2_df)


names(A1B_df) <- c("Xcoordinate", "Ycoordinate", "iniDist", "A1Bhs1", "A1Bhs2", "A1Bhs3", "A1Bhs4", "A1Bhs5", "dem_bar", "temp_bar")
names(A2_df) <- c("Xcoordinate", "Ycoordinate", "iniDist", "A2hs1", "A2hs2", "A2hs3", "A2hs4", "A2hs5", "dem_bar", "temp_bar")

## outputting dataframes
write.csv(A1B_df, "A1B_df.csv")
write.csv(A2_df, "A2_df.csv")

######################################################
################# MIGCLIM MODELLING ##################
######################################################
MigClim.migrate(iniDist = A1B_df[, 1:3], hsMap = A1B_df[,4:8], rcThreshold = 500, envChgSteps = 5, 
                dispSteps=5, barrier = A1B_df[, 9], simulName = "A1B_MigClimTest", overWrite = TRUE)
MigClim.migrate(iniDist = A2_df[, 1:3], hsMap = A2_df[,4:8], rcThreshold = 500, envChgSteps = 5, 
                dispSteps = 1, barrier = A2_df[, 9], simulName = "A2_MigClimTest", overWrite = TRUE)

## taking a look at dispersal model output rasters
plot(raster("A1B_MigClimTest/A1B_MigClimTest_raster.asc"))
plot(raster("A2_MigClimTest/A2_MigClimTest_raster.asc"))
