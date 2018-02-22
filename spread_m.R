## Code for spread data pre-processing and modelling of the species using MigClim package

library(MigClim)
library(raster)

## reading in forest land cover data at 250m (original) and 20km converted from the original data
treed <- raster("canadawide_forest/land_cover/NFI_MODIS250m_kNN_LandCover_VegTreed_v0.tif")
treed_20 <- raster("Can_VegTreed_20.tif")

## reading in species distribution data for present and future scenarios
pres <- raster("spread_m/Canada_present1.tif")
rlist=list.files(path = "spread_m/", pattern="tif")
for(i in rlist) { 
  assign(unlist(strsplit(i, "[.]"))[1], raster(i)) 
} 

## creating a raster stack of future distributions (under A1B and A2 scenarios)
A1B <- stack(rlist[1:5])
A2 <- stack(rlist[6:10])

## matching resolution of treed layer (250m) to the species distribution layers of 20km
# fact <- 20000/250
# treed_20 <- aggregate(treed, fact = fact)

## saving 20km resolution tree cover raster file
# writeRaster(treed_20, "Can_VegTreed_20.tif")

######################################################
##################### DATA PREP ######################
######################################################

## present distribution classified into 0 and 1
iniDist <- reclassify(pres, c(0,1,0, 1,4,1))
plot(iniDist)

## converting raster files to dataframe for modelling input
a <- rasterToPoints(iniDist)

## future distributions classified into 0 and 1000 for habitat suitability maps
A1BDist <- reclassify(A1B, c(0,1,0, 1,4,1000))
A2Dist <- reclassify(A2, c(0,1,0, 1,4,1000))

## converting to dataframe for modelling input
b <- rasterToPoints(A1BDist)
c <- rasterToPoints(A2Dist)

## merging all dataframes
A1B_df <- merge(a, b)
A2_df <- merge(a, c)
names(A1B_df) <- c("Xcoordinate", "Ycoordinate", "iniDist", "A1Bhs1", "A1Bhs2", "A1Bhs3", "A1Bhs4", "A1Bhs5")
names(A2_df) <- c("Xcoordinate", "Ycoordinate", "iniDist", "A2hs1", "A2hs2", "A2hs3", "A2hs4", "A2hs5")

## outputting dataframes
write.csv(A1B_df, "A1B_df.csv")
write.csv(A1B_df, "A2_df.csv")

## reclassifying values in treed raster wtih 1 indicating barrier (forest coverage <5%) 
## and 0 indicating non-barrier (forest coverage >5%).
tree_bar <- reclassify(treed_20, c(0,5,1, 5,100,0))

## converting to dataframe and outputting as .csv for modelling input 
treebar_df <- rasterToPoints(tree_bar)
write.csv(treebar_df, "treebar_df.csv")

# ## saving as .asc file for modelling input
# writeRaster(iniDist, "iniDist.asc")
# 
# ## habitat suitability classified into 0 and 1000 for all future distributions
# hsMap1 <- reclassify(A1B_20301, c(0,1,0, 1,4,1000))
# hsMap2 <- reclassify(A1B_20501, c(0,1,0, 1,4,1000))
# hsMap3 <- reclassify(A1B_20701, c(0,1,0, 1,4,1000))
# hsMap4 <- reclassify(A1B_20901, c(0,1,0, 1,4,1000))
# hsMap5 <- reclassify(A1B_21001, c(0,1,0, 1,4,1000))
# 
# ## outputing as .asc files for modelling
# writeRaster(hsMap1, "hsMap1.asc")
# writeRaster(hsMap2, "hsMap2.asc")
# writeRaster(hsMap3, "hsMap3.asc")
# writeRaster(hsMap4, "hsMap4.asc")
# writeRaster(hsMap5, "hsMap5.asc")

######################################################
################# MIGCLIM MODELLING ##################
######################################################
MigClim.migrate(iniDist = A1B_df[, 1:3], hsMap = A1B_df[,4:8], envChgSteps = 5, dispSteps = 1, 
                barrier = treebar_df, simulName = "A1B_MigClimTest", overWrite = TRUE)
MigClim.migrate(iniDist = A2_df[, 1:3], hsMap = A1B_df[,4:8], envChgSteps = 5, dispSteps = 1, 
                barrier = treebar_df, simulName = "A2_MigClimTest", overWrite = TRUE)

# MigClim.migrate(iniDist = "inidist.asc", hsMap = "hsMap", envChgSteps = 5, dispSteps = 1, overWrite = TRUE)

