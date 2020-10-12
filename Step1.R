# install libraries
library(rgdal)
library(sp)
library(raster)
library(here)


# Set Folders

here::here()
# [1] "C:/Users/pcoleman/Documents/FBIT2020"

# assign area of interest

gt<-(GridTopology(c(-16.090, 63.891), c(0.05, 0.05), c(200, 150))) # c(long, lat), c(cellsize long, lat), c(nb of grids long, lat)
grt<-SpatialGrid(gt, proj4string=CRS("+init=epsg:4326"))
spix <- as(grt, "SpatialPixels")
spol <- as(spix, "SpatialPolygons")
rnames<-sapply(slot(spol, "polygons"), function(x) slot(x, "ID"))
LOCUNI<-as.data.frame(seq(1,length(spix)))
rownames(LOCUNI)<-rnames
bargrid<-SpatialPolygonsDataFrame(spol, LOCUNI)
bargrid@bbox # make sure "min" is a whole number

# assign c-squares
#source(paste(pathdir, "Utilities/coords_to_csquare_VMStools.R",sep="/"))
source(here("FBIT_2020/FBIT-dev/TAF - ICES tutorial/Utilities/coords_to_csquare_VMStools.R"))
coord <- coordinates(bargrid)
squares<-CSquare(coord[,1],coord[,2],0.05)
bargrid@data$csquares <- squares

# assign EEZ
# shapeEEZ <- readOGR(dsn = paste(path_env,"EEZ_land_union_v2_201410",sep="/") ,layer="EEZ_land_v2_201410")
shapeEEZ <- readOGR(dsn = here("Data/Envirn_Layers/EEZ_land_union_v2_201410"),layer= "EEZ_land_v2_FBIT") ## custom layer (CelticSea, Bay of Biscay and Iberian EEZ's, PC2020)
plot(shapeEEZ)
shapeEEZ@proj4string # check coordinates reference system
shapeEEZ <- spTransform(shapeEEZ,CRS(proj4string(bargrid))) # make it similar to bargrid
shapeEEZ@proj4string # check coordinates reference system again
tr <- over(bargrid,shapeEEZ)
bargrid@data$EEZ <- tr$Country 

# assign ICES ecoregions
# shapeEcReg <- readOGR(dsn = paste(path_env,"ICES_ecoregions",sep="/") ,layer="ICES_ecoregions_20171207_erase_ESRI")
shapeEcReg <- readOGR(dsn = here("Data/Envirn_Layers/ICES_ecoregions"), layer="Celtic_FBIT_Eco") # custom layer (CelticSea, Bay of Biscay and Iberian EcoRegions, PC2020)
plot(shapeEcReg)
shapeEcReg@proj4string # check coordinates reference system
shapeEcReg <- spTransform(shapeEcReg,CRS(proj4string(bargrid))) # make it similar to bargrid
shapeEcReg@proj4string # check coordinates reference system again
tr <- over(bargrid,shapeEcReg)
bargrid@data$EcReg <- tr$Ecoregion 

# assign MSFD habitats
# fgdb <- paste(path_env,"EUSM2019_EUNIS_BroadscaleModel.gdb", sep="/")
fgdb <- here("Data/Envirn_Layers/EUSM2019_EUNIS_BroadscaleModel.gdb")

subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
print(fc_list)

# select the region (slow)
EUSeaMap2019 <- readOGR(dsn=fgdb,layer="EUSM_FBIT_HABS") # EUSM_FBIT_HABS custom layer (CelticSea, Bay of Biscay and Iberian Habitats, PC2020)
# plot(EUSeaMap2019) # Takes a long time to plot, don't run unless you really need to see.

# transform bargrid (the other way around takes a long time)
coord<-coordinates(bargrid)
coord <-as.data.frame(coord)
colnames(coord)<- c("Longitude", "Latitude")
coordinates(coord)<- ~ Longitude + Latitude  
crs(coord) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
coord <- spTransform(coord,CRS(proj4string(EUSeaMap2019)))
tr<-over(coord,EUSeaMap2019) # (slow)
bargrid@data$MSFDhab<-tr$MSFD_BBHT 

# Save bargrid
# setwd(path_env)
# save(bargrid,file="region_grid.RData")

# remove big file(s)
rm(EUSeaMap2019)

# assign OSPAR reporting units
# fgdb <- paste(path_env,"OSPAR_Reporting_Units_20180813.gdb", sep="/")
fgdb <- here("Data/Envirn_Layers/OSPAR_Reporting_Units_20180813.gdb")
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
print(fc_list)

# select the ospar habitat level: OSPAR Reporting Units; 
# Level 0 - OSPAR area, Level 1 - OSPAR regions 
# Level 2 - Subset of OSPAR regions, Level 3 - Zone of coastal influence
# Level 4 - WFD regions/amalgam of WFD region

OSPARlevel2 <- readOGR(dsn=fgdb,layer="OSPAR_RU_Level2_v5_170215")
OSPARlevel2@proj4string # check coordinates reference system
OSPARlevel2 <- spTransform(OSPARlevel2,CRS(proj4string(bargrid))) # make it similar to bargrid
OSPARlevel2@proj4string # check coordinates reference system again
tr <- over(bargrid,OSPARlevel2)
bargrid@data$OSPARL2 <- tr$L2_Region_

# now remove all c-squares on land
bargrid <- subset(bargrid,!(is.na(bargrid@data$MSFDhab)))


# save bargrid
# setwd(path_env)
save(bargrid, file = here("Data/Envirn_Layers/region_grid.RData"))












