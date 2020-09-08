
# install libraries
  library(rgdal)
  library(sp)
  library(raster)
  
# set folder directory
  pathdir <- "C:/Users/pdvd/Online for git/FBIT/TAF - ICES tutorial" # path to tutorial folder
  path_env <- paste(pathdir,"Step 1 Assign region of interest/Data layers", sep="/") # path to environmental data layers 
  
# assign area of interest
  gt<-(GridTopology(c(-1.975, 50.025), c(0.05, 0.05), c(200, 150))) # c(long, lat), c(cellsize long, lat), c(nb of grids long, lat)
  grt<-SpatialGrid(gt, proj4string=CRS("+init=epsg:4326"))
  spix <- as(grt, "SpatialPixels")
  spol <- as(spix, "SpatialPolygons")
  rnames<-sapply(slot(spol, "polygons"), function(x) slot(x, "ID"))
  LOCUNI<-as.data.frame(seq(1,length(spix)))
  rownames(LOCUNI)<-rnames
  bargrid<-SpatialPolygonsDataFrame(spol, LOCUNI)
  bargrid@bbox # make sure "min" is a whole number

# assign c-squares
  source(paste(pathdir, "Utilities/coords_to_csquare_VMStools.R",sep="/"))
  coord <- coordinates(bargrid)
  squares<-CSquare(coord[,1],coord[,2],0.05)
  bargrid@data$csquares <- squares
 
# assign EEZ
  shapeEEZ <- readOGR(dsn = paste(path_env,"EEZ_land_union_v2_201410",sep="/") ,layer="EEZ_land_v2_201410")
  plot(shapeEEZ)
  shapeEEZ@proj4string # check coordinates reference system
  shapeEEZ <- spTransform(shapeEEZ,CRS(proj4string(bargrid))) # make it similar to bargrid
  shapeEEZ@proj4string # check coordinates reference system again
  tr <- over(bargrid,shapeEEZ)
  bargrid@data$EEZ <- tr$Country 
  
# assign ICES ecoregions
  shapeEcReg <- readOGR(dsn = paste(path_env,"ICES_ecoregions",sep="/") ,layer="ICES_ecoregions_20171207_erase_ESRI")
  #plot(shapeEcReg)
  shapeEcReg@proj4string # check coordinates reference system
  shapeEcReg <- spTransform(shapeEcReg,CRS(proj4string(bargrid))) # make it similar to bargrid
  shapeEcReg@proj4string # check coordinates reference system again
  tr <- over(bargrid,shapeEcReg)
  bargrid@data$EcReg <- tr$Ecoregion 

# assign MSFD habitats
  fgdb <- paste(path_env,"EUSM2019_EUNIS_BroadscaleModel.gdb", sep="/")
  subset(ogrDrivers(), grepl("GDB", name))
  fc_list <- ogrListLayers(fgdb)
  print(fc_list)
  
  # select the region (slow)
  EUSeaMap2019 <- readOGR(dsn=fgdb,layer="EUSM_Arctic_Atlantic")
  
  # transform bargrid (the other way around takes a long time)
  coord<-coordinates(bargrid)
  coord <-as.data.frame(coord)
  colnames(coord)<- c("Longitude", "Latitude")
  coordinates(coord)<- ~ Longitude + Latitude  
  crs(coord) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  coord <- spTransform(coord,CRS(proj4string(EUSeaMap2019)))
  tr<-over(coord,EUSeaMap2019) # (slow)
  bargrid@data$MSFDhab<-tr$MSFD_BBHT 
  
# save bargrid
  setwd(path_env)
  save(bargrid,file="region_grid.RData")  
  
# remove big file(s)
  rm(EUSeaMap2019)
  
# assign OSPAR reporting units
  fgdb <- paste(path_env,"OSPAR_Reporting_Units_20180813.gdb", sep="/")
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
  setwd(path_env)
  save(bargrid,file="region_grid.RData")  
  
