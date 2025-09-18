
# install libraries
library(here)
setwd(here::here())

source("TAF - ICES tutorial/Libraries_needed.R")
source("TAF - ICES tutorial/Functions.R")

# assign area of interest
  origin <- c(-2, 50)                 # lower-left corner (long, lat)
  cellsize <- c(0.05, 0.05)           # cell size in degrees
  n_cells <- c(200, 150)              # number of cells in x (long) and y (lat)

  # Compute the sequences of cell centers
  x_coords <- origin[1] + (0:(n_cells[1]-1)) * cellsize[1] + cellsize[1]/2
  y_coords <- origin[2] + (0:(n_cells[2]-1)) * cellsize[2] + cellsize[2]/2

  # Create all combinations
  grid_centers <- expand.grid(x = x_coords, y = y_coords)

  # Function to create a square polygon given center and cellsize
  make_cell <- function(x, y, dx, dy) {
    st_polygon(list(matrix(c(
      x - dx/2, y - dy/2,
      x + dx/2, y - dy/2,
      x + dx/2, y + dy/2,
      x - dx/2, y + dy/2,
      x - dx/2, y - dy/2
    ), ncol = 2, byrow = TRUE)))
  }
  
  # Create list of polygons
  polygons_list <- mapply(make_cell, grid_centers$x, grid_centers$y,
                          MoreArgs = list(dx = cellsize[1], dy = cellsize[2]),
                          SIMPLIFY = FALSE)
  
  # Convert to sf object
  grid_sf <- st_sf(
    mid_long = grid_centers$x,
    mid_lat  = grid_centers$y,
    geometry = st_sfc(polygons_list),
    crs = 4326
  )
  
# assign c-squares
  grid_sf$csquares <- CSquare(grid_sf$mid_long,grid_sf$mid_lat,0.05)

# load data and match with c-squares
  dat <- read.csv("TAF - ICES tutorial/Data/250328_MSFDhabitat_depth.csv")
  grid_sf <- cbind(grid_sf, dat[match(grid_sf$csquares,dat$csquares), c("Ecoregion","EEZ",
                                                                        "area_sqkm","Depth",
                                                                        "Dominant_MSFD")])
  
  # check the map
  ggplot()+geom_sf(data=grid_sf,aes(col=Depth))
  

# now more complicated example of bed stress
bstress <-  read.csv("TAF - ICES tutorial/Data/Shear stress/M2 bed stress.csv")
  
# create sf object
bstress_sf <- st_as_sf(bstress, coords = c("Longitude", "Latitude"), crs = 4326)
bstress_sf <- subset(bstress_sf,bstress_sf$M2.bedstress < 1e5)

# find nearest mid-point
nearest_idx <- st_nearest_feature(grid_sf, bstress_sf)

# Assign M2.bedstress from the nearest midpoint
grid_sf$bedstress <- bstress_sf$M2.bedstress[nearest_idx]
grid_sf$bedstress[is.na(grid_sf$Dominant_MSFD)] <- NA

# check the map
ggplot()+geom_sf(data=grid_sf,aes(col=bedstress))
