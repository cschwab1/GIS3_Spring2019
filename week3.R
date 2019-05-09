##week3
library(sf)
library(raster)
library(dplyr)
library(spData)

library(RQGIS)
data(random_points)
data(ndvi)
ch = st_combine(random_points) %>% 
  st_convex_hull()

##Ch4 exercises
#1,2
nzdf <- nz

nzdf_sm <- st_simplify(nzdf, dTolerance = 5000)
nzdf_ms <- rmapshaper::ms_simplify(nzdf, keep = .0005)
plot(nzdf_sm[0])

nzh <- nz_height
canterbury = nz %>% filter(Name == "Canterbury")
can_cent <- st_centroid(canterbury)
can_buffer_100km <- st_buffer(can_cent, dist = 100000)
count_can_intersect <- st_intersects(nz_height, can_buffer_100km)

nz_joined <- st_join(nz, nz_height)
nz_count <- count(nz_joined, Name) %>% arrange(desc(n))

#3
data(dem, package = "RQGIS")
dem
summary(dem)
rcl1 = matrix(c(0, 366, 1, 366, 748, 2, 748, 1094, 3), ncol = 3, byrow = TRUE)
dem_rcl = reclassify(dem,  rcl = rcl1)

data(ndvi, package = "RQGIS")
ndvi_stack <- stack(dem, ndvi)
ndvi_zonal <- zonal(ndvi_stack, dem_rcl, fun = "mean") %>% as.data.frame()

#4
r_logo <- raster(system.file("external/rlogo.grd", package = "raster"))
plot(logo)
r_logo

