##week 3
library(sf)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge)

seine_simp = st_simplify(seine, dTolerance = 2000)  # 2000 m

us_states2163 = st_transform(us_states, 2163)
us_states2163$AREA = as.numeric(us_states2163$AREA)
us_states_simp2 = rmapshaper::ms_simplify(us_states2163, keep = 0.01,
                                          keep_shapes = TRUE)

nz_centroid = st_centroid(nz)
seine_centroid = st_centroid(seine)

nz_pos = st_point_on_surface(nz)
seine_pos = st_point_on_surface(seine)

nz_sfc = st_geometry(nz)
nz_shift = nz_sfc + c(0, 100000)

rotation = function(a){
  r = a * pi / 180 #degrees to radians
  matrix(c(cos(r), sin(r), -sin(r), cos(r)), nrow = 2, ncol = 2)
} 

##exercises
#2

canterbury_cent = st_centroid(canterbury)
canterbury_100km_buff = st_buffer(canterbury_cent, dist = 100000)
canterbury_points_within_100km = st_join(nz_height, canterbury_100km_buff)

#3

nz_combined = st_union(nz)
nz_cent = st_centroid(nz_combined)
nz_to_cant = st_distance(nz_cent, canterbury_cent)

#4
#for this question, I used the rotation formula provided in 5.2.4 pretty closely, replacing relevant
#objects and changing the degrees to 30. 
world_whole = st_union(world)
world_cent = st_centroid(world_whole)
world_flip = (world_whole - world_cent) * rotation(180) + world_cent

us_whole = world %>% filter(name_long == "United States")
us_sfc = st_geometry(us_whole)
us_cent = st_centroid(us_sfc)
us_flipped = (us_sfc - us_cent) * rotation(180) + us_cent

#5
#setting up the circles
b = st_sfc(st_point(c(0, 1)), st_point(c(1, 1)))
b = st_buffer(b, dist = 1)
x = b[1]
y = b[2]

#setting up the points
bb = st_bbox(st_union(x, y))
box = st_as_sfc(bb)
set.seed(2017)
p = st_sample(x = box, size = 10)

#creating region between x and y
x_and_y = st_intersection(x, y)

#using base subsetting operators
p_xy2 = p[x_and_y]

#using st_intersects
sel_p_xy = st_intersects(p, x, sparse = FALSE)[, 1] &
  st_intersects(p, y, sparse = FALSE)[, 1]

#as shown by 
identical(p_xy1, p_xy2)
#these methods produce the same result

#6
us_states_lines = st_cast(us_states, "MULTILINESTRING")
us_states_lines$boundary_length = st_length(us_states_lines)
us_states_lines_ordered = us_states_lines[order(us_states_lines$boundary_length) , ]

#7
ndvi_cropped_rp = crop(ndvi, random_points)
ndvi_cropped_ch = crop(ndvi, ch)

#8
random_points$ndvi_values <- raster::extract(ndvi, random_points)
random_points$buffered_ndvi_values <- raster::extract(ndvi, random_points, buffer = 90, fun = mean)

#9
nz_height_new <- nz_height[nz_height$elevation>3100, ]
template_raster <- raster(extent(nz_height_new), resolution = 3000, 
  crs = st_crs(nz_height_new)$proj4string)

#10
nz_height_raster_count <- rasterize(nz_height_new, template_raster, field = "elevation", fun = "count")
nz_height_raster_agg <- aggregate(nz_height_raster_count, fact = 2, fun = )

