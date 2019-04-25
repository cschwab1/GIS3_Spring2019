
library(sf)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge)

rotation = function(a){
  r = a * pi / 180 #degrees to radians
  matrix(c(cos(r), sin(r), -sin(r), cos(r)), nrow = 2, ncol = 2)
} 

world_whole = st_union(world)
world_cent = st_centroid(world_whole)

world_flip = (world_whole - world_cent) * rotation(180) + world_cent
