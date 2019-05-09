Census.Data <-read.csv("practicaldata.csv")

library("sp")
library("rgdal")
library("rgeos")
install.packages("spdep")
library(spdep)

Output.Areas <- readOGR(".", "Camden_oa11")
Census.Data <-read.csv("practicaldata.csv")

OA.Census <- merge(Output.Areas, Census.Data, by.x="OA11CD", by.y="OA")
House.Points <- readOGR(".", "Camden_house_sales")

library("tmap")
tm_shape(OA.Census) + tm_fill("Qualification", palette = "Reds", style = "quantile", title = "% with a Qualification") + tm_borders(alpha=.4) 

# Calculate neighbours
neighbours <- poly2nb(OA.Census)
neighbours

plot(OA.Census, border = 'lightgrey')
plot(neighbours, coordinates(OA.Census), add=TRUE, col='red')

neighbours2 <- poly2nb(OA.Census, queen = FALSE)
neighbours2

plot(OA.Census, border = 'lightgrey')
plot(neighbours, coordinates(OA.Census), add=TRUE, col='blue')
plot(neighbours2, coordinates(OA.Census), add=TRUE, col='red')

listw <- nb2listw(neighbours2)
listw

moran <- moran.plot(OA.Census$Qualification, listw = nb2listw(neighbours2, style = "W"))
moran.map <- cbind(OA.Census, local)
local <- localmoran(x = OA.Census$Qualification, listw = nb2listw(neighbours2, style = "W"))
tm_shape(moran.map) + tm_fill(col = "Ii", style = "quantile", title = "local moran statistic") 

### to create LISA cluster map ### 
quadrant <- vector(mode="numeric",length=nrow(local))

# centers the variable of interest around its mean
m.qualification <- OA.Census$Qualification - mean(OA.Census$Qualification)   

# centers the local Moran's around the mean
m.local <- local[,1] - mean(local[,1])

# significance threshold
signif <- 0.1 

# builds a data quadrant
quadrant[m.qualification >0 & m.local>0] <- 4  
quadrant[m.qualification <0 & m.local<0] <- 1      
quadrant[m.qualification <0 & m.local>0] <- 2
quadrant[m.qualification >0 & m.local<0] <- 3
quadrant[local[,5]>signif] <- 0

# plot in r
brks <- c(0,1,2,3,4)
colors <- c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")
plot(OA.Census,border="lightgray",col=colors[findInterval(quadrant,brks,all.inside=FALSE)])
box()
legend("bottomleft",legend=c("insignificant","low-low","low-high","high-low","high-high"),
       fill=colors,bty="n")

# creates centroid and joins neighbours within 0 and 800 units
nb <- dnearneigh(coordinates(OA.Census),0,800)
# creates listw
nb_lw <- nb2listw(nb, style = 'B')

# plot the data and neighbours
plot(OA.Census, border = 'lightgrey')
plot(nb, coordinates(OA.Census), add=TRUE, col = 'red')

# compute Getis-Ord Gi statistic
local_g <- localG(OA.Census$Qualification, nb_lw)
local_g <- cbind(OA.Census, as.matrix(local_g))
names(local_g)[6] <- "gstat"

# map the results
tm_shape(local_g) + tm_fill("gstat", palette = "RdBu", style = "pretty") + tm_borders(alpha=.4)



