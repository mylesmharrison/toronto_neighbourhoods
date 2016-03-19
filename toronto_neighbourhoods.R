# Toronto Neighbourhood Population Chloropleths
# Myles Harrison
# March 2016
# http://www.everdayanalytics.ca

library(RColorBrewer)
library(maptools)
library(ggmap)
library(rgeos)

# Read the neighborhood shapefile data and plot
shpfile <- "NEIGHBORHOODS_WGS84_2.shp"
sh <- readShapePoly(shpfile)
plot(sh)

# Add demographic data
# The neighbourhood ID is a string - change it to a integer
sh@data$AREA_S_CD <- as.numeric(sh@data$AREA_S_CD)

# Read in the demographic data and merge on Neighbourhood Id
demo <- read.csv(file="WB-Demographics.csv", header=T)
sh2 <- merge(sh, demo, by.x='AREA_S_CD', by.y='Neighbourhood.Id')

# Set the palette
p <- colorRampPalette(c("white", "red"))(128)
palette(p)

# Scale the total population to the palette
pop <- sh2@data$Total.Population
cols <- (pop - min(pop))/diff(range(pop))*127+1
plot(sh, col=cols)

#RColorBrewer, spectral
p <- colorRampPalette(brewer.pal(11, 'Spectral'))(128)
palette(rev(p))
plot(sh2, col=cols)

#GGPLOT 
points <- fortify(sh, region = 'AREA_S_CD')

# Plot the neighborhoods
toronto <- qmap("Toronto, Ontario", zoom=10)
toronto + geom_polygon(aes(x=long,y=lat, group=group, alpha=0.25), data=points, fill='white') + geom_polygon(aes(x=long,y=lat, group=group), data=points, color='black', fill=NA)

# merge the shapefile data with the social housing data, using the neighborhood ID
points2 <- merge(points, demo, by.x='id', by.y='Neighbourhood.Id', all.x=TRUE)

# Plot
toronto + geom_polygon(aes(x=long,y=lat, group=group, fill=Total.Population), data=points2, color='black') + 
  scale_fill_gradient(low='white', high='red')

# Spectral plot
toronto + geom_polygon(aes(x=long,y=lat, group=group, fill=Total.Population), data=points2, color='black') + 
  scale_fill_distiller(palette='Spectral') + scale_alpha(range=c(0.5,0.5))
