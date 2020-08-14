rm(list=ls())
###############
## geocoding function using OSM Nominatim API
## details: http://wiki.openstreetmap.org/wiki/Nominatim
## made by: D.Kisler 
## Code borrowed from: https://datascienceplus.com/osm-nominatim-with-r-getting-locations-geo-coordinates-by-its-address/


nominatim_osm <- function(address = NULL)
{
        if(suppressWarnings(is.null(address)))
                return(data.frame())
        tryCatch(
                d <- jsonlite::fromJSON( 
                        gsub('\\@addr\\@', gsub('\\s+', '\\%20', address), 
                             'http://nominatim.openstreetmap.org/search/@addr@?format=json&addressdetails=0&limit=1')
                ), error = function(c) return(data.frame())
        )
        if(length(d) == 0) return(data.frame())
        return(data.frame(lon = as.numeric(d$lon), lat = as.numeric(d$lat)))
}

###############

library(leaflet)
library(tidyverse)
library(geosphere)
library(viridisLite)

#Read the supply chain location data
supplyChain <- read.csv("SupplyChain.csv")
supplyChain$type<-factor(supplyChain$type)

#Get the coordinates
coord <- sapply(supplyChain$location,nominatim_osm)
coord <- data.frame(matrix(unlist(coord),nrow=length(coord)/2,ncol=2,byrow=TRUE))
names(coord) <- c("lng","lat")
supplyChain <- cbind(supplyChain,coord) %>% 
        mutate(popup=paste("Item Number:", supplyChain$itemNo, "- Company:", supplyChain$company,
                           "<br>Location: ", supplyChain$location, 
                           "<br>Destination: ", supplyChain$destination,
                           "<br>Type: ",supplyChain$type))


domain <- range(as.numeric(supplyChain$type))
pal <- colorNumeric(palette = viridis(100), domain = domain)


myMap <- leaflet(supplyChain) %>%
        addTiles() %>%
        addCircleMarkers(color=~pal(as.numeric(type)),popup=~popup)



for(i in 1:(dim(supplyChain)[1])) {
        if(supplyChain$destination[i]!="0") {
                destVector<-unlist(strsplit(supplyChain$destination[i],split=","))
                for(j in 1:length(destVector)) {
                        d <- as.numeric(destVector[j])
                        start <- c(supplyChain$lng[i], supplyChain$lat[i])
                        end <-   c(supplyChain$lng[d], supplyChain$lat[d])
                        line <- gcIntermediate(start,end, n=50, addStartEnd=TRUE)
                        
                        myMap <- myMap %>%  addPolylines(lng=line[,1],lat=line[,2]) 
                }
        } else {
        }
}

myMap




