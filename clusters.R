suppressPackageStartupMessages(library(sp,warn.conflicts = FALSE))
suppressPackageStartupMessages(library(sf,warn.conflicts = FALSE))
suppressPackageStartupMessages(library(rgdal,warn.conflicts = FALSE))
suppressPackageStartupMessages(library(geosphere,warn.conflicts = FALSE))
suppressPackageStartupMessages(library(dplyr,warn.conflicts = FALSE))
suppressPackageStartupMessages(library(leaflet,warn.conflicts = FALSE))
suppressPackageStartupMessages(library(ggplot2,warn.conflicts = FALSE))
suppressPackageStartupMessages(library(xaringan,warn.conflicts = FALSE))
suppressPackageStartupMessages(library(renderthis,warn.conflicts = FALSE))

airbnb_listings_tmp<-read.csv("/home/lina/Desktop/Interviews/listings.csv.gz")

airbnb_listings<-airbnb_listings_tmp %>% filter(latitude>-45 & longitude>175)

set.seed(123)

index <- sample(1:nrow(airbnb_listings), 1000)

airbnb_listings_north<-airbnb_listings[index, ]

nrow(airbnb_listings)
nrow(airbnb_listings_north)

x<-airbnb_listings_north$longitude
y<-airbnb_listings_north$latitude

# convert data to a SpatialPointsDataFrame object
xy <- SpatialPointsDataFrame(
  matrix(c(x,y), ncol=2), data.frame(ID=seq(1:length(x))),
  proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

# use the distm function to generate a geodesic distance matrix in meters
mdist <- distm(xy)

# cluster all points using a hierarchical clustering approach
hc <- hclust(as.dist(mdist), method="complete")

# plot(hc)
# abline(h = 5000, col = 'green')
# ggplot(as.data.frame(xy), aes(x=longitude, y = latitude, color = factor(clust))) + geom_point() + theme(legend.position = "none")
 
# define the distance threshold, in this case  5km - 5000m
d=5000

# define clusters based on a tree "height" cutoff "d" and add them to the SpDataFrame
xy$clust <- cutree(hc, h=d)

xy$longitude<-x
xy$latitude<-y

xy_sp<- st_as_sf(xy) %>% filter(duplicated(clust) | duplicated(clust,fromLast=TRUE)) %>% 
  group_by(clust) %>% 
  mutate(clust_fnl= cur_group_id()) %>% 
  ungroup() %>% 
  arrange(clust_fnl) %>%
  dplyr::select(-c("clust")) 


leaflet()%>% addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(data=xy_sp ,color='olive',radius=4,stroke=FALSE,fillOpacity=1) %>% 
  addMarkers(data=xy_sp ,clusterOptions = markerClusterOptions())


renderthis::to_pdf("MBI_report_linaberbesi.Rmd")

xaringan::decktape("MBI_report_linaberbesi.html", "xaringan.pdf", docker = FALSE)







