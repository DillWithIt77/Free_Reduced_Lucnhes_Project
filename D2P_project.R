# install.packages("sfheaders")
library(smerc)
library(spdep)
library(sf)
library(rgdal)
library(ggplot2)
library(rgeos)
library(maptools)
library(RColorBrewer)
library(sfheaders)

lunch_data<-read.csv("C:/Users/17347/Documents/CU Denver Documents/Spatial/HW/lunch_data.csv")
colnames(lunch_data) <- c("NAME","elig","year","percent",'number','total_pop')
co_names<- sf::st_read("C:/Users/17347/Desktop/Datasets for D2P21/CO School Districts/CDPHE_CDOE_School_District_Boundaries/CDPHE_CDOE_School_District_Boundaries.shp")
# co <- readShapePoly("C:/Users/17347/Desktop/Datasets for D2P21/CO School Districts/CDPHE_CDOE_School_District_Boundaries/CDPHE_CDOE_School_District_Boundaries.shp")

lunch.data<-merge(lunch_data,co_names, by="NAME")
centroids<-st_centroid(lunch.data$geometry, byid = TRUE)
centroids_df<-cbind(as.vector(as(centroids, "Spatial")@coords)[1:177],as.vector(as(centroids, "Spatial")@coords)[178:354])
# usable_centroids = rbind(centroids@coords[0:140,1:2],centroids@coords[142:178,1:2])

plot(lunch.data$geometry, border="grey60")
percent = lunch.data$percent
number = lunch.data$number

ggplot(lunch.data$geometry) +
  geom_sf(aes(fill = percent))+
  scale_fill_viridis_c(option = "B")+
  labs(title = "Percentage of Students Eligible for Free/Reduced Lunch (in decimal)")

ggplot(lunch.data$geometry) +
  geom_sf(aes(fill = number))+
  scale_fill_viridis_c(option = "B")+
  labs(title = "Number of Students Eligible for Free/Reduced Lunch")


### CEPP
n_star_mean = mean(lunch.data$total_pop)
n_star_min = min(lunch.data$total_pop)
n_star_max = max(lunch.data$total_pop)
mycol = brewer.pal(4, "Dark2")
for (i in c(5000, 10000, 15000, 20000)){
  cepp = cepp.test(coords = centroids_df,
                       cases = lunch.data$number,
                       pop = lunch.data$total_pop,
                       nstar = i,
                       alpha = 0.01)
  
  # create vector of colors to show results
  # default is white (no clustering)
  clustercol = rep("white", nrow(centroids_df))
  # the most likely cluster locations are lightorange for nstar = 12
  clustercol[cepp$clusters[[1]]$locids] = mycol[1]
  # the most likely cluster locations are lightgreen for nstar = 17
  clustercol[cepp$clusters[[2]]$locids] = mycol[2]
  # the most likely cluster locations are magenta for nstar = 6, 17
  clustercol[cepp$clusters[[3]]$locids] = mycol[3]
  clustercol[cepp$clusters[[4]]$locids] = mycol[4]
  clustercol[cepp$clusters[[5]]$locids] = mycol[5]
  
  
  plot(lunch.data$geometry, border = "grey60", axes = TRUE,
       col = color.clusters(cepp), main = paste("Results for CEPP Method with n*= ", i))
  # basic plot
  # plot(cepp)
  # basic info
  # cepp
  # cluster info
  print(paste("Results for CEPP when n* =", i))
  print(summary(cepp))
  print(clusters(cepp))
  
}



### Besag-Newell
c_star_mean = mean(lunch.data$number)
c_star_min = min(lunch.data$number)
c_star_max = max(lunch.data$number)
mycol = brewer.pal(4, "Dark2")
for (i in c(500, 2500, 5000, 10000)){
  
  bn = bn.test(coords = centroids_df,
                cases = lunch.data$number,
                pop = lunch.data$total_pop,
                cstar = i,
                alpha = 0.01)
  
  # create vector of colors to show results
  # default is white (no clustering)
  clustercol = rep("white", nrow(centroids_df))
  # the most likely cluster locations are lightorange for nstar = 12
  clustercol[bn$clusters[[1]]$locids] = mycol[1]
  # the most likely cluster locations are lightgreen for nstar = 17
  clustercol[bn$clusters[[2]]$locids] = mycol[2]
  # the most likely cluster locations are magenta for nstar = 6, 17
  clustercol[bn$clusters[[3]]$locids] = mycol[3]
  clustercol[bn$clusters[[4]]$locids] = mycol[4]
  
  plot(lunch.data$geometry, border = "grey60", axes = TRUE,
       col = color.clusters(bn), main = paste("Results for Besag-Newell Method with c*= ", i))
  # legend("topright", legend = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4"),lwd = 10, col = color.clusters(bn6))
  
  print(paste("Results for Besag-Newell when c* =", i))
  print(summary(bn))
  print(clusters(bn))
  
}


### Scan Method
for(i in c(0.0001,0.001,0.01,0.1)){
  scan = scan.test(coords = centroids_df,
                     cases = lunch.data$number,
                     pop = lunch.data$total_pop,
                     e =(sum(lunch.data$number)/sum(lunch.data$total_pop))*lunch.data$total_pop,
                     nsim=999,
                     alpha = 0.01,
                     ubpop = i)
  
  # create vector of colors to show results
  # default is white (no clustering)
  # clustercol = rep("white", nrow(centroids_df))
  # # the most likely cluster locations are lightorange for nstar = 12
  # clustercol[scan$clusters[[1]]$locids] = mycol[1]
  # # the most likely cluster locations are lightgreen for nstar = 17
  # clustercol[scan$clusters[[2]]$locids] = mycol[2]
  # # the most likely cluster locations are magenta for nstar = 6, 17
  # clustercol[scan$clusters[[3]]$locids] = mycol[3]
  # clustercol[scan$clusters[[4]]$locids] = mycol[4]
  
  plot(lunch.data$geometry, border = "grey60", axes = TRUE,
       col = color.clusters(scan), main = paste("Results for Spatial Scan Method with ubpop = ", i))
  # legend("topright", legend = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4"),lwd = 10, col = color.clusters(bn6))
  
  print(paste("Results for Spatial Scan when ubpop =", i))
  # results from the test are available in
  print(summary(scan))
  # cluster information
  print(clusters(scan))
}




