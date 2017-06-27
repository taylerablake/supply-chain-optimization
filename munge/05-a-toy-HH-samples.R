


load(file.path(getwd(),"cache","HH_samples.Rdata"))

toy_HH_samples <- HH_samples[,c("lon","lat","STATE")]
toy_HH_samples$region <- kmeans(HH_samples[,c("lon","lat")],
                                2)$cluster

#Create the fulfillment centers
toy_fulfill_centers_locations <- data.frame(grid_points[c(sample(which(grid_points$lon<quantile(grid_points$lon,probs=0.25)),
                                                             size=1),
                                                      sample(which(grid_points$lon>quantile(grid_points$lon,probs=0.75)),
                                                             size=1)),c("lon","lat")],
                                        fc = 1:2)


map.usa_country <- map_data("usa")
ggplot() +
  geom_polygon(data = map.usa_country,
               aes(x = long, y = lat, group = group),
               fill = "#484848") +
  geom_point(data=toy_HH_samples,
             aes(x=lon,y=lat,colour=factor(region)),
             alpha=0.5,
             size=0.6) +
  geom_text(data=toy_fulfill_centers_locations,
            aes(x=lon,y=lat+2.7,label = paste0("fulfillment\n center ",fc)),
            color = "white", size = 4) +
  geom_point(data=toy_fulfill_centers_locations,
             aes(x=lon,y=lat),
             color = "white",
             size = 3) +
  coord_map(projection = "albers", lat0 = 30, lat1 = 40, xlim = c(-121,-73), ylim = c(25,51)) +
  #labs(title = "Fulfillment center & order locations") +
  theme(panel.background = element_rect(fill = "#292929")
        ,plot.background = element_rect(fill = "#292929")
        ,legend.background = element_rect(fill = "#292929")
        ,legend.key = element_blank()
        ,legend.text=element_text(color="#A1A1A1", size=12)
        ,legend.title=element_text(color="#A1A1A1", size=16)
        ,panel.grid = element_blank()
        ,axis.title = element_blank()
        ,axis.text = element_blank()
        ,axis.ticks = element_blank()
        ,text = element_text(color = "#A1A1A1")
        ,plot.title = element_text(size = 20)) +
  guides(colour=guide_legend("Region",override.aes = list(alpha = 1,size=2))) +
  scale_color_tableau()
  ggsave(file.path(getwd(),"doc", "img", "toy-example-map.png"))

  cache("toy_HH_samples")
  cache("toy_fulfill_centers_locations")
  
  
  
  
  
  
  #Calculate distances from each ordering HH to each FC
  toy_deliver_distances <- as.matrix(dist(rbind(toy_fulfill_centers_locations[, c("lon", "lat")],
                                            toy_HH_samples[, c("lon", "lat")])))
  toy_deliver_distances <- toy_deliver_distances[1:length(unique(toy_fulfill_centers_locations$fc)), 
                                         -(1:length(unique(toy_fulfill_centers_locations$fc)))]
  toy_deliver_distances <- matrix(as.numeric(toy_deliver_distances),
                                  nrow = nrow(toy_fulfill_centers_locations))
  
  #Find the closest FC to each HH
  toy_closest_delivery <- apply(toy_deliver_distances, 2, which.min)
  table(toy_closest_delivery)
  
    

  