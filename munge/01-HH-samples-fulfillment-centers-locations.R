library(maps)
library(maptools)
library(sp)
library(rgdal)
library(ggmap)
library(ggthemes)

source(file.path(getwd(),"lib","latlon2state.R"))

pop_by_state <- read.csv(file.path(getwd(),
                   "data",
                   "census",
                   "2016",
                   "population_by_state.csv"),
                   header = TRUE)
names(pop_by_state)[names(pop_by_state)=="respop72016"] <- "total_pop_2016" 
names(pop_by_state)[names(pop_by_state)=="GEO.display.label"] <- "STATE"

pop_by_state <- subset(pop_by_state,!(STATE%in%c("American Samoa",
                                                 "Guam",
                                                 "Commonwealth of the Northern Mariana Islands",
                                                 "Puerto Rico",
                                                 "Hawaii",
                                                 "Alaska")))

## calculate the proportion of the total national population living in each state
pop_by_state <- transform(pop_by_state,
                          prop_national_pop=total_pop_2016/sum(pop_by_state$total_pop_2016))

## create a uniform grid over the rectangle bounding the lower 48 and label
## each point with the corresponding state

top <- 48.945
bottom <- 25
left <- -124
right <- -68

grid_points <- expand.grid(lon=seq(left,right,length.out=3000),
                           lat=seq(bottom,top,length.out=3000))
STATE <- str_to_title(latlon2state(grid_points))
grid_points <- grid_points[!is.na(STATE),]
grid_points <- transform(grid_points,
                         STATE=STATE[!is.na(STATE)])
rm(STATE)



## sample locations from the uniform grid for each state proportionally to 
## its population share

n_households <- 10000
sample_size_by_state <- ceiling(pop_by_state$prop_national_pop*n_households)

HH_samples <- NULL
for (state.i in 1:length(pop_by_state$STATE)) {
  if (as.character(pop_by_state$STATE[state.i]) %in%
      as.character(unique(grid_points$STATE))) {
    HH_samples <- rbind(HH_samples,
                        grid_points[sample(which(grid_points$STATE==as.character(pop_by_state$STATE[state.i])),
                                           sample_size_by_state[state.i],
                                           replace = TRUE),])
  }
}
rownames(HH_samples) <- 1:nrow(HH_samples)
if (nrow(HH_samples) > 10000) {
  HH_samples <- HH_samples[-sample(1:nrow(HH_samples),
                                   size=nrow(HH_samples)-n_households,
                                   replace=FALSE),]
}

#names(HH_samples)[match(c("x","y"),
#                        names(HH_samples))] <- c("lon","lat")


#Break the households into regions
n_regions <- 8
clusters <- kmeans(HH_samples[,c("lon","lat")],
                   n_regions)
HH_samples$region <- clusters$cluster

#Create the fulfillment centers
n_fulfill_centers <- 5
fulfill_centers_locations <- data.frame(grid_points[c(sample(which(grid_points$STATE=="California"),
                                                    size=1),
                                                    sample(which(grid_points$STATE=="South Dakota"),
                                                           size=1),
                                                    sample(which(grid_points$STATE=="Texas"),
                                                                          size=1),
                                                    sample(which(grid_points$STATE=="Georgia"),
                                                           size=1),
                                                    sample(which(grid_points$STATE=="New York"),
                                                           size=1)),c("lon","lat")],
                                        fc = 1:n_fulfill_centers)

map <- get_map('United States',
                     zoom=4,
               color="bw",
               maptype='hybrid')

ggmap(map,
      darken=0.2) +
  geom_point(data=HH_samples,
             aes(x=lon,
                 y=lat,
                 colour=factor(region)),
             alpha=0.7,
             size=0.6) +
  geom_text(data=fulfill_centers_locations,
            aes(x=lon,y=lat,label = fc),
            color = "white", size = 6) +
  guides(colour=guide_legend("Region")) +
  scale_colour_tableau('tableau10')
rm(grid_points)


cache("HH_samples")
cache("fulfill_centers_locations")





