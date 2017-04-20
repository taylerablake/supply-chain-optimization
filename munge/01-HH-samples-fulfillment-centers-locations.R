library(maps)
library(maptools)
library(sp)
library(rgdal)
library(ggmap)

source(file.path(getwd(),"lib","latlon2state.R"))
myYear <- 2016



nation_geo <- readOGR(dsn=file.path(getwd(),
                      "data",
                      "census",
                      myYear,
                      "cb_2015_us_nation_5m",
                      "cb_2015_us_nation_5m.shp"))

state_geo <- readOGR(dsn=file.path(getwd(),
                                    "data",
                                    "census",
                                    myYear,
                                    "cb_2016_us_state_5m",
                                    "cb_2016_us_state_5m.shp"))


pop_by_state <- read.csv(file.path(getwd(),
                   "data",
                   "census",
                   myYear,
                   "population_by_state.csv"),
                   header = TRUE)

pop_by_state <- merge(attributes(state_geo)$data,pop_by_state[,c("GEO.display.label","respop72016")],
      by.x="NAME",
      by.y="GEO.display.label")

names(pop_by_state)[names(pop_by_state)=="respop72016"] <- "total_pop_2016" 
names(pop_by_state)[names(pop_by_state)=="GEO.display.label"] <- "STATE"
pop_by_state <- subset(pop_by_state,!(STATE%in%c("American Samoa",
                                                "Guam",
                                                "Commonwealth of the Northern Mariana Islands",
                                                "Puerto Rico")))

## calculate the proportion of the total national population living in each state
pop_by_state <- transform(pop_by_state,
                          prop_national_pop=total_pop_2016/sum(pop_by_state$total_pop_2016))





## create a uniform grid over the rectangle bounding the lower 48 and label
## each point with the corresponding state

top <- 48.945
bottom <- 25
left <- -124
right <- -68

grid_points <- expand.grid(x=seq(left,right,length.out=2000),
                           y=seq(bottom,top,length.out=2000))
STATE <- latlon2state(grid_points)
grid_points <- grid_points[!is.na(STATE),]
grid_points <- transform(grid_points,
                         STATE=str_to_upper(as.character(STATE[!is.na(STATE)])))
rm(STATE)








## sample locations from the uniform grid for each state proportionally to 
## its population share

n_households <- 10000
sample_size_by_state <- ceiling(pop_by_state$prop_national_pop*n_households)

HH_samples <- NULL
for (state.i in 1:length(pop_by_state$STATE)) {
  if (str_to_upper(pop_by_state$STATE[state.i]) %in% unique(grid_points$STATE)) {
    HH_samples <- rbind(HH_samples,
                        grid_points[sample(which(grid_points$STATE==str_to_upper(as.character(pop_by_state$STATE[state.i]))),
                                           sample_size_by_state[state.i],
                                           replace = FALSE),])
  }
}

rm(state_geo)
rm(nation_geo)


#Break the households into regions
n_regions <- 8
clusters <- kmeans(HH_samples[,c("x","y")],
                   n_regions)
HH_samples$region <- clusters$cluster
names(HH_samples)[match(c("x","y"),
                        names(HH_samples))] <- c("lon","lat")

#Create the fulfillment centers
n_fulfill_centers <- 5
fulfill_centers_locations <- data.frame(grid_points[sample(which(!(grid_points$STATE %in%
                                                                     c("MONTANA",
                                                                       "WYOMING",
                                                                       "NORTH DAKOTA",
                                                                       "SOUTH DAKOTA"))),
                                                           size=n_fulfill_centers),c("x","y")],
                                        fc = 1:n_fulfill_centers)
names(fulfill_centers_locations)[match(c("x","y"),
                        names(fulfill_centers_locations))] <- c("lon","lat")


map <- get_map('United States',
                     zoom=4,
               color="bw",
               maptype='hybrid')
ggmap(map, extent = 'device',
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





