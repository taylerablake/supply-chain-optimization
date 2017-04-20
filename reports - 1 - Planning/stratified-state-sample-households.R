source(file.path(getwd(),"lib","latlon2state.R"))

myYear <- 2016

if (!( dir.exists(file.path(getwd(),
                           "data",
                           "census",
                           myYear)) )){
  dir.create(file.path(getwd(),
                       "data",
                       "census",
                       myYear))  
}



nation_geo <- readOGR(dsn=file.path(getwd(),
                      "data",
                      "census",
                      myYear,
                      "cb_2015_us_nation_5m",
                      "cb_2015_us_nation_5m.shp"))

coordinatesattributes(nation_geo)$polygons[[1]]
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
pop_by_state <- transform(pop_by_state,
                          prop_national_pop=total_pop_2016/sum(pop_by_state$total_pop_2016))
names(pop_by_state)[names(pop_by_state)=="GEO.display.label"] <- "NAME"


pop_by_state <- subset(pop_by_state,!(NAME%in%c("American Samoa",
  "Guam",
  "Commonwealth of the Northern Mariana Islands",
  "Puerto Rico")))


## create a uniform grid over the rectangle bounding the lower 48
top <- 48.945
bottom <- 30
left <- -120
right <- -76.8

grid_points <- expand.grid(x=seq(left,right,length.out=2000),
                           y=seq(bottom,top,length.out=1000))
STATE <- latlon2state(grid_points)
grid_points <- grid_points[!is.na(STATE),]
#%>% is.na %>% sum
