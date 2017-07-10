
source(file.path(getwd(),"lib","latlon2state.R"))

#This code generates three datasets:
#1. Demand: state * scenario - Expected demand in each state.  Uses
# full state names.
#2. Allocation: scenario * FC - All possible allocations and their attendant
# values
#3. Location: FC - latitude and longitude.

library(ProjectTemplate)
load.project()

set.seed(1977)

#Set a time range
n_time_periods <- 100

#Create all the households------------------
zips <- readShapePoly("data/census/2016/cb_2016_us_zcta510_500k/cb_2016_us_zcta510_500k",
                      proj4string = CRS("+proj=longlat +datum=WGS84"),
                      IDvar = "GEOID10")

#Subset to continental US
us_range <- map("usa", plot = FALSE)$range
x_range <- us_range[2] - us_range[1]
y_range <- us_range[4] - us_range[3]
us_range[1] <- us_range[1] - .05 * x_range
us_range[2] <- us_range[2] + .05 * x_range
us_range[3] <- us_range[3] - .05 * y_range
us_range[4] <- us_range[4] + .05 * y_range
temp <- zips
to_null <- rep(FALSE, length(zips))
for (i in 1:length(zips)) {
  if (i %% 1000 == 0)
    cat(i, "of", length(zips), "\n")
  if (zips[i, ]@bbox[1, 1] < us_range[1] |
      zips[i, ]@bbox[1, 2] > us_range[2] |
      zips[i, ]@bbox[2, 1] < us_range[3] |
      zips[i, ]@bbox[2, 2] > us_range[4])
    to_null[i] <- TRUE
}
zips <- temp[!to_null, ]

#Loop through zips, generating observations proportional to actual
# population
pop_data <- read.csv("data/census/DEC_10_SF2_PCT1_with_ann.csv",
                     skip = 1)
pop_data <- pop_data[pop_data$Id == 1,]
pop_data$GEOID10 <- gsub(".*([0-9]{5})$", "\\1", pop_data$Geography)

household_locations <- vector("list", nrow(zips))
for (i in 1:nrow(zips)) {
  if (i %% 1000 == 0)
    cat(i, "of", nrow(zips), "\n")
  
  n_points <- ceiling(.001 * pop_data$Total[pop_data$GEOID10 == zips$GEOID10[i]])
  
  if (length(n_points) > 0) {
    this_zip_polygon <- zips[i, ]
    candidate_points <- data.frame(x = runif(10 * n_points, this_zip_polygon@bbox[1, 1],
                                             this_zip_polygon@bbox[1, 2]),
                                   y = runif(10 * n_points, this_zip_polygon@bbox[2, 1],
                                             this_zip_polygon@bbox[2, 2]))
    # Convert pointsDF to a SpatialPoints object 
    pointsSP <- SpatialPoints(candidate_points, 
                              proj4string=CRS("+proj=longlat +datum=WGS84"))
    
    # Use 'over' to get _indices_ of the Polygons object containing each point 
    indices <- over(pointsSP, this_zip_polygon)
    final_locations <- data.frame(GEOID10 = zips$GEOID10[i],
                                  (candidate_points[!is.na(indices), ])[1:n_points, ])
    household_locations[[i]] <- final_locations
  }
}
household_locations <- do.call(rbind.data.frame, household_locations)

#Cut out people who didn't get x and y due to wacky zip shapes
household_locations <- household_locations[!is.na(household_locations$x),]

#Add state info
states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
states_sp <- map2SpatialPolygons(states, IDs=states$names,
                                 proj4string=CRS("+proj=longlat +datum=WGS84"))
pointsSP <- SpatialPoints(household_locations[, c("x", "y")], 
                          proj4string=CRS("+proj=longlat +datum=WGS84"))
household_locations$state <- states$names[over(pointsSP, states_sp)]

#Cut any households that didn't map to a state
household_locations <- household_locations[!is.na(household_locations$state),]

us_gg_map <- get_map(location = c(lon = -97.30, lat = 39.64),
                     maptype = "roadmap",
                     zoom = 3)
ggmap(us_gg_map) +
  geom_point(data = household_locations, aes(x = x, y = y), color = "#000000",
             size = .1)
ggsave("reports - 1 - Planning/microstrategy-symposium-images/household-locations.png",
       height = 33, width = 55, limitsize = FALSE)

household_locations$state_fips <- state.fips$fips[match(household_locations$state, 
                                                        state.fips$polyname)]
household_locations$state <- str_to_title(gsub("(.*):.*", "\\1", household_locations$state)) #Get rid of stuff after colons, and make title case

#Break the households into regions

household_locations$region <- NA
household_locations$region[household_locations$state %in% c(unique(household_locations$state[household_locations$x > max(household_locations$x[household_locations$state%in%c("Minnesota",
                                                    "Iowa", "Oklahoma",
                                                    "Kansas", "Missouri","Louisiana")])]),
  c("Minnesota",
    "Iowa", "Oklahoma",
    "Kansas", "Missouri","Louisiana"))] <- "East Region"
household_locations$region[is.na(household_locations$region)] <- "West Region"




#Create the fulfillment centers
n_fulfill_centers <- 2
minnesota_row <- sample(which(household_locations$state=="Minnesota"),size=1)
cali_row <- sample(which(household_locations$state=="California"),size=1)
fulfill_centers_locations <- data.frame(fc=c(1:n_fulfill_centers),
                                        x=household_locations$x[c(cali_row,minnesota_row)],
                                        y=household_locations$y[c(cali_row,minnesota_row)])


map.usa_country <- map_data("usa")
ggplot() +
  geom_polygon(data = map.usa_country,
               aes(x = long, y = lat, group = group),
               fill = "#484848") +
  geom_point(data=HH_samples,
             aes(x=lon,
                 y=lat,
                 colour=factor(region)),
             alpha=0.7,
             size=0.6) +
  coord_map(projection = "albers", lat0 = 30, lat1 = 40, xlim = c(-121,-73), ylim = c(25,51)) +
  labs(title = "Rivers and waterways of the United States") +
  theme(panel.background = element_rect(fill = "#292929")
        ,plot.background = element_rect(fill = "#292929")
        ,panel.grid = element_blank()
        ,axis.title = element_blank()
        ,axis.text = element_blank()
        ,axis.ticks = element_blank()
        ,text = element_text(family = "Gill Sans", color = "#A1A1A1")
        ,plot.title = element_text(size = 34)
  ) 

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





