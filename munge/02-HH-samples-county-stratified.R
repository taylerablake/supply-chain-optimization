
library(maps)
library(maptools)
library(sp)
library(rgdal)
library(ggmap)

source(file.path(getwd(),"lib","latlon2state.R"))

myYear <- 2016
pop_by_county <- read.csv(file.path(getwd(),
                                   "data",
                                   "census",
                                   myYear,
                                   "co-est2016-alldata.csv"),
                         header = TRUE)
pop_by_county <- pop_by_county[,c("SUMLEV",
                                  "STATE",
                                  "STNAME",
                                  "COUNTY",
                                  "CTYNAME",
                                  "POPESTIMATE2016")] %>%
  transform(.,STATENAME=str_to_lower(STNAME))


counties <- map('county', fill=TRUE, col="transparent", plot=FALSE)
county_df <- counties$names %>%
  str_split(.,",") %>%
  ldply(.,function(c) {data.frame(state=c[1],county=c[2])}) %>%
  ddply(.,.(state),transform,
        COUNTY=1:length(county))
table(county_df$state)
sum(pop_by_county$STNAME=="California")



