haversine_distance <- function(lat1, lon1, lat2, lon2, radius = 3959) {
  #lat1 - latitude of the first location in degrees
  #lon1 - longitude of the first location in degrees
  #lat2 - latitude of the second location in degrees
  #lon2 - longitude of the second location in degrees
  #radius - radius of the sphere (default is earth in miles)
  
  lat1_radians <- lat1 * 2 * pi / 360
  lat2_radians <- lat2 * 2 * pi / 360
  lon1_radians <- lon1 * 2 * pi / 360
  lon2_radians <- lon2 * 2 * pi / 360
  return(2 * radius * 
           asin(sqrt((sin((lat2_radians - lat1_radians) / 2)^2 +
                        cos(lat1_radians) * cos(lat2_radians) * 
                        (sin((lon2_radians - lon1_radians) / 2))^2))))
}
