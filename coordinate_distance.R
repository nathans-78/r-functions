## Coordinate distance function.
coordinate_distance <- function(r=6371, lat1, lat2, lon1, lon2) {
  # use Haversine formula to calculate distance, currently in kilometers
  # sourced from https://www.movable-type.co.uk/scripts/latlong.html
  phi1 <- lat1*pi/180
  phi2 <- lat2*pi/180
  delta_phi <- (lat1-lat2)*pi/180
  delta_lambda <- (lon1-lon2)*pi/180
  
  a <- (sin(delta_phi/2))^2 + cos(phi1)*cos(phi2)*(sin(delta_lambda/2))^2
  
  c <- 2*atan2(sqrt(a), sqrt(1-a))
  
  d <- r*c
  
  return(d)
}