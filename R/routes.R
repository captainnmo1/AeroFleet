# routes.R
# all 8 routes depart from London Heathrow (LHR)
# coordinates from OurAirports (https://ourairports.com/data/)
# ticket prices are average one way economy fares from Heathrow 2024
# these are average fares not the cheapest - Heathrow is served by full
# service carriers like British Airways not budget airlines
# load factors from UK CAA 2023 and IATA 2023/2024 annual reports


#' Calculate the distance between two airports using the Haversine formula
#'
#' The Haversine formula calculates the real world distance between two points
#' on the surface of the Earth using their latitude and longitude coordinates.
#' This is the standard formula used in aviation for route distances.
#' Source: https://en.wikipedia.org/wiki/Haversine_formula
#'
#' @param lat1 Latitude of the departure airport in degrees
#' @param lon1 Longitude of the departure airport in degrees
#' @param lat2 Latitude of the arrival airport in degrees
#' @param lon2 Longitude of the arrival airport in degrees
#' @return Distance in kilometres as a numeric value
#' @export
haversine_distance <- function(lat1, lon1, lat2, lon2) {
  R <- 6371 # earths radius in km
  lat1 <- lat1 * pi / 180
  lat2 <- lat2 * pi / 180
  lon1 <- lon1 * pi / 180
  lon2 <- lon2 * pi / 180
  dlat <- lat2 - lat1
  dlon <- lon2 - lon1
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
  c <- 2 * asin(sqrt(a))
  return(R * c)
}


routes <- data.frame(

  route_name = c(
    "London to Paris",
    "London to Amsterdam",
    "London to Madrid",
    "London to Rome",
    "London to Dubai",
    "London to New York",
    "London to Singapore",
    "London to Los Angeles"
  ),

  dep_airport = rep("LHR", 8),
  arr_airport = c("CDG", "AMS", "MAD", "FCO", "DXB", "JFK", "SIN", "LAX"),

  # all routes depart from London Heathrow
  dep_lat = rep(51.4775, 8),
  dep_lon = rep(-0.4614, 8),

  # arrival airport coordinates from OurAirports database
  arr_lat = c(49.0097, 52.3086, 40.4719, 41.8003, 25.2532, 40.6413, 1.3644, 33.9425),
  arr_lon = c(2.5478, 4.7639, -3.5626, 12.2389, 55.3657, -73.7781, 103.9915, -118.4081),

  # average one way economy ticket prices in USD for 2024
  # London-Paris $150: Expedia LHR-CDG average 2024
  # London-Amsterdam $160: Kayak LHR-AMS average 2024
  # London-Madrid $200: Kayak LHR-MAD average 2024
  # London-Rome $220: Expedia LHR-FCO average 2024
  # London-Dubai $520: Expedia LHR-DXB average 2024
  # London-New York $500: farecompare.com average 2024
  # London-Singapore $750: Kayak and Expedia average 2024
  # London-Los Angeles $750: farecompare.com average 2024
  avg_ticket_price = c(150, 160, 200, 220, 520, 500, 750, 750),

  # real average load factors from official sources
  # London-Paris 84%: UK CAA load factor data 2023
  # London-Amsterdam 84%: UK CAA load factor data 2023
  # London-Madrid 83%: IATA European carriers 2023
  # London-Rome 83%: IATA European carriers 2023
  # London-Dubai 80%: IATA Middle East carriers 2023
  # London-New York 82%: US DOT transatlantic average 2024
  # London-Singapore 79%: IATA Asia Pacific carriers 2023
  # London-Los Angeles 75%: US DOT LAX-LHR 2024
  load_factor_base = c(0.84, 0.84, 0.83, 0.83, 0.80, 0.82, 0.79, 0.75),

  stringsAsFactors = FALSE
)


# calculate distance for each route using the haversine function

routes$distance_km <- 0

for (i in 1:nrow(routes)) {
  routes$distance_km[i] <- haversine_distance(
    routes$dep_lat[i],
    routes$dep_lon[i],
    routes$arr_lat[i],
    routes$arr_lon[i]
  )
}

routes$distance_km <- round(routes$distance_km)
