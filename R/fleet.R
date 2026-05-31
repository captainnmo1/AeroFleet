# fleet.R
# defines the 4 aircraft in our fleet using S3 classes
# each aircraft has real world specs and cost data
# all sources listed in DATA_SOURCES.md


#' Create a new aircraft object
#'
#' @param name Aircraft name as a character string
#' @param seats Number of seats as a positive number
#' @param fuel_burn_per_km Fuel burn in kg per km as a positive number
#' @param max_range_km Maximum range in km as a positive number
#' @param fuel_cost_per_kg Fuel price in USD per kg as a positive number
#' @param fixed_cost Fixed cost per flight in USD as a positive number
#' @param hourly_cost Hourly crew and maintenance cost in USD as a positive number
#' @param cruise_speed_kmh Cruise speed in km/h as a positive number
#' @param ground_handling_cost Ground handling cost per turnaround in USD as a positive number
#' @param landing_fee_per_pax Per passenger landing fee in USD, zero for widebody
#' @param widebody_landing_fee Flat landing fee in USD, zero for narrowbody
#' @param daily_lease_cost Daily lease cost in USD as a positive number
#' @return An object of class aircraft
#' @export
new_aircraft <- function(name, seats, fuel_burn_per_km, max_range_km,
                         fuel_cost_per_kg, fixed_cost, hourly_cost,
                         cruise_speed_kmh, ground_handling_cost,
                         landing_fee_per_pax, widebody_landing_fee,
                         daily_lease_cost) {

  # stop the function if any input is wrong
  stopifnot(is.character(name))
  stopifnot(is.numeric(seats),                seats > 0)
  stopifnot(is.numeric(fuel_burn_per_km),     fuel_burn_per_km > 0)
  stopifnot(is.numeric(max_range_km),         max_range_km > 0)
  stopifnot(is.numeric(fuel_cost_per_kg),     fuel_cost_per_kg > 0)
  stopifnot(is.numeric(fixed_cost),           fixed_cost > 0)
  stopifnot(is.numeric(hourly_cost),          hourly_cost > 0)
  stopifnot(is.numeric(cruise_speed_kmh),     cruise_speed_kmh > 0)
  stopifnot(is.numeric(ground_handling_cost), ground_handling_cost > 0)
  stopifnot(is.numeric(landing_fee_per_pax),  landing_fee_per_pax >= 0)
  stopifnot(is.numeric(widebody_landing_fee), widebody_landing_fee >= 0)
  stopifnot(is.numeric(daily_lease_cost),     daily_lease_cost > 0)

  # bundle everything into a list and label it as class aircraft
  # this is how S3 classes work in R
  obj <- list(
    name                 = name,
    seats                = seats,
    fuel_burn_per_km     = fuel_burn_per_km,
    max_range_km         = max_range_km,
    fuel_cost_per_kg     = fuel_cost_per_kg,
    fixed_cost           = fixed_cost,
    hourly_cost          = hourly_cost,
    cruise_speed_kmh     = cruise_speed_kmh,
    ground_handling_cost = ground_handling_cost,
    landing_fee_per_pax  = landing_fee_per_pax,
    widebody_landing_fee = widebody_landing_fee,
    daily_lease_cost     = daily_lease_cost
  )

  class(obj) <- "aircraft"
  return(obj)
}


#' Print an aircraft object
#'
#' @param x An object of class aircraft
#' @param ... Additional arguments (not used)
#' @export
print.aircraft <- function(x, ...) {
  cat("Aircraft:", x$name, "\n")
  cat("Seats:", x$seats, "\n")
  cat("Fuel burn:", x$fuel_burn_per_km, "kg/km\n")
  cat("Max range:", x$max_range_km, "km\n")
  cat("Cruise speed:", x$cruise_speed_kmh, "km/h\n")
  cat("Daily lease cost: $", x$daily_lease_cost, "\n")
}


#' Summarise an aircraft object
#'
#' Shows the full cost breakdown for an aircraft so you can
#' quickly compare different aircraft types.
#'
#' @param object An object of class aircraft
#' @param ... Additional arguments (not used)
#' @export
summary.aircraft <- function(object, ...) {
  cat("--- Aircraft Summary ---\n")
  cat("Name:               ", object$name, "\n")
  cat("Seats:              ", object$seats, "\n")
  cat("Max range:          ", object$max_range_km, "km\n")
  cat("Cruise speed:       ", object$cruise_speed_kmh, "km/h\n")
  cat("\n")
  cat("--- Cost Information ---\n")
  cat("Fuel burn:          ", object$fuel_burn_per_km, "kg/km\n")
  cat("Fuel price:         $", object$fuel_cost_per_kg, "per kg\n")
  cat("Hourly cost:        $", object$hourly_cost, "per hour\n")
  cat("Ground handling:    $", object$ground_handling_cost, "per turnaround\n")
  cat("Fixed cost:         $", object$fixed_cost, "per flight\n")
  cat("Daily lease cost:   $", object$daily_lease_cost, "\n")
  if (object$widebody_landing_fee > 0) {
    cat("Landing fee:        $", object$widebody_landing_fee, "flat fee per landing\n")
  } else {
    cat("Landing fee:        $", object$landing_fee_per_pax, "per passenger\n")
  }
}


# Embraer E190
# fuel burn: block fuel ~4000 lbs/hr from Embraer pilot data = 1814 kg/hr
#            divided by cruise speed 870 km/h = 2.1 kg/km
# daily lease: ~$8,500/day based on ~$255k/month Cirium lease rate data 2024
# sources: flightinfo.com, aerocorner.com, faa.gov, airlinegeeks.com

E190 <- new_aircraft(
  name                 = "Embraer E190",
  seats                = 98,
  fuel_burn_per_km     = 2.1,
  max_range_km         = 4537,
  fuel_cost_per_kg     = 0.90,
  fixed_cost           = 3000,
  hourly_cost          = 1622,
  cruise_speed_kmh     = 870,
  ground_handling_cost = 1400,
  landing_fee_per_pax  = 32,
  widebody_landing_fee = 0,
  daily_lease_cost     = 8500
)


# Boeing 737 MAX 8
# fuel burn: ~2600 kg/hr cruise / 839 km/h = 3.1 kg/km
# daily lease: ~$14,000/day based on ~$420k/month IBA Insight 2024
# sources: boeing.com, theflyingengineer.com, faa.gov

B737MAX <- new_aircraft(
  name                 = "Boeing 737 MAX 8",
  seats                = 162,
  fuel_burn_per_km     = 3.1,
  max_range_km         = 6480,
  fuel_cost_per_kg     = 0.90,
  fixed_cost           = 5000,
  hourly_cost          = 3157,
  cruise_speed_kmh     = 839,
  ground_handling_cost = 1400,
  landing_fee_per_pax  = 32,
  widebody_landing_fee = 0,
  daily_lease_cost     = 14000
)


# Airbus A320neo
# fuel burn: ~2500 kg/hr cruise / 833 km/h = 3.0 kg/km
# daily lease: ~$13,500/day based on ~$405k/month IBA Insight 2024
# sources: airbus.com, theflyingengineer.com, eurocontrol

A320NEO <- new_aircraft(
  name                 = "Airbus A320neo",
  seats                = 165,
  fuel_burn_per_km     = 3.0,
  max_range_km         = 6300,
  fuel_cost_per_kg     = 0.90,
  fixed_cost           = 5000,
  hourly_cost          = 3000,
  cruise_speed_kmh     = 833,
  ground_handling_cost = 1400,
  landing_fee_per_pax  = 32,
  widebody_landing_fee = 0,
  daily_lease_cost     = 13500
)


# Boeing 787-9 Dreamliner
# fuel burn: 6.27 kg/km taken directly from aircraftinvestigation.info, rounded to 6.3
# daily lease: ~$35,000/day based on ~$1.05M/month IBA Insight via simpleflying.com
# widebody landing fee: $6,982 flat per landing at Heathrow (£5,737 converted to USD)
# sources: aircraftinvestigation.info, simpleflying.com, wikipedia

B787 <- new_aircraft(
  name                 = "Boeing 787-9",
  seats                = 296,
  fuel_burn_per_km     = 6.3,
  max_range_km         = 14140,
  fuel_cost_per_kg     = 0.90,
  fixed_cost           = 15000,
  hourly_cost          = 3155,
  cruise_speed_kmh     = 903,
  ground_handling_cost = 3000,
  landing_fee_per_pax  = 0,
  widebody_landing_fee = 6982,
  daily_lease_cost     = 35000
)


# all 4 aircraft stored in one list for easy access in other files

fleet <- list(
  E190    = E190,
  B737MAX = B737MAX,
  A320NEO = A320NEO,
  B787    = B787
)
