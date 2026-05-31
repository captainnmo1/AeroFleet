# simulate.R
# core profit simulation for AeroFleet
# calculates per-flight profit AND daily profit for each aircraft on each route
#
# costs included per flight:
# 1. fuel cost = distance x fuel burn per km x fuel price per kg
# 2. hourly crew and maintenance = hourly rate x flight time in hours
# 3. ground handling = fixed cost per turnaround
# 4. fixed cost per flight = navigation fees, admin, insurance
# 5. landing fees = per passenger for narrowbody, flat fee for widebody
# 6. lease cost per flight = daily lease divided by flights per day
#    this is the key insight - a 787 leasing at $35,000/day doing only
#    4 short flights pays $8,750 lease per flight vs an A320neo doing
#    8 flights paying only $1,688 per flight
#
# turnaround times used:
#   E190: 35 mins = 0.58 hrs (regional jet standard)
#   737 MAX and A320neo: 45 mins = 0.75 hrs (ADS-B data, eplaneai.com)
#   787-9: 120 mins = 2.0 hrs (widebody standard, aileronair.com)
#
# sources:
#   lease costs: https://simpleflying.com/airbus-boeing-narrowbody-lease-rates-compared/
#   widebody landing fees LHR: https://simpleflying.com/the-cost-of-flying/
#   turnaround times: https://www.aileronair.com/blog/aircraft-turnaround-time-aviation/
#   daily utilization: https://aerodata.ai/aircraft-utilization-rate/
#   CAA load factors: https://www.caa.co.uk/data-and-analysis/uk-aviation-market/airports/uk-airport-load-factor-data/
#   IATA load factors: https://www.iata.org/en/pressroom/2024-releases/2024-01-31-02/
#   Heathrow fees: https://airlinegeeks.com/2023/05/18/london-s-heathrow-airport-recorded-substantial-recovery-amid-controversial-landing-fees/


#' Get turnaround time in hours for a given aircraft
#'
#' @param aircraft_name Aircraft name as a character string
#' @return Turnaround time in hours as a numeric value
#' @export
get_turnaround_hours <- function(aircraft_name) {
  if (aircraft_name == "Embraer E190") {
    return(0.58)
  } else if (aircraft_name %in% c("Boeing 737 MAX 8", "Airbus A320neo")) {
    return(0.75)
  } else {
    return(2.0)
  }
}


#' Simulate profit for one aircraft on one route
#'
#' Runs a Monte Carlo simulation using random daily load factor variation
#' around the real average load factor from CAA and IATA data.
#' Returns per-flight and daily profit figures.
#'
#' @param aircraft An object of class aircraft
#' @param route A single row data frame from the routes data frame
#' @param n_simulations Number of simulations to run, default is 1000
#' @return A list containing profit, revenue, cost and load factor results
#' @export
calculate_profit <- function(aircraft, route, n_simulations = 1000) {

  # defensive programming - stop if inputs are wrong
  stopifnot(inherits(aircraft, "aircraft"))
  stopifnot(is.data.frame(route))
  stopifnot(nrow(route) == 1)
  stopifnot(is.numeric(n_simulations), n_simulations > 0)

  # check if aircraft can physically reach this route
  if (route$distance_km > aircraft$max_range_km) {
    message(aircraft$name, " cannot fly this route - out of range!")
    return(NULL)
  }

  # flight time = distance divided by cruise speed plus 0.5 hours for
  # taxi, climb and descent - this is standard aviation block time
  flight_time_hrs <- (route$distance_km / aircraft$cruise_speed_kmh) + 0.5

  # how many flights can this aircraft complete in a 14 hour operating day
  turnaround_hrs  <- get_turnaround_hours(aircraft$name)
  flights_per_day <- floor(14 / (flight_time_hrs + turnaround_hrs))
  flights_per_day <- max(1, flights_per_day)

  # lease cost is spread equally across all flights done that day
  lease_per_flight <- aircraft$daily_lease_cost / flights_per_day

  # these costs are the same for every simulated flight so we calculate once
  fuel_cost    <- route$distance_km * aircraft$fuel_burn_per_km * aircraft$fuel_cost_per_kg
  hourly_total <- aircraft$hourly_cost * flight_time_hrs
  handling     <- aircraft$ground_handling_cost
  fixed        <- aircraft$fixed_cost

  # widebody uses a flat landing fee, narrowbody uses per passenger fee
  widebody_fee <- aircraft$widebody_landing_fee

  # load factor base from real CAA and IATA data stored in routes.R
  base_lf <- route$load_factor_base

  # generate all random daily variations at once instead of one at a time
  # runif(n_simulations) gives us all 1000 random numbers in a single call
  daily_variations  <- runif(n_simulations, min = -0.06, max = 0.06)

  # calculate all 1000 load factors at once using vector addition
  load_factors_used <- base_lf + daily_variations

  # keep every load factor between 0.65 and 0.95
  # pmin and pmax are the vectorised versions of min and max
  # they work on the whole vector at once instead of one value at a time
  load_factors_used <- pmax(0.65, pmin(0.95, load_factors_used))

  # calculate passengers and revenue for all 1000 simulations at once
  passengers <- round(aircraft$seats * load_factors_used)
  revenues   <- passengers * route$avg_ticket_price

  # landing fees - widebody pays a flat fee, narrowbody pays per passenger
  # for narrowbody this produces a vector of 1000 landing fee values
  if (widebody_fee > 0) {
    landing_fees <- widebody_fee
  } else {
    landing_fees <- passengers * aircraft$landing_fee_per_pax
  }

  # calculate profit for all 1000 simulations in one line
  # R applies the arithmetic to every element of the vector automatically
  total_costs <- fuel_cost + hourly_total + handling + fixed + landing_fees + lease_per_flight
  profits     <- revenues - total_costs

  avg_profit_per_flight <- round(mean(profits))

  results <- list(
    aircraft_name    = aircraft$name,
    route_name       = route$route_name,
    distance_km      = route$distance_km,
    flight_time_hrs  = round(flight_time_hrs, 1),
    flights_per_day  = flights_per_day,
    avg_profit       = avg_profit_per_flight,
    daily_profit     = round(avg_profit_per_flight * flights_per_day),
    best_profit      = round(max(profits)),
    worst_profit     = round(min(profits)),
    avg_revenue      = round(mean(revenues)),
    fuel_cost        = round(fuel_cost),
    hourly_cost      = round(hourly_total),
    handling_cost    = round(handling),
    fixed_cost       = round(fixed),
    lease_per_flight = round(lease_per_flight),
    avg_load_factor  = round(mean(load_factors_used) * 100, 1),
    profitable       = mean(profits) > 0
  )

  return(results)
}
