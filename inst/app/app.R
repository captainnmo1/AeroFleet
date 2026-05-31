# app.R
# this is the main file that runs the whole shiny dashboard
# it pulls in the aircraft, routes and simulation logic from the other files
# then builds the UI and connects everything in the server

options(shiny.autoload.r = FALSE)

library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(DT)
library(scales)
library(leaflet)
library(png)

source("fleet.R")
source("routes.R")
source("simulate.R")


# generates curved great circle points between two airports
# makes the map lines look like real flight paths instead of straight lines
great_circle_points <- function(lon1, lat1, lon2, lat2, n = 50) {
  lon1 = lon1 * pi / 180
  lat1 = lat1 * pi / 180
  lon2 = lon2 * pi / 180
  lat2 = lat2 * pi / 180
  d = 2 * asin(sqrt(sin((lat2 - lat1) / 2)^2 + cos(lat1) * cos(lat2) * sin((lon2 - lon1) / 2)^2))
  if (d == 0) return(data.frame(lon = lon1 * 180 / pi, lat = lat1 * 180 / pi))
  f = seq(0, 1, length.out = n)
  A = sin((1 - f) * d) / sin(d)
  B = sin(f * d) / sin(d)
  x = A * cos(lat1) * cos(lon1) + B * cos(lat2) * cos(lon2)
  y = A * cos(lat1) * sin(lon1) + B * cos(lat2) * sin(lon2)
  z = A * sin(lat1) + B * sin(lat2)
  lat_out = atan2(z, sqrt(x^2 + y^2)) * 180 / pi
  lon_out = atan2(y, x) * 180 / pi
  return(data.frame(lon = lon_out, lat = lat_out))
}


ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "AeroFleet Simulator"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Simulation", tabName = "simulation", icon = icon("chart-bar")),
      menuItem("Flight Map", tabName = "map", icon = icon("globe")),
      menuItem("Route Checker", tabName = "checker", icon = icon("search")),
      menuItem("Custom Simulator", tabName = "custom", icon = icon("sliders-h")),
      menuItem("Fleet Manager", tabName = "fleet_mgr", icon = icon("plane")),
      menuItem("Route Manager", tabName = "route_mgr", icon = icon("map-marked-alt")),
      menuItem("Reports & Downloads", tabName = "reports", icon = icon("download"))
    )
  ),

  dashboardBody(
    tabItems(

      # Tab 1 - simulation
      tabItem(tabName = "simulation",
        fluidRow(
          column(3,
            box(width = 12, status = "primary",
              h4("Select a Route"),
              selectInput(inputId = "selected_route", label = NULL, choices = routes$route_name),
              br(),
              actionButton(inputId = "run_sim", label = "Run Simulation",
                style = "background-color: #3c8dbc; color: white; width: 100%; border-radius: 4px; padding: 8px;"),
              br(), br(),
              uiOutput("route_info"),
              br(),
              p("Runs 1000 random passenger load scenarios per aircraft. Load factors from real CAA and IATA data. Daily profit accounts for how many flights each aircraft can complete per day including lease costs.",
                style = "color: grey; font-size: 11px; line-height: 1.5;")
            )
          ),
          column(9,
            fluidRow(uiOutput("stat_boxes")),
            br(),
            fluidRow(
              box(title = "Profit per Flight - hover for details", status = "primary", solidHeader = TRUE, width = 6,
                plotlyOutput("profit_chart", height = "300px")),
              box(title = "Daily Profit - key metric airlines use", status = "success", solidHeader = TRUE, width = 6,
                plotlyOutput("daily_profit_chart", height = "300px"))
            ),
            fluidRow(
              box(title = "Revenue vs All Costs Breakdown - hover for details", status = "warning", solidHeader = TRUE, width = 6,
                plotlyOutput("cost_chart", height = "300px")),
              box(title = "Best Case vs Worst Case Range", status = "danger", solidHeader = TRUE, width = 6,
                plotlyOutput("range_chart", height = "300px"))
            ),
            fluidRow(
              box(title = "How Load Factor Affects Profit - select aircraft", status = "info", solidHeader = TRUE, width = 6,
                selectInput(inputId = "selected_aircraft", label = NULL,
                  choices = c("Embraer E190", "Boeing 737 MAX 8", "Airbus A320neo", "Boeing 787-9")),
                plotlyOutput("load_curve", height = "250px")),
              box(title = "Daily Profit Across All Routes - select aircraft", status = "primary", solidHeader = TRUE, width = 6,
                plotlyOutput("route_comparison", height = "250px"))
            ),
            fluidRow(
              box(title = "Route Info", status = "info", solidHeader = TRUE, width = 4,
                uiOutput("route_details")),
              box(title = "Full Results - click any column to sort", status = "primary", solidHeader = TRUE, width = 8,
                DTOutput("results_table"))
            )
          )
        )
      ),

      # Tab 2 - flight map
      tabItem(tabName = "map",
        fluidRow(
          box(title = "Flight Route Map - all routes from London Heathrow",
            status = "primary", solidHeader = TRUE, width = 12,
            fluidRow(
              column(4, selectInput(inputId = "map_route", label = "Select a Route to Highlight",
                choices = c("Show All Routes", routes$route_name))),
              column(4, br(), p("Click on any airport marker to see its details.",
                style = "color: grey; font-size: 12px;"))
            ),
            leafletOutput("flight_map", height = "550px")
          )
        )
      ),

      # Tab 3 - route checker
      tabItem(tabName = "checker",
        fluidRow(
          box(title = "Aircraft Route Checker", status = "danger", solidHeader = TRUE, width = 12,
            fluidRow(
              column(4, selectInput(inputId = "check_aircraft", label = "Select Aircraft",
                choices = c("Embraer E190", "Boeing 737 MAX 8", "Airbus A320neo", "Boeing 787-9"))),
              column(4, selectInput(inputId = "check_route", label = "Select Route",
                choices = routes$route_name)),
              column(4, br(), actionButton(inputId = "check_btn", label = "Check Route",
                style = "background-color: #e74c3c; color: white; width: 100%; border-radius: 4px; padding: 8px;"))
            ),
            br(),
            uiOutput("checker_result")
          )
        )
      ),

      # Tab 4 - custom simulator
      tabItem(tabName = "custom",
        fluidRow(
          column(6,
            box(title = "Custom Aircraft", status = "primary", solidHeader = TRUE, width = 12,
              fluidRow(
                column(6, textInput("custom_name", "Aircraft Name", value = "My Aircraft")),
                column(6, numericInput("custom_seats", "Seats", value = 180, min = 50, max = 600))
              ),
              fluidRow(
                column(6, numericInput("custom_range", "Max Range (km)", value = 5000, min = 500, max = 16000)),
                column(6, numericInput("custom_speed", "Cruise Speed (km/h)", value = 840, min = 700, max = 1000))
              ),
              fluidRow(
                column(6, numericInput("custom_fuel_burn", "Fuel Burn per km (kg)", value = 3.5, min = 1, max = 12, step = 0.1)),
                column(6, numericInput("custom_fuel_cost", "Fuel Cost per kg ($)", value = 0.88, min = 0.5, max = 2, step = 0.01))
              ),
              fluidRow(
                column(6, numericInput("custom_lease", "Daily Lease Cost ($)", value = 13500, min = 1000, max = 100000, step = 500)),
                column(6, numericInput("custom_hourly", "Hourly Crew & Maintenance ($)", value = 1800, min = 500, max = 10000, step = 100))
              ),
              fluidRow(
                column(6, numericInput("custom_handling", "Ground Handling per Flight ($)", value = 800, min = 100, max = 5000, step = 50)),
                column(6, numericInput("custom_fixed", "Fixed Cost per Flight ($)", value = 400, min = 100, max = 3000, step = 50))
              ),
              fluidRow(
                column(6, numericInput("custom_landing_pax", "Landing Fee per Pax ($)", value = 22, min = 0, max = 100, step = 1)),
                column(6, numericInput("custom_widebody_fee", "Widebody Flat Landing Fee ($, 0 if none)", value = 0, min = 0, max = 20000, step = 100))
              )
            )
          ),
          column(6,
            box(title = "Custom Route", status = "success", solidHeader = TRUE, width = 12,
              fluidRow(
                column(6, numericInput("custom_distance", "Route Distance (km)", value = 1500, min = 200, max = 16000, step = 50)),
                column(6, numericInput("custom_ticket", "Avg Ticket Price ($)", value = 180, min = 20, max = 2000, step = 10))
              ),
              fluidRow(
                column(6, numericInput("custom_load", "Avg Load Factor (%)", value = 82, min = 50, max = 99, step = 1)),
                column(6, numericInput("custom_sims", "Number of Simulations", value = 1000, min = 100, max = 5000, step = 100))
              ),
              br(),
              actionButton("run_custom", "Run Custom Simulation",
                style = "background-color: #27ae60; color: white; width: 100%; border-radius: 4px; padding: 10px; font-size: 14px;"),
              br(), br(),
              p("Enter your own aircraft specifications and route details above, then click Run Custom Simulation.",
                style = "color: grey; font-size: 11px; line-height: 1.5;")
            )
          )
        ),
        fluidRow(
          column(12, uiOutput("custom_result_boxes")),
          column(6, uiOutput("custom_result_detail")),
          column(6, plotlyOutput("custom_load_curve", height = "300px"))
        )
      ),

      # Tab 5 - fleet manager
      tabItem(tabName = "fleet_mgr",
        fluidRow(
          box(title = "Fleet Manager - click any aircraft to view or edit",
            status = "primary", solidHeader = TRUE, width = 12,
            p("Click a row to expand its details. Edit any field and press Save Changes. Use the form below to add a new aircraft.",
              style = "color: grey; font-size: 12px;"),
            br(),
            DTOutput("fleet_table"),
            br(),
            uiOutput("fleet_edit_panel")
          )
        ),
        fluidRow(
          box(title = "Add New Aircraft", status = "success", solidHeader = TRUE, width = 12,
            fluidRow(
              column(3, textInput("new_ac_name", "Aircraft Name", value = "")),
              column(3, numericInput("new_ac_seats", "Seats", value = 150, min = 50, max = 600)),
              column(3, numericInput("new_ac_range", "Max Range (km)", value = 5000, min = 500, max = 16000)),
              column(3, numericInput("new_ac_speed", "Cruise Speed (km/h)", value = 840, min = 600, max = 1100))
            ),
            fluidRow(
              column(3, numericInput("new_ac_fuel_burn", "Fuel Burn per km (kg)", value = 3.0, min = 1, max = 12, step = 0.1)),
              column(3, numericInput("new_ac_fuel_cost", "Fuel Cost per kg ($)", value = 0.90, min = 0.5, max = 2, step = 0.01)),
              column(3, numericInput("new_ac_lease", "Daily Lease Cost ($)", value = 13500, min = 1000, max = 100000, step = 500)),
              column(3, numericInput("new_ac_hourly", "Hourly Cost ($)", value = 2000, min = 500, max = 10000, step = 100))
            ),
            fluidRow(
              column(3, numericInput("new_ac_handling", "Ground Handling ($)", value = 1400, min = 100, max = 5000, step = 50)),
              column(3, numericInput("new_ac_fixed", "Fixed Cost per Flight ($)", value = 5000, min = 100, max = 20000, step = 100)),
              column(3, numericInput("new_ac_landing_pax", "Landing Fee per Pax ($)", value = 32, min = 0, max = 100, step = 1)),
              column(3, numericInput("new_ac_widebody", "Widebody Landing Fee ($, 0 if none)", value = 0, min = 0, max = 20000, step = 100))
            ),
            br(),
            actionButton("add_aircraft_btn", "Add Aircraft to Fleet",
              style = "background-color: #27ae60; color: white; border-radius: 4px; padding: 8px 20px;"),
            br(), br(),
            uiOutput("fleet_add_msg")
          )
        )
      ),

      # Tab 6 - route manager
      tabItem(tabName = "route_mgr",
        fluidRow(
          box(title = "Route Manager - click any route to view or edit",
            status = "primary", solidHeader = TRUE, width = 12,
            p("Click a row to expand its details. Edit any field and press Save Changes. Use the form below to add a new route.",
              style = "color: grey; font-size: 12px;"),
            br(),
            DTOutput("route_table"),
            br(),
            uiOutput("route_edit_panel")
          )
        ),
        fluidRow(
          box(title = "Add New Route", status = "success", solidHeader = TRUE, width = 12,
            fluidRow(
              column(4, textInput("new_rt_name", "Route Name (e.g. London to Tokyo)", value = "")),
              column(4, textInput("new_rt_arr_airport", "Arrival Airport Code (e.g. NRT)", value = "")),
              column(4, numericInput("new_rt_ticket", "Avg Ticket Price ($)", value = 300, min = 20, max = 5000, step = 10))
            ),
            fluidRow(
              column(3, numericInput("new_rt_arr_lat", "Arrival Latitude", value = 0, min = -90, max = 90, step = 0.0001)),
              column(3, numericInput("new_rt_arr_lon", "Arrival Longitude", value = 0, min = -180, max = 180, step = 0.0001)),
              column(3, numericInput("new_rt_load", "Avg Load Factor (%)", value = 80, min = 50, max = 99, step = 1)),
              column(3, br(), p("Distance is calculated automatically from coordinates.",
                style = "color: grey; font-size: 11px;"))
            ),
            br(),
            actionButton("add_route_btn", "Add Route",
              style = "background-color: #27ae60; color: white; border-radius: 4px; padding: 8px 20px;"),
            br(), br(),
            uiOutput("route_add_msg")
          )
        )
      ),

      # Tab 7 - reports and downloads
      tabItem(tabName = "reports",
        fluidRow(
          box(title = "Download Simulation Results as CSV", status = "primary", solidHeader = TRUE, width = 6,
            p("Run a simulation on the Simulation tab first, then come here to download the full results table as a CSV file."),
            br(),
            downloadButton("download_csv", "Download Results CSV",
              style = "background-color: #3c8dbc; color: white; border-radius: 4px; padding: 8px 20px;")
          ),
          box(title = "Download Fleet & Routes Summary as CSV", status = "info", solidHeader = TRUE, width = 6,
            p("Downloads the current state of your fleet and routes including any edits or additions you have made."),
            br(),
            downloadButton("download_fleet_csv", "Download Fleet CSV",
              style = "background-color: #3c8dbc; color: white; border-radius: 4px; padding: 8px 20px; margin-right: 10px;"),
            downloadButton("download_routes_csv", "Download Routes CSV",
              style = "background-color: #3c8dbc; color: white; border-radius: 4px; padding: 8px 20px;")
          )
        ),
        fluidRow(
          box(title = "Download PDF Report", status = "success", solidHeader = TRUE, width = 12,
            p("Select a route below then click Generate PDF. The report includes all simulation results, cost breakdown, profit summary and charts."),
            br(),
            fluidRow(
              column(4, selectInput("pdf_route", "Select Route", choices = routes$route_name)),
              column(4, br(), downloadButton("download_pdf", "Generate & Download PDF",
                style = "background-color: #27ae60; color: white; border-radius: 4px; padding: 8px 20px;"))
            ),
            br(),
            uiOutput("pdf_preview")
          )
        )
      )

    )
  )
)


server <- function(input, output, session) {

  # reactive fleet and routes - stored as reactiveVal so any edits update everywhere instantly
  fleet_rv <- reactiveVal(fleet)
  routes_rv <- reactiveVal(routes)

  # update all aircraft dropdowns when fleet changes
  observe({
    current_fleet = fleet_rv()
    aircraft_names = sapply(current_fleet, function(x) x$name)
    updateSelectInput(session, "selected_aircraft", choices = aircraft_names)
    updateSelectInput(session, "check_aircraft", choices = aircraft_names)
  })

  # update all route dropdowns when routes change
  observe({
    current_routes = routes_rv()
    route_names = current_routes$route_name
    updateSelectInput(session, "selected_route", choices = route_names)
    updateSelectInput(session, "check_route", choices = route_names)
    updateSelectInput(session, "map_route", choices = c("Show All Routes", route_names))
    updateSelectInput(session, "pdf_route", choices = route_names)
  })


  # simulation - only runs when Run Simulation button is clicked
  sim_results <- eventReactive(input$run_sim, {
    current_routes = routes_rv()
    current_fleet = fleet_rv()
    selected = current_routes[current_routes$route_name == input$selected_route, ]
    results_list = list()
    for (i in 1:length(current_fleet)) {
      aircraft = current_fleet[[i]]
      result = calculate_profit(aircraft, selected)
      if (!is.null(result)) {
        results_list[[length(results_list) + 1]] = result
      }
    }
    df = data.frame(
      aircraft = sapply(results_list, function(x) x$aircraft_name),
      avg_profit = sapply(results_list, function(x) x$avg_profit),
      daily_profit = sapply(results_list, function(x) x$daily_profit),
      flights_per_day = sapply(results_list, function(x) x$flights_per_day),
      best_profit = sapply(results_list, function(x) x$best_profit),
      worst_profit = sapply(results_list, function(x) x$worst_profit),
      avg_revenue = sapply(results_list, function(x) x$avg_revenue),
      fuel_cost = sapply(results_list, function(x) x$fuel_cost),
      hourly_cost = sapply(results_list, function(x) x$hourly_cost),
      handling_cost = sapply(results_list, function(x) x$handling_cost),
      fixed_cost = sapply(results_list, function(x) x$fixed_cost),
      lease_per_flight = sapply(results_list, function(x) x$lease_per_flight),
      avg_load_factor = sapply(results_list, function(x) x$avg_load_factor),
      flight_time = sapply(results_list, function(x) x$flight_time_hrs)
    )
    return(df)
  })


  output$route_info <- renderUI({
    current_routes = routes_rv()
    selected = current_routes[current_routes$route_name == input$selected_route, ]
    if (nrow(selected) == 0) return(NULL)
    div(
      p(style = "font-size: 12px; margin: 2px 0;", paste("Distance:", selected$distance_km, "km")),
      p(style = "font-size: 12px; margin: 2px 0;", paste("Avg ticket: $", selected$avg_ticket_price)),
      p(style = "font-size: 12px; margin: 2px 0;", paste("Real avg load factor:", round(selected$load_factor_base * 100), "%")),
      p(style = "font-size: 12px; margin: 2px 0;", paste("To:", selected$arr_airport))
    )
  })


  output$stat_boxes <- renderUI({
    df = sim_results()
    profitable = df[df$daily_profit > 0, ]
    if (nrow(profitable) > 0) {
      best_row = profitable[which.max(profitable$daily_profit), ]
      best_name = best_row$aircraft
      best_val = paste0("$", format(best_row$daily_profit, big.mark = ","))
    } else {
      best_name = "None profitable"
      best_val = "All losing money"
    }
    worst_row = df[which.min(df$daily_profit), ]
    profitable_count = sum(df$daily_profit > 0)
    tagList(
      valueBox(value = best_name, subtitle = "Best Aircraft (Daily Profit)", icon = icon("plane"), color = "blue", width = 3),
      valueBox(value = best_val, subtitle = "Best Daily Profit", icon = icon("dollar-sign"), color = "green", width = 3),
      valueBox(value = paste0(profitable_count, " of ", nrow(df)), subtitle = "Aircraft Profitable on This Route", icon = icon("check-circle"), color = "light-blue", width = 3),
      valueBox(value = worst_row$aircraft, subtitle = "Least Profitable Aircraft", icon = icon("times-circle"), color = "red", width = 3)
    )
  })


  output$route_details <- renderUI({
    df = sim_results()
    current_routes = routes_rv()
    selected = current_routes[current_routes$route_name == input$selected_route, ]
    profitable = df[df$daily_profit > 0, ]
    if (nrow(profitable) > 0) {
      best_row = profitable[which.max(profitable$daily_profit), ]
      rec_name = span(best_row$aircraft, style = "color: #2ecc71; font-weight: bold;")
      rec_daily = span(paste0("$", format(best_row$daily_profit, big.mark = ",")), style = "color: #2ecc71; font-weight: bold;")
      rec_flight = span(paste0("$", format(best_row$avg_profit, big.mark = ",")), style = "color: #2ecc71;")
    } else {
      rec_name = span("None profitable", style = "color: #e74c3c;")
      rec_daily = span("N/A", style = "color: #e74c3c;")
      rec_flight = span("N/A", style = "color: #e74c3c;")
    }
    tagList(
      p(strong("Route: "), input$selected_route),
      p(strong("Distance: "), paste(selected$distance_km, "km")),
      p(strong("Ticket price: "), paste0("$", selected$avg_ticket_price)),
      p(strong("Real load factor: "), paste0(round(selected$load_factor_base * 100), "%")),
      hr(),
      p(strong("Aircraft tested: "), nrow(df)),
      p(strong("Out of range: "), length(fleet_rv()) - nrow(df)),
      hr(),
      p(strong("Best aircraft: "), rec_name),
      p(strong("Best daily profit: "), rec_daily),
      p(strong("Per flight: "), rec_flight)
    )
  })


  output$profit_chart <- renderPlotly({
    df = sim_results()
    p = ggplot(df, aes(x = reorder(aircraft, avg_profit), y = avg_profit, fill = avg_profit > 0,
      text = paste("Aircraft:", aircraft,
        "<br>Per Flight: $", format(avg_profit, big.mark = ","),
        "<br>Flights/Day:", flights_per_day,
        "<br>Lease/Flight: $", format(lease_per_flight, big.mark = ","),
        "<br>Load Factor:", avg_load_factor, "%",
        "<br>Flight Time:", flight_time, "hrs"))) +
      geom_bar(stat = "identity", width = 0.5) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.7) +
      scale_fill_manual(values = c("TRUE" = "#3c8dbc", "FALSE" = "#e74c3c"), guide = "none") +
      scale_y_continuous(labels = comma) +
      coord_flip() +
      labs(x = "", y = "Profit per Flight (USD)") +
      theme_minimal() +
      theme(axis.text = element_text(size = 10))
    ggplotly(p, tooltip = "text") %>% layout(hoverlabel = list(bgcolor = "white"))
  })


  output$daily_profit_chart <- renderPlotly({
    df = sim_results()
    p2 = ggplot(df, aes(x = reorder(aircraft, daily_profit), y = daily_profit, fill = daily_profit > 0,
      text = paste("Aircraft:", aircraft,
        "<br>Daily Profit: $", format(daily_profit, big.mark = ","),
        "<br>Flights/Day:", flights_per_day,
        "<br>Per Flight: $", format(avg_profit, big.mark = ","),
        "<br>Lease/Flight: $", format(lease_per_flight, big.mark = ",")))) +
      geom_bar(stat = "identity", width = 0.5) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.7) +
      scale_fill_manual(values = c("TRUE" = "#2ecc71", "FALSE" = "#e74c3c"), guide = "none") +
      scale_y_continuous(labels = comma) +
      coord_flip() +
      labs(x = "", y = "Daily Profit (USD)") +
      theme_minimal() +
      theme(axis.text = element_text(size = 10))
    ggplotly(p2, tooltip = "text") %>% layout(hoverlabel = list(bgcolor = "white"))
  })


  output$cost_chart <- renderPlotly({
    df = sim_results()
    cost_df = data.frame(
      aircraft = rep(df$aircraft, 5),
      category = c(rep("Revenue", nrow(df)), rep("Fuel", nrow(df)),
        rep("Crew & Maintenance", nrow(df)), rep("Handling & Fees", nrow(df)),
        rep("Lease Cost", nrow(df))),
      value = c(df$avg_revenue, df$fuel_cost, df$hourly_cost,
        df$handling_cost + df$fixed_cost, df$lease_per_flight)
    )
    p3 = ggplot(cost_df, aes(x = aircraft, y = value, fill = category,
      text = paste(category, ": $", format(value, big.mark = ",")))) +
      geom_bar(stat = "identity", position = "dodge", width = 0.6) +
      scale_fill_manual(values = c("Revenue" = "#2ecc71", "Fuel" = "#e74c3c",
        "Crew & Maintenance" = "#f39c12", "Handling & Fees" = "#9b59b6", "Lease Cost" = "#1abc9c")) +
      scale_y_continuous(labels = comma) +
      labs(x = "", y = "USD", fill = "") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 15, size = 9), legend.position = "top")
    ggplotly(p3, tooltip = "text") %>% layout(hoverlabel = list(bgcolor = "white"))
  })


  output$range_chart <- renderPlotly({
    df = sim_results()
    p4 = ggplot(df, aes(x = reorder(aircraft, avg_profit),
      text = paste("Aircraft:", aircraft,
        "<br>Best Case: $", format(best_profit, big.mark = ","),
        "<br>Avg Profit: $", format(avg_profit, big.mark = ","),
        "<br>Worst Case: $", format(worst_profit, big.mark = ",")))) +
      geom_linerange(aes(ymin = worst_profit, ymax = best_profit), color = "#3c8dbc", linewidth = 2) +
      geom_point(aes(y = avg_profit), color = "white", size = 3) +
      geom_point(aes(y = best_profit), color = "#2ecc71", size = 2) +
      geom_point(aes(y = worst_profit), color = "#e74c3c", size = 2) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.7) +
      scale_y_continuous(labels = comma) +
      coord_flip() +
      labs(x = "", y = "Profit per Flight (USD)") +
      theme_minimal() +
      theme(axis.text = element_text(size = 10))
    ggplotly(p4, tooltip = "text") %>% layout(hoverlabel = list(bgcolor = "white"))
  })


  output$load_curve <- renderPlotly({
    current_routes = routes_rv()
    current_fleet = fleet_rv()
    selected = current_routes[current_routes$route_name == input$selected_route, ]
    aircraft_obj = NULL
    for (i in 1:length(current_fleet)) {
      if (current_fleet[[i]]$name == input$selected_aircraft) {
        aircraft_obj = current_fleet[[i]]
      }
    }
    if (is.null(aircraft_obj)) return(NULL)
    if (selected$distance_km > aircraft_obj$max_range_km) {
      return(plotly_empty() %>% layout(title = "Aircraft cannot reach this route"))
    }
    flight_time_hrs = (selected$distance_km / aircraft_obj$cruise_speed_kmh) + 0.5
    turnaround_hrs = if (aircraft_obj$name == "Embraer E190") 0.58 else if (aircraft_obj$name == "Boeing 787-9") 2.0 else 0.75
    flights_per_day = max(1, floor(14 / (flight_time_hrs + turnaround_hrs)))
    lease_per_flight = aircraft_obj$daily_lease_cost / flights_per_day
    fuel_cost = selected$distance_km * aircraft_obj$fuel_burn_per_km * aircraft_obj$fuel_cost_per_kg
    fixed_total = aircraft_obj$fixed_cost + (aircraft_obj$hourly_cost * flight_time_hrs) + aircraft_obj$ground_handling_cost + lease_per_flight
    load_factors = seq(0.50, 1.00, by = 0.01)
    profits = c()
    for (lf in load_factors) {
      passengers = round(aircraft_obj$seats * lf)
      revenue = passengers * selected$avg_ticket_price
      if (aircraft_obj$widebody_landing_fee > 0) {
        landing_fees = aircraft_obj$widebody_landing_fee
      } else {
        landing_fees = passengers * aircraft_obj$landing_fee_per_pax
      }
      profit = revenue - fuel_cost - fixed_total - landing_fees
      profits = c(profits, profit)
    }
    curve_df = data.frame(load_factor = load_factors * 100, profit = profits)
    real_lf = selected$load_factor_base * 100
    real_profit = curve_df$profit[which.min(abs(curve_df$load_factor - real_lf))]
    p5 = ggplot(curve_df, aes(x = load_factor, y = profit,
      text = paste("Load Factor:", round(load_factor), "%",
        "<br>Profit: $", format(round(profit), big.mark = ",")))) +
      geom_line(color = "#3c8dbc", linewidth = 1) +
      geom_vline(xintercept = real_lf, linetype = "dotted", color = "orange", linewidth = 0.8) +
      geom_point(aes(x = real_lf, y = real_profit), color = "orange", size = 3, inherit.aes = FALSE) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.7) +
      scale_y_continuous(labels = comma) +
      labs(x = "Load Factor (%)", y = "Profit (USD)", caption = "Orange dot = real average load factor for this route") +
      theme_minimal()
    ggplotly(p5, tooltip = "text") %>% layout(hoverlabel = list(bgcolor = "white"))
  })


  output$route_comparison <- renderPlotly({
    current_routes = routes_rv()
    current_fleet = fleet_rv()
    aircraft_obj = NULL
    for (i in 1:length(current_fleet)) {
      if (current_fleet[[i]]$name == input$selected_aircraft) {
        aircraft_obj = current_fleet[[i]]
      }
    }
    if (is.null(aircraft_obj)) return(NULL)
    route_profits = c()
    route_names = c()
    for (i in 1:nrow(current_routes)) {
      result = calculate_profit(aircraft_obj, current_routes[i, ], n_simulations = 500)
      if (!is.null(result)) {
        route_profits = c(route_profits, result$daily_profit)
        route_names = c(route_names, current_routes$route_name[i])
      }
    }
    route_df = data.frame(route = route_names, profit = route_profits)
    p6 = ggplot(route_df, aes(x = reorder(route, profit), y = profit, fill = profit > 0,
      text = paste("Route:", route, "<br>Daily Profit: $", format(round(profit), big.mark = ",")))) +
      geom_bar(stat = "identity", width = 0.5) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.7) +
      scale_fill_manual(values = c("TRUE" = "#3c8dbc", "FALSE" = "#e74c3c"), guide = "none") +
      scale_y_continuous(labels = comma) +
      coord_flip() +
      labs(x = "", y = "Daily Profit (USD)") +
      theme_minimal() +
      theme(axis.text = element_text(size = 9))
    ggplotly(p6, tooltip = "text") %>% layout(hoverlabel = list(bgcolor = "white"))
  })


  output$flight_map <- renderLeaflet({
    current_routes = routes_rv()
    arr_airports = data.frame(
      code = current_routes$arr_airport,
      lat = current_routes$arr_lat,
      lon = current_routes$arr_lon,
      route = current_routes$route_name,
      stringsAsFactors = FALSE
    )
    lhr = data.frame(code = "LHR", lat = 51.4775, lon = -0.4614)
    map = leaflet() %>% addTiles() %>% setView(lng = 20, lat = 30, zoom = 2)
    if (input$map_route == "Show All Routes") {
      for (i in 1:nrow(current_routes)) {
        gc = great_circle_points(current_routes$dep_lon[i], current_routes$dep_lat[i], current_routes$arr_lon[i], current_routes$arr_lat[i])
        map = map %>% addPolylines(lng = gc$lon, lat = gc$lat, color = "#3c8dbc", weight = 2, opacity = 0.7, label = current_routes$route_name[i])
      }
    } else {
      selected = current_routes[current_routes$route_name == input$map_route, ]
      gc = great_circle_points(selected$dep_lon, selected$dep_lat, selected$arr_lon, selected$arr_lat)
      map = map %>% addPolylines(lng = gc$lon, lat = gc$lat, color = "#e74c3c", weight = 3, opacity = 0.9, label = selected$route_name)
    }
    map = map %>%
      addCircleMarkers(data = arr_airports, lat = ~lat, lng = ~lon,
        radius = 6, color = "white", fillColor = "#3c8dbc", fillOpacity = 0.9, weight = 2,
        popup = ~paste("<b>Airport:</b>", code, "<br><b>Route:</b>", route)) %>%
      addCircleMarkers(data = lhr, lat = ~lat, lng = ~lon,
        radius = 8, color = "white", fillColor = "#e74c3c", fillOpacity = 0.9, weight = 2,
        popup = "<b>London Heathrow (LHR)</b><br>Departure airport for all routes")
    return(map)
  })


  observeEvent(input$map_route, {
    current_routes = routes_rv()
    arr_airports = data.frame(
      code = current_routes$arr_airport,
      lat = current_routes$arr_lat,
      lon = current_routes$arr_lon,
      route = current_routes$route_name,
      stringsAsFactors = FALSE
    )
    lhr = data.frame(code = "LHR", lat = 51.4775, lon = -0.4614)
    leafletProxy("flight_map") %>% clearShapes() %>% clearMarkers()
    if (input$map_route == "Show All Routes") {
      for (i in 1:nrow(current_routes)) {
        gc = great_circle_points(current_routes$dep_lon[i], current_routes$dep_lat[i], current_routes$arr_lon[i], current_routes$arr_lat[i])
        leafletProxy("flight_map") %>% addPolylines(lng = gc$lon, lat = gc$lat, color = "#3c8dbc", weight = 2, opacity = 0.7, label = current_routes$route_name[i])
      }
    } else {
      selected = current_routes[current_routes$route_name == input$map_route, ]
      gc = great_circle_points(selected$dep_lon, selected$dep_lat, selected$arr_lon, selected$arr_lat)
      leafletProxy("flight_map") %>% addPolylines(lng = gc$lon, lat = gc$lat, color = "#e74c3c", weight = 3, opacity = 0.9, label = selected$route_name)
    }
    leafletProxy("flight_map") %>%
      addCircleMarkers(data = arr_airports, lat = ~lat, lng = ~lon,
        radius = 6, color = "white", fillColor = "#3c8dbc", fillOpacity = 0.9, weight = 2,
        popup = ~paste("<b>Airport:</b>", code, "<br><b>Route:</b>", route)) %>%
      addCircleMarkers(data = lhr, lat = ~lat, lng = ~lon,
        radius = 8, color = "white", fillColor = "#e74c3c", fillOpacity = 0.9, weight = 2,
        popup = "<b>London Heathrow (LHR)</b><br>Departure airport for all routes")
  })


  output$checker_result <- renderUI({
    input$check_btn
    isolate({
      if (input$check_btn == 0) return(NULL)
      current_fleet = fleet_rv()
      current_routes = routes_rv()
      aircraft_obj = NULL
      for (i in 1:length(current_fleet)) {
        if (current_fleet[[i]]$name == input$check_aircraft) {
          aircraft_obj = current_fleet[[i]]
        }
      }
      selected_route = current_routes[current_routes$route_name == input$check_route, ]
      if (selected_route$distance_km > aircraft_obj$max_range_km) {
        div(style = "background-color: #e74c3c; color: white; padding: 15px; border-radius: 6px;",
          icon("times-circle"), strong(" OUT OF RANGE"), br(), br(),
          p(style = "margin: 0;", paste("Aircraft:", aircraft_obj$name)),
          p(style = "margin: 0;", paste("Route distance:", selected_route$distance_km, "km")),
          p(style = "margin: 0;", paste("Aircraft max range:", aircraft_obj$max_range_km, "km")),
          p(style = "margin: 0;", paste("Exceeds range by:", selected_route$distance_km - aircraft_obj$max_range_km, "km"))
        )
      } else {
        result = calculate_profit(aircraft_obj, selected_route, n_simulations = 1000)
        profit_color = if (result$daily_profit > 0) "#2ecc71" else "#e74c3c"
        profit_label = if (result$daily_profit > 0) " WITHIN RANGE - PROFITABLE" else " WITHIN RANGE - NOT PROFITABLE"
        div(style = paste0("background-color: ", profit_color, "; color: white; padding: 15px; border-radius: 6px;"),
          icon(if (result$daily_profit > 0) "check-circle" else "exclamation-circle"),
          strong(profit_label), br(), br(),
          p(style = "margin: 0;", paste("Aircraft:", aircraft_obj$name)),
          p(style = "margin: 0;", paste("Route:", input$check_route)),
          p(style = "margin: 0;", paste("Distance:", selected_route$distance_km, "km")),
          p(style = "margin: 0;", paste("Range remaining:", aircraft_obj$max_range_km - selected_route$distance_km, "km")),
          p(style = "margin: 0;", paste("Flight time:", result$flight_time_hrs, "hrs")),
          p(style = "margin: 0;", paste("Flights per day:", result$flights_per_day)),
          p(style = "margin: 0;", paste("Avg load factor:", result$avg_load_factor, "%")),
          p(style = "margin: 0;", paste("Lease cost per flight: $", format(result$lease_per_flight, big.mark = ","))),
          hr(style = "border-color: white;"),
          p(style = "margin: 0;", paste("Profit per flight: $", format(result$avg_profit, big.mark = ","))),
          p(style = "margin: 0;", paste("Daily profit: $", format(result$daily_profit, big.mark = ","))),
          p(style = "margin: 0;", paste("Best case: $", format(result$best_profit, big.mark = ","))),
          p(style = "margin: 0;", paste("Worst case: $", format(result$worst_profit, big.mark = ",")))
        )
      }
    })
  })


  # custom simulation
  custom_sim <- eventReactive(input$run_custom, {
    custom_aircraft = new_aircraft(
      name = input$custom_name,
      seats = input$custom_seats,
      max_range_km = input$custom_range,
      cruise_speed_kmh = input$custom_speed,
      fuel_burn_per_km = input$custom_fuel_burn,
      fuel_cost_per_kg = input$custom_fuel_cost,
      daily_lease_cost = input$custom_lease,
      hourly_cost = input$custom_hourly,
      ground_handling_cost = input$custom_handling,
      fixed_cost = input$custom_fixed,
      landing_fee_per_pax = input$custom_landing_pax,
      widebody_landing_fee = input$custom_widebody_fee
    )
    custom_route = data.frame(
      route_name = "Custom Route",
      distance_km = input$custom_distance,
      avg_ticket_price = input$custom_ticket,
      load_factor_base = input$custom_load / 100,
      dep_lat = 51.4775, dep_lon = -0.4614,
      arr_lat = 0, arr_lon = 0,
      arr_airport = "Custom",
      stringsAsFactors = FALSE
    )
    result = calculate_profit(custom_aircraft, custom_route, n_simulations = input$custom_sims)
    return(result)
  })

  output$custom_result_boxes <- renderUI({
    result = custom_sim()
    if (is.null(result)) return(NULL)
    profit_color = if (result$daily_profit > 0) "green" else "red"
    tagList(
      valueBox(value = paste0("$", format(result$avg_profit, big.mark = ",")), subtitle = "Avg Profit per Flight", icon = icon("plane"), color = "blue", width = 3),
      valueBox(value = paste0("$", format(result$daily_profit, big.mark = ",")), subtitle = "Daily Profit", icon = icon("dollar-sign"), color = profit_color, width = 3),
      valueBox(value = result$flights_per_day, subtitle = "Flights per Day", icon = icon("clock"), color = "light-blue", width = 3),
      valueBox(value = paste0(result$avg_load_factor, "%"), subtitle = "Avg Load Factor", icon = icon("users"), color = "yellow", width = 3)
    )
  })

  output$custom_result_detail <- renderUI({
    result = custom_sim()
    if (is.null(result)) return(NULL)
    profit_color = if (result$daily_profit > 0) "#2ecc71" else "#e74c3c"
    div(style = paste0("background-color: ", profit_color, "; color: white; padding: 15px; border-radius: 6px;"),
      strong(if (result$daily_profit > 0) "PROFITABLE ROUTE" else "UNPROFITABLE ROUTE"), br(), br(),
      p(style = "margin: 0;", paste("Flight time:", result$flight_time_hrs, "hrs")),
      p(style = "margin: 0;", paste("Flights per day:", result$flights_per_day)),
      p(style = "margin: 0;", paste("Lease per flight: $", format(result$lease_per_flight, big.mark = ","))),
      p(style = "margin: 0;", paste("Avg revenue: $", format(result$avg_revenue, big.mark = ","))),
      p(style = "margin: 0;", paste("Fuel cost: $", format(result$fuel_cost, big.mark = ","))),
      hr(style = "border-color: white;"),
      p(style = "margin: 0;", paste("Profit per flight: $", format(result$avg_profit, big.mark = ","))),
      p(style = "margin: 0;", paste("Daily profit: $", format(result$daily_profit, big.mark = ","))),
      p(style = "margin: 0;", paste("Best case: $", format(result$best_profit, big.mark = ","))),
      p(style = "margin: 0;", paste("Worst case: $", format(result$worst_profit, big.mark = ",")))
    )
  })

  output$custom_load_curve <- renderPlotly({
    result = custom_sim()
    if (is.null(result)) return(NULL)
    flight_time_hrs = (input$custom_distance / input$custom_speed) + 0.5
    turnaround_hrs = 0.75
    flights_per_day = max(1, floor(14 / (flight_time_hrs + turnaround_hrs)))
    lease_per_flight = input$custom_lease / flights_per_day
    fuel_cost = input$custom_distance * input$custom_fuel_burn * input$custom_fuel_cost
    fixed_total = input$custom_fixed + (input$custom_hourly * flight_time_hrs) + input$custom_handling + lease_per_flight
    load_factors = seq(0.50, 1.00, by = 0.01)
    profits = c()
    for (lf in load_factors) {
      passengers = round(input$custom_seats * lf)
      revenue = passengers * input$custom_ticket
      if (input$custom_widebody_fee > 0) {
        landing_fees = input$custom_widebody_fee
      } else {
        landing_fees = passengers * input$custom_landing_pax
      }
      profit = revenue - fuel_cost - fixed_total - landing_fees
      profits = c(profits, profit)
    }
    curve_df = data.frame(load_factor = load_factors * 100, profit = profits)
    real_lf = input$custom_load
    real_profit = curve_df$profit[which.min(abs(curve_df$load_factor - real_lf))]
    p = ggplot(curve_df, aes(x = load_factor, y = profit,
      text = paste("Load Factor:", round(load_factor), "%", "<br>Profit: $", format(round(profit), big.mark = ",")))) +
      geom_line(color = "#27ae60", linewidth = 1) +
      geom_vline(xintercept = real_lf, linetype = "dotted", color = "orange", linewidth = 0.8) +
      geom_point(aes(x = real_lf, y = real_profit), color = "orange", size = 3, inherit.aes = FALSE) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.7) +
      scale_y_continuous(labels = comma) +
      labs(x = "Load Factor (%)", y = "Profit (USD)", caption = "Orange dot = your input load factor") +
      theme_minimal()
    ggplotly(p, tooltip = "text") %>% layout(hoverlabel = list(bgcolor = "white"))
  })


  # fleet manager table
  output$fleet_table <- renderDT({
    current_fleet = fleet_rv()
    fleet_df = data.frame(
      "#" = 1:length(current_fleet),
      "Aircraft Name" = sapply(current_fleet, function(x) x$name),
      "Seats" = sapply(current_fleet, function(x) x$seats),
      "Max Range (km)" = sapply(current_fleet, function(x) x$max_range_km),
      "Cruise Speed (km/h)" = sapply(current_fleet, function(x) x$cruise_speed_kmh),
      "Daily Lease ($)" = sapply(current_fleet, function(x) format(x$daily_lease_cost, big.mark = ",")),
      "Fuel Burn (kg/km)" = sapply(current_fleet, function(x) x$fuel_burn_per_km),
      check.names = FALSE
    )
    datatable(fleet_df, selection = "single", rownames = FALSE,
      options = list(pageLength = 10, dom = "t"),
      caption = "Click a row to edit that aircraft")
  })

  selected_aircraft_row <- reactiveVal(NULL)

  observeEvent(input$fleet_table_rows_selected, {
    selected_aircraft_row(input$fleet_table_rows_selected)
  })

  output$fleet_edit_panel <- renderUI({
    row = selected_aircraft_row()
    if (is.null(row)) return(NULL)
    current_fleet = fleet_rv()
    ac = current_fleet[[row]]
    div(style = "background-color: #f4f4f4; padding: 15px; border-radius: 6px; margin-top: 10px;",
      h4(paste("Editing:", ac$name)),
      fluidRow(
        column(3, numericInput("edit_ac_seats", "Seats", value = ac$seats, min = 50, max = 600)),
        column(3, numericInput("edit_ac_range", "Max Range (km)", value = ac$max_range_km, min = 500, max = 16000)),
        column(3, numericInput("edit_ac_speed", "Cruise Speed (km/h)", value = ac$cruise_speed_kmh, min = 600, max = 1100)),
        column(3, numericInput("edit_ac_lease", "Daily Lease Cost ($)", value = ac$daily_lease_cost, min = 1000, max = 100000, step = 500))
      ),
      fluidRow(
        column(3, numericInput("edit_ac_fuel_burn", "Fuel Burn per km (kg)", value = ac$fuel_burn_per_km, min = 1, max = 12, step = 0.1)),
        column(3, numericInput("edit_ac_fuel_cost", "Fuel Cost per kg ($)", value = ac$fuel_cost_per_kg, min = 0.5, max = 2, step = 0.01)),
        column(3, numericInput("edit_ac_hourly", "Hourly Cost ($)", value = ac$hourly_cost, min = 500, max = 10000, step = 100)),
        column(3, numericInput("edit_ac_handling", "Ground Handling ($)", value = ac$ground_handling_cost, min = 100, max = 5000, step = 50))
      ),
      fluidRow(
        column(3, numericInput("edit_ac_fixed", "Fixed Cost per Flight ($)", value = ac$fixed_cost, min = 100, max = 20000, step = 100)),
        column(3, numericInput("edit_ac_landing_pax", "Landing Fee per Pax ($)", value = ac$landing_fee_per_pax, min = 0, max = 100, step = 1)),
        column(3, numericInput("edit_ac_widebody", "Widebody Landing Fee ($)", value = ac$widebody_landing_fee, min = 0, max = 20000, step = 100)),
        column(3, br(), br(),
          actionButton("delete_aircraft_btn", "Delete Aircraft",
            style = "background-color: #e74c3c; color: white; border-radius: 4px; padding: 8px 16px;"))
      ),
      br(),
      actionButton("save_aircraft_btn", "Save Changes",
        style = "background-color: #3c8dbc; color: white; border-radius: 4px; padding: 8px 20px;"),
      br(), br(),
      uiOutput("fleet_save_msg")
    )
  })

  observeEvent(input$save_aircraft_btn, {
    row = selected_aircraft_row()
    if (is.null(row)) return()
    current_fleet = fleet_rv()
    ac = current_fleet[[row]]
    updated_ac = new_aircraft(
      name = ac$name,
      seats = input$edit_ac_seats,
      max_range_km = input$edit_ac_range,
      cruise_speed_kmh = input$edit_ac_speed,
      fuel_burn_per_km = input$edit_ac_fuel_burn,
      fuel_cost_per_kg = input$edit_ac_fuel_cost,
      daily_lease_cost = input$edit_ac_lease,
      hourly_cost = input$edit_ac_hourly,
      ground_handling_cost = input$edit_ac_handling,
      fixed_cost = input$edit_ac_fixed,
      landing_fee_per_pax = input$edit_ac_landing_pax,
      widebody_landing_fee = input$edit_ac_widebody
    )
    current_fleet[[row]] = updated_ac
    fleet_rv(current_fleet)
    output$fleet_save_msg <- renderUI({
      p(style = "color: #27ae60; font-weight: bold;", paste(ac$name, "updated successfully"))
    })
  })

  observeEvent(input$delete_aircraft_btn, {
    row = selected_aircraft_row()
    if (is.null(row)) return()
    current_fleet = fleet_rv()
    if (length(current_fleet) <= 1) {
      output$fleet_save_msg <- renderUI({
        p(style = "color: #e74c3c;", "Cannot delete - fleet must have at least one aircraft")
      })
      return()
    }
    ac_name = current_fleet[[row]]$name
    current_fleet[[row]] = NULL
    fleet_rv(current_fleet)
    selected_aircraft_row(NULL)
    output$fleet_save_msg <- renderUI({
      p(style = "color: #e74c3c; font-weight: bold;", paste(ac_name, "removed from fleet"))
    })
  })

  observeEvent(input$add_aircraft_btn, {
    if (nchar(trimws(input$new_ac_name)) == 0) {
      output$fleet_add_msg <- renderUI({
        p(style = "color: #e74c3c;", "Please enter an aircraft name")
      })
      return()
    }
    new_ac = tryCatch(
      new_aircraft(
        name = input$new_ac_name,
        seats = input$new_ac_seats,
        max_range_km = input$new_ac_range,
        cruise_speed_kmh = input$new_ac_speed,
        fuel_burn_per_km = input$new_ac_fuel_burn,
        fuel_cost_per_kg = input$new_ac_fuel_cost,
        daily_lease_cost = input$new_ac_lease,
        hourly_cost = input$new_ac_hourly,
        ground_handling_cost = input$new_ac_handling,
        fixed_cost = input$new_ac_fixed,
        landing_fee_per_pax = input$new_ac_landing_pax,
        widebody_landing_fee = input$new_ac_widebody
      ),
      error = function(e) NULL
    )
    if (is.null(new_ac)) {
      output$fleet_add_msg <- renderUI({
        p(style = "color: #e74c3c;", "Error creating aircraft - check all values are valid")
      })
      return()
    }
    current_fleet = fleet_rv()
    current_fleet[[length(current_fleet) + 1]] = new_ac
    names(current_fleet)[length(current_fleet)] = input$new_ac_name
    fleet_rv(current_fleet)
    output$fleet_add_msg <- renderUI({
      p(style = "color: #27ae60; font-weight: bold;", paste(input$new_ac_name, "added to fleet successfully"))
    })
  })


  # route manager table
  output$route_table <- renderDT({
    current_routes = routes_rv()
    route_display = data.frame(
      "#" = 1:nrow(current_routes),
      "Route Name" = current_routes$route_name,
      "Arrival Airport" = current_routes$arr_airport,
      "Distance (km)" = current_routes$distance_km,
      "Ticket Price ($)" = current_routes$avg_ticket_price,
      "Load Factor" = paste0(round(current_routes$load_factor_base * 100), "%"),
      check.names = FALSE
    )
    datatable(route_display, selection = "single", rownames = FALSE,
      options = list(pageLength = 10, dom = "t"),
      caption = "Click a row to edit that route")
  })

  selected_route_row <- reactiveVal(NULL)

  observeEvent(input$route_table_rows_selected, {
    selected_route_row(input$route_table_rows_selected)
  })

  output$route_edit_panel <- renderUI({
    row = selected_route_row()
    if (is.null(row)) return(NULL)
    current_routes = routes_rv()
    rt = current_routes[row, ]
    div(style = "background-color: #f4f4f4; padding: 15px; border-radius: 6px; margin-top: 10px;",
      h4(paste("Editing:", rt$route_name)),
      fluidRow(
        column(3, numericInput("edit_rt_ticket", "Avg Ticket Price ($)", value = rt$avg_ticket_price, min = 20, max = 5000, step = 10)),
        column(3, numericInput("edit_rt_load", "Avg Load Factor (%)", value = round(rt$load_factor_base * 100), min = 50, max = 99, step = 1)),
        column(3, numericInput("edit_rt_arr_lat", "Arrival Latitude", value = rt$arr_lat, min = -90, max = 90, step = 0.0001)),
        column(3, numericInput("edit_rt_arr_lon", "Arrival Longitude", value = rt$arr_lon, min = -180, max = 180, step = 0.0001))
      ),
      p(style = "color: grey; font-size: 11px;", "Changing coordinates will recalculate the distance automatically when you save."),
      br(),
      fluidRow(
        column(3, actionButton("save_route_btn", "Save Changes",
          style = "background-color: #3c8dbc; color: white; border-radius: 4px; padding: 8px 20px;")),
        column(3, actionButton("delete_route_btn", "Delete Route",
          style = "background-color: #e74c3c; color: white; border-radius: 4px; padding: 8px 16px;"))
      ),
      br(),
      uiOutput("route_save_msg")
    )
  })

  observeEvent(input$save_route_btn, {
    row = selected_route_row()
    if (is.null(row)) return()
    current_routes = routes_rv()
    current_routes$avg_ticket_price[row] = input$edit_rt_ticket
    current_routes$load_factor_base[row] = input$edit_rt_load / 100
    current_routes$arr_lat[row] = input$edit_rt_arr_lat
    current_routes$arr_lon[row] = input$edit_rt_arr_lon
    current_routes$distance_km[row] = round(haversine_distance(
      current_routes$dep_lat[row], current_routes$dep_lon[row],
      input$edit_rt_arr_lat, input$edit_rt_arr_lon))
    routes_rv(current_routes)
    output$route_save_msg <- renderUI({
      p(style = "color: #27ae60; font-weight: bold;", paste(current_routes$route_name[row], "updated successfully"))
    })
  })

  observeEvent(input$delete_route_btn, {
    row = selected_route_row()
    if (is.null(row)) return()
    current_routes = routes_rv()
    if (nrow(current_routes) <= 1) {
      output$route_save_msg <- renderUI({
        p(style = "color: #e74c3c;", "Cannot delete - must have at least one route")
      })
      return()
    }
    rt_name = current_routes$route_name[row]
    current_routes = current_routes[-row, ]
    routes_rv(current_routes)
    selected_route_row(NULL)
    output$route_save_msg <- renderUI({
      p(style = "color: #e74c3c; font-weight: bold;", paste(rt_name, "removed"))
    })
  })

  observeEvent(input$add_route_btn, {
    if (nchar(trimws(input$new_rt_name)) == 0) {
      output$route_add_msg <- renderUI({
        p(style = "color: #e74c3c;", "Please enter a route name")
      })
      return()
    }
    dist = round(haversine_distance(51.4775, -0.4614, input$new_rt_arr_lat, input$new_rt_arr_lon))
    new_row = data.frame(
      route_name = input$new_rt_name,
      dep_airport = "LHR",
      arr_airport = toupper(input$new_rt_arr_airport),
      dep_lat = 51.4775,
      dep_lon = -0.4614,
      arr_lat = input$new_rt_arr_lat,
      arr_lon = input$new_rt_arr_lon,
      avg_ticket_price = input$new_rt_ticket,
      load_factor_base = input$new_rt_load / 100,
      distance_km = dist,
      stringsAsFactors = FALSE
    )
    current_routes = routes_rv()
    current_routes = rbind(current_routes, new_row)
    routes_rv(current_routes)
    output$route_add_msg <- renderUI({
      p(style = "color: #27ae60; font-weight: bold;",
        paste(input$new_rt_name, "added successfully - distance calculated as", dist, "km"))
    })
  })


  # results table
  output$results_table <- renderDT({
    df = sim_results()
    display_df = data.frame(
      Aircraft = df$aircraft,
      "Per Flight" = paste0("$", format(df$avg_profit, big.mark = ",")),
      "Daily Profit" = paste0("$", format(df$daily_profit, big.mark = ",")),
      "Flights/Day" = df$flights_per_day,
      "Best Case" = paste0("$", format(df$best_profit, big.mark = ",")),
      "Worst Case" = paste0("$", format(df$worst_profit, big.mark = ",")),
      "Revenue" = paste0("$", format(df$avg_revenue, big.mark = ",")),
      "Fuel" = paste0("$", format(df$fuel_cost, big.mark = ",")),
      "Crew Cost" = paste0("$", format(df$hourly_cost, big.mark = ",")),
      "Lease/Flight" = paste0("$", format(df$lease_per_flight, big.mark = ",")),
      "Load Factor" = paste0(df$avg_load_factor, "%"),
      "Flight Time" = paste0(df$flight_time, " hrs"),
      check.names = FALSE
    )
    datatable(display_df, options = list(pageLength = 10, dom = "t", ordering = TRUE), rownames = FALSE) %>%
      formatStyle("Daily Profit", backgroundColor = styleInterval(0, c("#e74c3c", "#2ecc71")), color = "white", fontWeight = "bold") %>%
      formatStyle("Per Flight", backgroundColor = styleInterval(0, c("#e74c3c", "#2ecc71")), color = "white", fontWeight = "bold")
  })


  # download simulation results as CSV
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("AeroFleet_Results_", gsub(" ", "_", input$selected_route), "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      df = sim_results()
      write.csv(df, file, row.names = FALSE)
    }
  )

  # download current fleet as CSV
  output$download_fleet_csv <- downloadHandler(
    filename = function() { paste0("AeroFleet_Fleet_", Sys.Date(), ".csv") },
    content = function(file) {
      current_fleet = fleet_rv()
      fleet_df = data.frame(
        name = sapply(current_fleet, function(x) x$name),
        seats = sapply(current_fleet, function(x) x$seats),
        max_range_km = sapply(current_fleet, function(x) x$max_range_km),
        cruise_speed_kmh = sapply(current_fleet, function(x) x$cruise_speed_kmh),
        fuel_burn_per_km = sapply(current_fleet, function(x) x$fuel_burn_per_km),
        fuel_cost_per_kg = sapply(current_fleet, function(x) x$fuel_cost_per_kg),
        daily_lease_cost = sapply(current_fleet, function(x) x$daily_lease_cost),
        hourly_cost = sapply(current_fleet, function(x) x$hourly_cost),
        ground_handling_cost = sapply(current_fleet, function(x) x$ground_handling_cost),
        fixed_cost = sapply(current_fleet, function(x) x$fixed_cost),
        landing_fee_per_pax = sapply(current_fleet, function(x) x$landing_fee_per_pax),
        widebody_landing_fee = sapply(current_fleet, function(x) x$widebody_landing_fee)
      )
      write.csv(fleet_df, file, row.names = FALSE)
    }
  )

  # download current routes as CSV
  output$download_routes_csv <- downloadHandler(
    filename = function() { paste0("AeroFleet_Routes_", Sys.Date(), ".csv") },
    content = function(file) {
      write.csv(routes_rv(), file, row.names = FALSE)
    }
  )

  # generate and download PDF report
  output$download_pdf <- downloadHandler(
    filename = function() {
      paste0("AeroFleet_Report_", gsub(" ", "_", input$pdf_route), "_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      current_routes = routes_rv()
      current_fleet = fleet_rv()
      selected = current_routes[current_routes$route_name == input$pdf_route, ]
      results_list = list()
      for (i in 1:length(current_fleet)) {
        ac = current_fleet[[i]]
        result = calculate_profit(ac, selected)
        if (!is.null(result)) {
          results_list[[length(results_list) + 1]] = result
        }
      }
      if (length(results_list) == 0) {
        writeLines("No aircraft can reach this route.", file)
        return()
      }
      df = data.frame(
        Aircraft = sapply(results_list, function(x) x$aircraft_name),
        Per_Flight = sapply(results_list, function(x) x$avg_profit),
        Daily_Profit = sapply(results_list, function(x) x$daily_profit),
        Flights_Per_Day = sapply(results_list, function(x) x$flights_per_day),
        Best_Case = sapply(results_list, function(x) x$best_profit),
        Worst_Case = sapply(results_list, function(x) x$worst_profit),
        Avg_Revenue = sapply(results_list, function(x) x$avg_revenue),
        Fuel_Cost = sapply(results_list, function(x) x$fuel_cost),
        Lease_Per_Flight = sapply(results_list, function(x) x$lease_per_flight),
        Load_Factor = sapply(results_list, function(x) x$avg_load_factor),
        Flight_Time = sapply(results_list, function(x) x$flight_time_hrs)
      )

      # save charts as temp image files to embed in PDF
      tmp_profit = tempfile(fileext = ".png")
      tmp_daily = tempfile(fileext = ".png")

      p_profit = ggplot(df, aes(x = reorder(Aircraft, Per_Flight), y = Per_Flight, fill = Per_Flight > 0)) +
        geom_bar(stat = "identity", width = 0.5) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        scale_fill_manual(values = c("TRUE" = "#3c8dbc", "FALSE" = "#e74c3c"), guide = "none") +
        scale_y_continuous(labels = comma) +
        coord_flip() + labs(title = "Profit per Flight", x = "", y = "USD") + theme_minimal()
      ggsave(tmp_profit, plot = p_profit, width = 7, height = 3, dpi = 150)

      p_daily = ggplot(df, aes(x = reorder(Aircraft, Daily_Profit), y = Daily_Profit, fill = Daily_Profit > 0)) +
        geom_bar(stat = "identity", width = 0.5) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        scale_fill_manual(values = c("TRUE" = "#2ecc71", "FALSE" = "#e74c3c"), guide = "none") +
        scale_y_continuous(labels = comma) +
        coord_flip() + labs(title = "Daily Profit", x = "", y = "USD") + theme_minimal()
      ggsave(tmp_daily, plot = p_daily, width = 7, height = 3, dpi = 150)

      # build PDF
      pdf(file, width = 8.5, height = 11)
      par(mar = c(0, 0, 0, 0))
      plot.new()

      text(0.5, 0.97, "AeroFleet Simulation Report", cex = 1.8, font = 2, adj = 0.5)
      text(0.5, 0.92, paste("Route:", input$pdf_route), cex = 1.2, adj = 0.5)
      text(0.5, 0.89, paste("Distance:", selected$distance_km, "km  |  Avg Ticket: $", selected$avg_ticket_price,
        "  |  Load Factor:", round(selected$load_factor_base * 100), "%"), cex = 0.9, adj = 0.5, col = "grey40")
      text(0.5, 0.86, paste("Generated:", format(Sys.Date(), "%d %B %Y")), cex = 0.8, adj = 0.5, col = "grey60")

      text(0.05, 0.81, "Simulation Results Summary", cex = 1.1, font = 2, adj = 0)
      abline(h = 0.79, col = "grey70")

      y_pos = 0.76
      text(0.05, y_pos, "Aircraft", cex = 0.75, font = 2, adj = 0)
      text(0.35, y_pos, "Per Flight", cex = 0.75, font = 2, adj = 0)
      text(0.50, y_pos, "Daily Profit", cex = 0.75, font = 2, adj = 0)
      text(0.65, y_pos, "Flights/Day", cex = 0.75, font = 2, adj = 0)
      text(0.78, y_pos, "Best Case", cex = 0.75, font = 2, adj = 0)
      text(0.90, y_pos, "Worst Case", cex = 0.75, font = 2, adj = 0)

      for (i in 1:nrow(df)) {
        y_pos = y_pos - 0.04
        row_color = if (df$Daily_Profit[i] > 0) "#1a7a3c" else "#c0392b"
        text(0.05, y_pos, df$Aircraft[i], cex = 0.72, adj = 0)
        text(0.35, y_pos, paste0("$", format(df$Per_Flight[i], big.mark = ",")), cex = 0.72, adj = 0, col = row_color)
        text(0.50, y_pos, paste0("$", format(df$Daily_Profit[i], big.mark = ",")), cex = 0.72, adj = 0, col = row_color, font = 2)
        text(0.65, y_pos, df$Flights_Per_Day[i], cex = 0.72, adj = 0)
        text(0.78, y_pos, paste0("$", format(df$Best_Case[i], big.mark = ",")), cex = 0.72, adj = 0)
        text(0.90, y_pos, paste0("$", format(df$Worst_Case[i], big.mark = ",")), cex = 0.72, adj = 0)
      }

      abline(h = y_pos - 0.02, col = "grey70")
      text(0.05, y_pos - 0.05, "Charts", cex = 1.1, font = 2, adj = 0)

      img1 = png::readPNG(tmp_profit)
      rasterImage(img1, 0.02, y_pos - 0.25, 0.50, y_pos - 0.07)
      img2 = png::readPNG(tmp_daily)
      rasterImage(img2, 0.52, y_pos - 0.25, 1.00, y_pos - 0.07)

      text(0.05, y_pos - 0.28, "Cost Detail per Flight", cex = 1.0, font = 2, adj = 0)
      y2 = y_pos - 0.32
      for (i in 1:nrow(df)) {
        text(0.05, y2, paste0(df$Aircraft[i],
          "  |  Fuel: $", format(df$Fuel_Cost[i], big.mark = ","),
          "  |  Lease: $", format(df$Lease_Per_Flight[i], big.mark = ","),
          "  |  Revenue: $", format(df$Avg_Revenue[i], big.mark = ","),
          "  |  Load: ", df$Load_Factor[i], "%",
          "  |  Flight time: ", df$Flight_Time[i], " hrs"),
          cex = 0.68, adj = 0)
        y2 = y2 - 0.035
      }

      dev.off()
      file.remove(tmp_profit, tmp_daily)
    }
  )

  output$pdf_preview <- renderUI({
    div(style = "background-color: #f9f9f9; padding: 15px; border-radius: 6px;",
      p(strong("The PDF report will include:")),
      tags$ul(
        tags$li("Route summary - distance, ticket price, load factor"),
        tags$li("Full results table - profit per flight, daily profit, flights per day, best and worst case"),
        tags$li("Profit per Flight chart"),
        tags$li("Daily Profit chart"),
        tags$li("Cost detail breakdown per aircraft")
      ),
      p(style = "color: grey; font-size: 11px;",
        "Make sure the png package is installed: install.packages('png')")
    )
  })

}


shinyApp(ui = ui, server = server)
