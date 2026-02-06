library(shiny)
library(bslib)

ui <- bslib::page_navbar(
  title = "Algorithmic Eats V1.1",
  theme = bs_theme(bootswatch = "lux"),
  id = "main_navigation",

  bslib::nav_panel("User Profile",
           value = "tab_profile",
           fluidRow(
                    card(
                      card_header("Step 1: Who are you?"),
                      card_body(
                        textInput("user_name", "Name", "Lifter"),
                        selectInput("location", "Location",
                                    choices = unique(food_prices$Location),
                                    selected = "Alberta"),
                        numericInput("weight", "Current Weight (lbs)", 180),
                        selectInput("goal", "Goal", choices = c("Bulking", "Cutting", "Maintenance")),
                        selectizeInput("allergies", "Foods to Exclude",
                                       choices = unique(food_prices$Products),
                                       multiple = TRUE))))),
  bslib::nav_panel("Macro Optimizer",
           value = "tab_macros"),

  bslib::nav_panel("Grocery Optimizer",
           value = "tab_grocery"),

  bslib::nav_panel("Gym Finder",
           value = "tab_gym"))
