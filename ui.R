# library(shiny)
# library(bslib)
# library(shinydashboard)
# 
# ui <- bslib::page_navbar(
#   title = "Algorithmic Eats V1.1",
#   theme = bs_theme(bootswatch = "lux"),
#   id = "main_navigation",
# 
#   bslib::nav_panel(
#     "User Profile",
#     value = "tab_profile",
#     fluidRow(
#       bslib::card(
#         bslib::card_header("Step 1: Who are you?"),
#         bslib::card_body(
#           textInput("user_name", "Name", "Lifter"),
# 
#           selectInput(
#             "location", "Location",
#             # choices = c(
#             #   "Newfoundland and Labrador",
#             #   "Prince Edward Island",
#             #   "Nova Scotia",
#             #   "New Brunswick",
#             #   "Quebec",
#             #   "Ontario",
#             #   "Manitoba",
#             #   "Saskatchewan",
#             #   "Alberta",
#             #   "British Columbia",
#             #   "Whitehorse, Yukon",
#             #   "Yellowknife, Northwest Territories"
#             # ),
#             choices = c("Canada"),
#             selected = "Canada"
#           ),
#           numericInput("age", "Age (Years)", 21),
#           selectInput("sex", "Sex",
#                       choices = c("Male", "Female"),
#                       selected = "Male"),
#           selectInput("height_cm", "Height",
#                       choices = setNames(height$cm, height$concat),
#                       selected = 178),
#           numericInput("weight", "Current Weight (lbs)", 180),
#           selectInput("goal", "Goal", choices = c("Bulking", "Cutting", "Maintenance")),
#           sliderInput(
#             "lift_sessions",
#             "Weightlifting sessions per week",
#             min = 0,
#             max = 7,
#             value = 3,
#             step = 1
#           ),
#           selectizeInput(
#             "allergies", "Foods to Exclude",
#             choices = sort(unique(final_food_dataset$name)),
#             multiple = TRUE,
#             options = list(placeholder = "Type to search foods...")))))),
# 
#   bslib::nav_panel("Macro Optimizer", value = "tab_macros"),
#   bslib::nav_panel("Grocery Optimizer", value = "tab_grocery"),
#   bslib::nav_panel("Gym Finder", value = "tab_gym")
# )
library(shiny)
library(bslib)
library(shinydashboard)

shinyUI(dashboardPage(
  dashboardHeader(title = 'Algorithmic Eats'),
  dashboardSidebar(
    selectInput("height_cm", "Height", 
                choices = setNames(height$cm, height$concat), 
                selected = 178),
    numericInput("weight", "Current Weight (lbs)", 180),
    numericInput("age", "Age (Years)", 21),
    selectInput("sex", "Sex",
                choices = c("Male", "Female"),
                selected = "Male"),
    
    tags$hr(),
    
    radioButtons(
      "body_type", "Body Archetype",
      choiceValues = c("lean", "balanced", "muscular", "overweight"),
      choiceNames = list(
        tagList(icon("user"), " Underweight"),
        tagList(icon("dumbbell"), " Athletic"),
        tagList(icon("user-friends"), " Balanced"),
        tagList(icon("user-plus"), " Overweight")
      ),
      selected = "balanced"
    ),
    radioButtons(
      "cardio_frequency", "Cardio Frequency",
      choiceValues = c("cardio_none",
                       "cardio_low",
                       "cardio_mid",
                       "cardio_high"
      ),
      choiceNames = list(
        tagList(icon("chair"), " Rarely / none"),
        tagList(icon("walking"), " 1-2 sessions / week"),
        tagList(icon("running"), " 3-5 sessions / week"),
        tagList(icon("person-biking"), "6-7 sessions / week")
      ),
      selected = "cardio_low"
    ),
    radioButtons(
      "lift_freq",
      "Weight Training Frequency",
      choiceValues = c("lift_none",
                       "lift_low",
                       "lift_mid",
                       "lift_high"
      ),
      choiceNames = list(
        tagList(icon("ban"), " None"),
        tagList(icon("user"), " 1-2 sessions / week"),
        tagList(icon("dumbbell"), " 3-4 sessions / week"),
        tagList(icon("weight-hanging"), " 5+ sessions / week")        
      ),
      selected = "lift_mid"
    ),
    selectizeInput(
      "allergies", "Foods to Exclude",
      choices = sort(unique(final_food_dataset$food_group)),
      multiple = TRUE,
      options = list(placeholder = "Type to search foods..."))
  ),
  dashboardBody()
))
