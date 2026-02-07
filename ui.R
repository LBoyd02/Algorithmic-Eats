library(shiny)
library(shinydashboard)

shiny::shinyUI(
  shinydashboard::dashboardPage(
    
    shinydashboard::dashboardHeader(title = 'Algorithmic Eats'),
    
    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        id = "tabs",
        
        shinydashboard::menuItem(
          "Macro Calculator",
          tabName = "Macro_Calc",
          icon = icon("calculator")
        ),
        shinydashboard::menuItem(
          "Grocery Optimizer",
          tabName = "Grocery_Opt",
          icon = icon("shopping-cart")
        ),
        shinydashboard::menuItem(
          "Dynamic Gym Map",
          tabName = "Gym_Map",
          icon = icon("map")
        )
      ),
      
      # --- Macro Calculator Side Panel ---
      shiny::conditionalPanel(
        condition = "input.tabs == 'Macro_Calc'",

        shiny::selectInput("height_cm", "Height",
                    choices = setNames(height$cm, height$concat),
                    selected = 178),
        shiny::numericInput("weight", "Current Weight (lbs)", 180),
        shiny::numericInput("age", "Age (Years)", 21),
        shiny::selectInput("sex", "Sex",
                    choices = c("Male", "Female"),
                    selected = "Male"),

        tags$hr(),

        shiny::radioButtons(
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
        shiny::radioButtons(
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
        shiny::radioButtons(
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
        shiny::selectizeInput(
          "allergies", "Foods to Exclude",
          choices = sort(unique(final_food_dataset$food_group)),
          multiple = TRUE,
          options = list(placeholder = "Type to search foods...")
        )
      ),
      
      
      # --- Gym Map Side Panel ---
      shiny::conditionalPanel(
        condition = "input.tabs == 'Gym_Map'",
          
        shiny::textInput("address", "Address",
                          value = "116 St & 85 Ave, Edmonton, AB"),
          
        shiny::selectInput("selected_gym", "Gym Selected",
                          choices = NULL,
                          selected = NULL),
          
        shiny::selectInput("map_mode", "Type of Travel",
                          choices = c("Walking" = "walking", "Driving" = "driving"),
                          selected = "driving")
        )
      ),
    
    # Tab Body -- Throw yalls charts in here n stuff
    shinydashboard::dashboardBody(
      shinydashboard::tabItems(
        
        shinydashboard::tabItem(tabName = "Macro_Calc", "Throw Macro Optimizer stuff here"),
        
        shinydashboard::tabItem(tabName = "Grocery_Opt", "Throw Grocery Optimizer stuff here"),
        
        shinydashboard::tabItem(tabName = "Gym_Map",
          shiny::fluidRow(
            shiny::column(
              width = 12,
              leaflet::leafletOutput("gym_map", height = "90vh")
            )
          )
        )
      )
    )
  )
)