library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)

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
        
        shiny::selectInput(
          "height_cm", "Height",
          choices  = setNames(height$cm, height$concat),
          selected = 178
        ),
        shiny::numericInput("weight_lb", "Current Weight (lbs)", 180),
        shiny::numericInput("age", "Age (Years)", 21),
        shiny::selectInput("sex", "Sex", choices = c("Male", "Female"), selected = "Male"),
        
        tags$hr(),
        
        shiny::radioButtons(
          "body_type", "Body Archetype",
          choiceValues = c("lean", "balanced", "muscular", "overweight"),
          choiceNames  = list(
            tagList(icon("user"), " Lean"),
            tagList(icon("user-friends"), " Balanced"),
            tagList(icon("dumbbell"), " Muscular"),
            tagList(icon("user-plus"), " Overweight")
          ),
          selected = "balanced"
        ),
        
        shiny::radioButtons(
          "cardio_frequency", "Cardio Frequency",
          choiceValues = c("cardio_none", "cardio_low", "cardio_mid", "cardio_high"),
          choiceNames  = list(
            tagList(icon("chair"), " Rarely / none"),
            tagList(icon("walking"), " 1-2 sessions / week"),
            tagList(icon("running"), " 3-5 sessions / week"),
            tagList(icon("person-biking"), " 6-7 sessions / week")
          ),
          selected = "cardio_low"
        ),
        
        shiny::radioButtons(
          "lift_frequency", "Weight Training Frequency",
          choiceValues = c("lift_none", "lift_low", "lift_mid", "lift_high"),
          choiceNames  = list(
            tagList(icon("ban"), " None"),
            tagList(icon("user"), " 1-2 sessions / week"),
            tagList(icon("dumbbell"), " 3-4 sessions / week"),
            tagList(icon("weight-hanging"), " 5+ sessions / week")
          ),
          selected = "lift_mid"
        )
      ),
      
      # --- Grocery Opt Calculator Side Panel ---
      shiny::conditionalPanel(
        condition = "input.tabs == 'Grocery_Opt'",
        shiny::numericInput("grocery_budget", "Monthly Budget ($)", value = 500, min = 100, step = 50),
        shiny::actionButton("generate_grocery", "Generate Monthly Plan", class = "btn-success")
      ),
      # --- Gym Map Side Panel ---
      shiny::conditionalPanel(
        condition = "input.tabs == 'Gym_Map'",
        
        shiny::textInput("address", "Address",
                         value = "11211 Saskatchewan Dr NW, Edmonton, AB"),
        
        shiny::selectInput("selected_gym", "Gym Selected",
                           choices = NULL,
                           selected = NULL),
        
        shiny::selectInput("map_mode", "Type of Travel",
                           choices = c("Walking" = "walking", "Driving" = "driving"),
                           selected = "driving")
      )
    ),
    
    # Tab Body
    shinydashboard::dashboardBody(
      shinydashboard::tabItems(
        
        shinydashboard::tabItem(tabName = "Macro_Calc",
                                fluidRow(
                                  box(title = 'Diet Optimizer', 
                                      width = 4,
                                      solidHeader = TRUE, 
                                      status = 'primary',
    #Goal Input
                                      sliderInput(
                                        "goal",
                                        "Goal (1-5)",
                                        min = 1, max = 5, value = 3, step = 1,
                                        ticks = FALSE
                                      ),
    #Dynamic goal label for style and aesthetics
    uiOutput("goal_label"),
    tags$hr(),
    
      
                                      
                                      
                                      
    #Calorie Slider --
                                      sliderInput('cal', 'Slide to see how the chart changes based on your calorie intake',
                                                  min = 1200, max = 5000, value = 2500,
                                                  step = 10, ticks = FALSE),
                                      valueBoxOutput("maint_box", width = 12),
                                      valueBoxOutput("balance_box", width = 12),
                                      valueBoxOutput("protein_box", width = 12),
                                      valueBoxOutput("year_box", width = 12)
                                  ),
    

    #Line Chart --
                             tabBox(
                                title = "Progression",
                                width = 8,
                                tabPanel("Forecast",
                                     plotOutput("weight_plot", height = 730)
                                    ),
                                
                                
                                tabPanel("Body Composition",
                                  plotOutput("comparison_plot", height = 730)
                                )
                               )
                              )
                             ),
        
    shinydashboard::tabItem(
      tabName = "Grocery_Opt",
      shiny::fluidRow(
        shinydashboard::box(width = 12, status = "info", solidHeader = TRUE, title = "Plan Summary", shiny::uiOutput("grocery_summary"))
      ),
      shiny::fluidRow(
        shiny::uiOutput("grocery_receipts")
      )
    ),
    
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