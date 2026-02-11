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
        
        shiny::selectInput(
          "height_cm_2", "Height",
          choices  = setNames(height$cm, height$concat),
          selected = 178
        ),
        shiny::numericInput("weight_lb_2", "Current Weight (lbs)", 180),
        shiny::numericInput("age_2", "Age (Years)", 21),
        shiny::selectInput("sex_2", "Sex", choices = c("Male", "Female"), selected = "Male"),
        
        tags$hr(),
        
        shiny::radioButtons(
          "body_type_2", "Body Archetype",
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
          "cardio_frequency_2", "Cardio Frequency",
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
          "lift_frequency_2", "Weight Training Frequency",
          choiceValues = c("lift_none", "lift_low", "lift_mid", "lift_high"),
          choiceNames  = list(
            tagList(icon("ban"), " None"),
            tagList(icon("user"), " 1-2 sessions / week"),
            tagList(icon("dumbbell"), " 3-4 sessions / week"),
            tagList(icon("weight-hanging"), " 5+ sessions / week")
          ),
          selected = "lift_mid"
        )
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
      fluidRow(
        box(
          title = "Monthly Budget",
          width = 3,
          solidHeader = TRUE,
          status = "primary",
          
          sliderInput("budget", "Budget ($)", min = 0, max = 2000, value = 1000, step = 25, ticks = FALSE),
          uiOutput("budget_label"),
          actionButton("reset_exclusions", "Reset Removed Foods"),
          tags$hr(),
          uiOutput("grocery_stats")
        ),
        
        box(title = "Protein", width = 3, solidHeader = TRUE, DT::DTOutput("protein_table")),
        box(title = "Carbs",   width = 3, solidHeader = TRUE, DT::DTOutput("carb_table")),
        box(title = "Fats",    width = 3, solidHeader = TRUE, DT::DTOutput("fat_table"))
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