library(shiny)
library(plotly)
library(leaflet)
library(httr2)
library(googlePolylines)
library(tidyverse)
library(lpSolve)
library(DT)

#--- calcs for macro optimizer ---
 weight_simulation <- function(height_cm, 
                               weight_lb, 
                               age, 
                               sex,
                               body_type,
                               cal, 
                               cardio_frequency, 
                               lift_frequency,
                               goal,
                               weeks = 52){
   
   height_cm <- as.numeric(height_cm)
   weight_lb <- as.numeric(weight_lb)
   age <- as.numeric(age)
   sex <- as.character(sex)
   cal <- as.numeric(cal)
   body_type <- as.character(body_type)
   goal <- as.numeric(goal)
   
   lb_to_kg <- function(lb) lb * 0.453592
   
   #Mifflin-St Jeor bmr is the basal metabolic rate - how many calories a person burns naturally throughout the day
   mifflin_bmr <- function(kg, cm, age, sex) {
     base <- 10 * kg + 6.25 * cm - 5 * age
     if (tolower(sex) == "male") base + 5 else base - 161
    
   }
   
   bf0 <- if (tolower(sex) == "male") {
     
    switch (body_type,
      lean = 0.12,
      muscular = 0.15,
      balanced = 0.2,
      overweight = 0.28,
      0.2
    )
   } else {
     switch (body_type,
      lean = 0.2,
      muscular = 0.24,
      balanced = 0.3,
      overweight = 0.38,
      0.3
     )
   }
   
   
   #converting our activity inputs to approximate multipliers for calculations
   cardio_addon <- switch(
     cardio_frequency,
     "cardio_none" = 0,
     "cardio_low" = 0.10,
     "cardio_mid" = 0.20,
     "cardio_high" = 0.35,
     0
   )
   
   lift_addon <- switch(
     lift_frequency,
     "lift_none" = 0,
     "lift_low" = 0.10,
     "lift_mid" = 0.20,
     "lift_high" = 0.35,
     0
   )
   
   #to calculate total daily energy expenditure (calories)
   activity_factor <- 1.2 + cardio_addon + lift_addon
   
   #initialize body fat and fat-free mass
   fat_lb <- weight_lb * bf0
   ffm_lb <- weight_lb - fat_lb
   
   min_bf <- if (tolower(sex) == "male")  0.05 else 0.13
   min_fat_from_ffm <- function(ffm, min_bf) (min_bf / (1 - min_bf)) * ffm
   
   #initialize df to track cal over time using below metrics
   out <- data.frame(
     week = 0:weeks,
     weight_lb = NA_real_,
     bmr = NA_real_,
     tdee = NA_real_,
     kcal_balance_day = NA_real_,
     fat_lb = NA_real_,
     ffm_lb = NA_real_,
     bf_pct = NA_real_
   )
   
   for (t in 0:weeks) {
     
     w <- fat_lb + ffm_lb
     kg <- lb_to_kg(w)
     
     bmr <- mifflin_bmr(kg, height_cm, age, sex)
     tdee <- bmr * activity_factor
     bal_day <- cal - tdee
     
     out$weight_lb[t + 1] <- w
     out$bmr[t + 1] <- bmr
     out$tdee[t + 1] <- tdee
     out$kcal_balance_day[t + 1] <- bal_day
     out$fat_lb[t + 1] <- fat_lb
     out$ffm_lb[t + 1] <- ffm_lb
     out$bf_pct[t + 1] <- 100 * fat_lb / (fat_lb + ffm_lb)
     
     #update for week and convert to weight (3500 kcal = 1 lb)
     delta_w_week <- (7 * bal_day)/ 3500
     
  #If you lift more, you will be able to retain more muscle when losing weight
     if (delta_w_week < 0) {
       ffm_loss_frac <- switch(
         lift_frequency,
         "lift_none" = 0.25,
         "lift_low" = 0.2,
         "lift_mid" = 0.15,
         "lift_high" = 0.1,
         0.2
       )
       
       if (goal == 2) ffm_loss_frac <- ffm_loss_frac - 0.03
       
       ffm_loss_frame <- max(0.06, min(0.35, ffm_loss_frac))
       
       loss <- abs(delta_w_week)
       ffm_loss <- loss * ffm_loss_frac
       fat_loss <- loss - ffm_loss
       
       ffm_lb <- max(0, ffm_lb - ffm_loss)
       fat_lb <- max(0, fat_lb - fat_loss)
     } else if (delta_w_week > 0) {
       #on the other hand, if gaining weight, lifters will gain relatively more muscle than fat
       
       ffm_gain_frac <- switch(
         lift_frequency,
         "lift_none" = 0.1,
         "lift_low" = 0.2,
         "lift_mid" = 0.3,
         "lift_high" = 0.4,
         0.2
       )
       
       if (goal == 4) ffm_gain_frac <- ffm_gain_frac + 0.03
       if (goal == 5) ffm_gain_frac <- ffm_gain_frac + 0.05
       
       ffm_gain_frac <- max(0.10, min(0.55, ffm_gain_frac))
       
       gain <- delta_w_week
       ffm_gain <- gain * ffm_gain_frac
       fat_gain <- gain - ffm_gain
       
       ffm_lb <- ffm_lb + ffm_gain
       fat_lb <- fat_lb + fat_gain     
     }
     
     #minimum BF% so users can't break the system
     
     min_fat <- min_fat_from_ffm(ffm_lb, min_bf)
     if (fat_lb < min_fat) fat_lb <- min_fat
   }
   
   out
 }


server <- function(input, output, session) {
  
  # --- Macro Calculator ---
  
  sim_data <- reactive({
    
    weight_simulation(
      height_cm = input$height_cm,
      weight_lb = input$weight_lb,
      age = input$age,
      sex = input$sex,
      cal = input$cal,
      cardio_frequency = input$cardio_frequency,
      lift_frequency = input$lift_frequency,
      weeks = 52,
      body_type = input$body_type,
      goal = input$goal
    )
  })
  
  output$goal_label <- renderUI({
    lab <- switch(
      as.character(input$goal),
      "1" = "Weight Loss - ",
      "2" = "Cut & Sculpt - ",
      "3" = "Maintain & Perform - ",
      "4" = "Lean Bulk - ",
      "5" = "Bulk - "
    )
    goal_desc <- switch(
      as.character(input$goal),
      "1" = "Prioritize weight loss",
      "2" = "Lose weight and preserve/gain muscle",
      "3" = "Maintain weight and preserve/gain muscle",
      "4" = "Gain muscle with slight weight gain",
      "5" = "Prioritize muscle gain"
    )
    
    tagList(
      tags$b(paste0("Selected goal: ", lab)),
      tags$span(goal_desc)
    )
  })
    
  maintenance_calories <- reactive({
    df <- sim_data()
      round(df$tdee[1])
  })   
  
  output$maint_box <- renderValueBox({
    valueBox(
      value = paste0(format(round(maintenance_calories()), big.mark = ","), " Calories"),
      subtitle = "Estimated daily consumption to maintain weight",
      icon = icon("burger"),
      color = "blue"
    )
  })
  
  balance_kcal <- reactive({
    df <- sim_data()
    df$kcal_balance_day[1]
  })
  
  protein_min_g <- reactive({
    #protein g/lb by goal
    base_g_lb <- switch(
      as.character(input$goal),
      "1" = 0.75,
      "2" = 1,
      "3" = 0.75,
      "4" = 0.8,
      "5" = 0.8,
      0.75
    )
    
    #bump up protein to recover from lifting
    lift_bump <- switch(
      input$lift_frequency,
      "lift_none" = 0,
      "lift_low" = 0.05,
      "lift_mid" = 0.08,
      "lift_high" = 0.1,
      0.05
      )
    (base_g_lb + lift_bump) * as.numeric(input$weight_lb)
  })
  
  output$balance_box <- renderValueBox({
    b <- balance_kcal()
    valueBox(
      value = paste0(format(round(abs(b)), big.mark = ","), " kcal/day"),
      subtitle = if (b >= 0) "Estimated surplus" else "Estimated deficit",
      icon = icon("scale-unbalanced"),
      color = if (b >= 0) "yellow" else "aqua"
    )
  })
  
  output$protein_box <- renderValueBox({
    valueBox(
      value = paste0(format(round(protein_min_g()), big.mark = ","), " grams"),
      subtitle = "Minimum daily protein target",
      icon = icon("drumstick-bite"),
      color = "olive"
    )
  })
  
  output$year_box <- renderValueBox({
    df <- sim_data()
    now <- df[1, ]
    yr <- df[nrow(df), ]
    
    valueBox(
      value = paste0(round(yr$weight_lb, 1), " lb"),
      subtitle = paste0("Body Fat % after 1 year: ", round(yr$bf_pct, 1), "% (today: ", round(now$bf_pct, 1), "%)"),
      icon = icon("bullseye"),
      color = "teal"
    )
  })
  
  output$weight_plot <- renderPlot({
    df <- sim_data()
    
    ggplot(df, aes(x = week)) +
      geom_line(aes(y = weight_lb, color = "Weight"), linewidth = 2) +
      geom_line(aes(y = ffm_lb, color = "Fat-Free Mass"), linewidth = 1.5, linetype = "dashed") +
      scale_color_manual(
        values = c(
          "Weight" = "limegreen",
          "Fat-Free Mass" = "firebrick2"
        )
      ) +
      scale_y_continuous(expand = expansion(mult = c(0.5, 0.5))) +
      labs(
        title = "Projected Weight Over Time",
        x = "Week",
        y = "weight (lb)"
      )
  })    
  
  output$comparison_plot <- renderPlot({
    df <- sim_data()
    now <- df[1, ]
    yr <- df[nrow(df), ]
    
    comp <- data.frame(
      time = c("Today", "Today", "1 Year", "1 Year"),
      component = c("Weight", "Body Fat", "Weight", "Body Fat"),
      value = c(now$weight_lb, now$fat_lb, yr$weight_lb, yr$fat_lb)
    )
    
    comp$time <- factor(comp$time, levels = c("Today", "1 Year"))
    
    ggplot(comp, aes(x = time, y = value, fill = component)) +
      geom_col() +
      labs(title = "Body Composition (Fat vs Lean)", x = NULL, y = "Pounds") 
  })
  
  # --- Grocery Optimizer ---
  excluded_ids <- reactiveVal(integer(0))
  
  observeEvent(input$exclude_food_id, {
    id <- as.integer(input$exclude_food_id)
    excluded_ids(unique(c(excluded_ids(), id)))
  })
  
  observeEvent(input$reset_exclusions, {
    excluded_ids(integer(0))
  })
  
  budget_day <- reactive({
    req(input$budget)
    as.numeric(input$budget) / 30
  })
  
  protein_min_g_groc <- reactive({
    req(input$weight_lb_groc, input$goal_groc, input$lift_frequency_groc)
    
    base_g_lb <- switch(
      as.character(input$goal_groc),
      "1" = 0.75,
      "2" = 1.00,
      "3" = 0.75,
      "4" = 0.80,
      "5" = 0.80,
      0.75
    )
    
    lift_bump <- switch(
      input$lift_frequency_groc,
      "lift_none" = 0,
      "lift_low"  = 0.05,
      "lift_mid"  = 0.08,
      "lift_high" = 0.10,
      0.05
    )
    
    (base_g_lb + lift_bump) * as.numeric(input$weight_lb_groc)
  })
  
  target_calories_groc <- reactive({
    req(input$cal_groc)
    as.numeric(input$cal_groc)
  })
  
  opt_foods <- reactive({
    df <- food_catalog
    
    if (length(excluded_ids()) > 0) {
      df <- df %>% dplyr::filter(!id %in% excluded_ids())
    }
    
    df %>%
      dplyr::filter(
        !is.na(price_100g), price_100g > 0,
        !is.na(kcal_100g),  kcal_100g > 0,
        !is.na(protein_g),
        !is.na(carb_g),
        !is.na(fat_g)
      )
  })
  
  optimizer_solution <- reactive({
    df <- opt_foods()
    validate(need(nrow(df) > 0, "No Foods Available"))
    
    n_foods <- nrow(df)
    
    cost_per_g <- df$price_100g / 100
    prot_per_g <- df$protein_g / 100
    carb_per_g <- df$carb_g / 100
    fat_per_g  <- df$fat_g / 100
    kcal_per_g <- df$kcal_100g / 100
    
    protein_target_g <- as.numeric(protein_min_g_groc())
    calorie_target_kcal <- as.numeric(target_calories_groc())
    
    tol <- as.numeric(input$cal_tol_pct) / 100
    calorie_min_kcal <- calorie_target_kcal * (1 - tol)
    calorie_max_kcal <- calorie_target_kcal * (1 + tol)
    
    min_g <- as.numeric(input$min_serv_g)
    max_g <- as.numeric(input$max_serv_g)
    
    daily_budget <- budget_day()
    
    constraints <- list(
      values = rbind(
        prot_per_g,
        kcal_per_g,
        kcal_per_g,
        cost_per_g
      ),
      direction = c(">=", ">=", "<=", "<="),
      limit = c(protein_target_g, calorie_min_kcal, calorie_max_kcal, daily_budget)
    )
    
    min_g <- 0
    max_g <- 1000
    
    solution <- lpSolve::lp(
      direction   = "min",
      objective.in = cost_per_g,
      const.mat   = constraints$values,
      const.dir   = constraints$direction,
      const.rhs   = constraints$limit,
      lower       = rep(min_g, n_foods),
      upper       = rep(max_g, n_foods),
      all.int     = FALSE
    )
    
    validate(need(solution$status == 0, "No Feasible Solution"))
    
    grams_day <- solution$solution
    
    plan_day <- df %>%
      dplyr::mutate(
        grams_day = grams_day,
        cost_day = grams_day * cost_per_g,
        kcal_day = grams_day * kcal_per_g,
        protein_day = grams_day * prot_per_g,
        carb_day = grams_day * carb_per_g,
        fat_day = grams_day * fat_per_g
      ) %>%
      dplyr::filter(grams_day > 1e-6)
    
    days <- as.numeric(input$days)
    
    summary <- plan_day %>%
      dplyr::summarise(
        foods = dplyr::n(),
        cost_day = sum(cost_day),
        kcal_day = sum(kcal_day),
        protein_day = sum(protein_day),
        carb_day = sum(carb_day),
        fat_day = sum(fat_day),
        cost_period = cost_day * days
      )
    
    list(
      plan_day = plan_day,
      summary = summary,
      days = days
    )
  })
  
  output$budget_label <- renderUI({
    tags$b(paste0("Daily budget: $", format(round(budget_day(), 2), nsmall = 2)))
  })
  
  output$grocery_stats <- renderUI({
    s <- optimizer_solution()$summary
    tagList(
      tags$p(tags$b("Plan summary (per day)")),
      tags$ul(
        tags$li(paste0("Foods used: ", s$foods)),
        tags$li(paste0("Cost: $", format(round(s$cost_day, 2), nsmall = 2))),
        tags$li(paste0("Calories: ", round(s$kcal_day), " kcal")),
        tags$li(paste0("Protein: ", round(s$protein_day), " g")),
        tags$li(paste0("Carbs: ", round(s$carb_day), " g")),
        tags$li(paste0("Fats: ", round(s$fat_day), " g")),
        tags$li(paste0("Cost (", optimizer_solution()$days, " days): $", format(round(s$cost_period, 2), nsmall = 2)))
      )
    )
  })
  
  dt_with_remove <- function(plan_day, sort_col) {
    df2 <- plan_day %>%
      dplyr::arrange(dplyr::desc(.data[[sort_col]])) %>%
      dplyr::transmute(
        remove = sprintf(
          '<button class="btn btn-xs btn-danger remove-food" data-id="%s" type="button">x</button>',
          id
        ),
        id,
        name,
        grams_day = round(grams_day, 0),
        macro_g = round(.data[[sort_col]], 1),
        cost_day = round(cost_day, 2)
      )
    
    DT::datatable(
      df2,
      escape = FALSE,
      rownames = FALSE,
      options = list(
        dom = "tip",
        paging = FALSE,
        scrollY = "650px",
        scroller = TRUE,
        columnDefs = list(
          list(targets = 1, visible = FALSE),
          list(width = "28px", targets = 0),
          list(width = "70px", targets = 3),
          list(width = "70px", targets = 4),
          list(width = "70px", targets = 5)
        )
      ),
      callback = DT::JS(
        "table.on('click', 'button.remove-food', function(){",
        "  var id = $(this).attr('data-id');",
        "  Shiny.setInputValue('exclude_food_id', id, {priority: 'event'});",
        "});"
      )
    )
  }
  
  output$protein_table <- DT::renderDT({
    res <- optimizer_solution()
    dt_with_remove(res$plan_day, "protein_day")
  }, server = FALSE)
  
  output$carb_table <- DT::renderDT({
    res <- optimizer_solution()
    dt_with_remove(res$plan_day, "carb_day")
  }, server = FALSE)
  
  output$fat_table <- DT::renderDT({
    res <- optimizer_solution()
    dt_with_remove(res$plan_day, "fat_day")
  }, server = FALSE)
  
  # --- Gym Map ---
  shiny::observeEvent(c(input$address, input$map_mode), {
    shiny::req(input$address)
    
    # --- (Geocoding Address) ---
    validation = get_lat_lon(input$address)
    if (!is.null(validation)){
      r$origin_lat_lng = get_lat_lon(input$address)
      origin_coords <- paste(r$origin_lat_lng$lat, r$origin_lat_lng$lng, sep = ",")
      
      # --- (Find Nearby Gyms) ---
      search_request <- 
        httr2::request("https://maps.googleapis.com/maps/api/place/nearbysearch/json") %>% 
        httr2::req_url_query(
          location = origin_coords,
          rankby = "distance",
          keyword = "gym",
          key = api_key
        )
      
      search_results <- httr2::req_perform(search_request) %>% 
        httr2::resp_body_json(simplifyVector = TRUE)
      
      # --- (Cleaning Gym Data) ---
      r$gym_results <- search_results$results %>% 
        dplyr::filter(user_ratings_total >= 50,
                      purrr::map_chr(types, 1) == "gym") %>% 
        dplyr::transmute(
          Gym_Name = name,
          Reference = reference,
          Address = vicinity,
          Coordinates = paste(geometry$location$lat, geometry$location$lng, sep = ","),
          Latitude = geometry$location$lat,
          Longitude = geometry$location$lng,
          Phone_Number = international_phone_number,
          Rating = rating,
          Rating_Amount = user_ratings_total
        ) %>%
        # Accounts for multiple of the exact same Gym Names
        dplyr::group_by(Gym_Name) %>% 
        dplyr::mutate(Num = dplyr::row_number(Gym_Name), 
                      Count = NROW(Gym_Name),
                      Name = dplyr::if_else(Count > 1, paste(Gym_Name, Num), Gym_Name)) %>%
        dplyr::ungroup() %>% 
        dplyr::select(-c(Count, Gym_Name, Num))
      
      r$closest_gyms <- r$gym_results %>%
        head(5) %>%
        cbind(gym_colors)
      
      r$gym_results <- r$gym_results %>% 
        dplyr::left_join(r$closest_gyms, 
                         by = dplyr::join_by(Name, Address, Coordinates, Latitude, Longitude, Phone_Number, Rating, Rating_Amount)) %>% 
        dplyr::mutate(Color = ifelse(is.na((gym_colors)), "#3AE492", gym_colors),
                      .keep = "unused")
      
      destinations <- paste(r$closest_gyms$Coordinates, collapse = "|")
      
      # --- (Time to Location) ---
      time_request <- httr2::request("https://maps.googleapis.com/maps/api/distancematrix/json") %>% 
        httr2::req_url_query(
          origins = origin_coords,
          destinations = destinations,
          mode = input$map_mode,
          key = api_key
        )

            time_response <- httr2::req_perform(time_request) %>% 
        httr2::resp_body_json(simplifyVector = TRUE)
      
      travel_times <- time_response$rows$elements[[1]]$duration$text
      r$closest_gyms$Drive_Time <- travel_times
      
      shiny::req(r$gym_results)
      shiny::updateSelectInput(
        session = session, 
        inputId = "selected_gym", 
        label = "Gym Selected",
        choices = sort(unique(r$gym_results$Name)),
        selected = sort(unique(r$gym_results$Name))[1]
      )
      r$route_response <- NULL
    }
  })
  
  # --- (Routing w/ Polyline) ---
  shiny::observeEvent(c(input$map_mode, input$selected_gym, r$origin_lat_lng), {
    shiny::req(r$origin_lat_lng, input$map_mode, input$selected_gym, r$gym_results)
    target_gym <- r$gym_results %>%
      dplyr::filter(Name == input$selected_gym)

    shiny::req(nrow(target_gym) > 0)
    
    route_request <- httr2::request("https://routes.googleapis.com/directions/v2:computeRoutes") %>%
      httr2::req_headers(
        "X-Goog-Api-Key" = api_key,
        "X-Goog-FieldMask" = "routes.duration,routes.distanceMeters,routes.polyline.encodedPolyline"
      ) %>%
      httr2::req_body_json(list(
        origin = list(
          location = list(
            latLng = list(
              latitude = r$origin_lat_lng$lat[1],
              longitude = r$origin_lat_lng$lng[1]
            )
          )
        ),
        destination = list(
          location = list(
            latLng = list(
              latitude = as.numeric(target_gym$Latitude)[1],
              longitude = as.numeric(target_gym$Longitude)[1]
            )
          )
        ),
        travelMode = ifelse(input$map_mode == "driving", "DRIVE", "WALK")
      ))
    r$route_response <- httr2::req_perform(route_request) %>%
      httr2::resp_body_json(simplifyVector = TRUE)
  })
  
  # --- (Leaflet Map) ---
  output$gym_map <- leaflet::renderLeaflet({
    shiny::req(r$gym_results, r$route_response, r$closest_gyms)
    if (!is.null(googlePolylines::decode(r$route_response$routes$polyline$encodedPolyline)[[1]])){ 
      route_duration <- lubridate::seconds_to_period(as.numeric(gsub("\\D", "", r$route_response$routes$duration)))
      target_gym <- r$gym_results %>% 
        dplyr::filter(Name == input$selected_gym)
      
      leaflet::leaflet(data = r$gym_results) %>%
        leaflet::addProviderTiles(providers$CartoDB.Positron) %>%
        # All Gym  Markers
        leaflet::addCircleMarkers(
          lng = ~Longitude,
          lat = ~Latitude,
          radius = 8,
          color = ~Color,
          weight = 2,
          fillOpacity = 0.7,
          popup = ~paste0(
            "<div style='", main_font, "'>",
            "<b>", Name, "</b><br>",
            "<b>Rating:</b> ", Rating, " &#9734; (", Rating_Amount, " reviews)<br>",
            "<b>Address:</b> ", Address,
            "</div>"
          ),
          label = ~Name,
          labelOptions = leaflet::labelOptions(style = label_font)
        ) %>%
        # Origin Marker
        leaflet::addCircleMarkers(data = r$origin_lat_lng,
          lng = ~lng,
          lat = ~lat,
          radius = 8,
          color = "#FF0000",
          weight = 2,
          fillOpacity = 1,
          popup = ~paste0(
            "<div style='", main_font, "'>",
            "<b> Current Location: </b><br>",
            input$address,
            "</div>"
          ),
          label = "Current Location",
          labelOptions = leaflet::labelOptions(
            style = label_font,
            noHide = TRUE,
            direction = "top"
          )
        ) %>%
        # Target Gym Marker
        leaflet::addCircleMarkers(data = target_gym,
          lng = ~Longitude,
          lat = ~Latitude,
          radius = 7,
          color = "#000000",
          weight = 2,
          fillOpacity = 1,
          popup = ~paste0(
            "<div style='", main_font, "'>",
            "<b>", Name, "</b><br>",
            "<b>Rating:</b> ", Rating, " &#9734; (", Rating_Amount, " reviews)<br>",
            "<b>Address:</b> ", Address,
            "</div>"
          ),
          label = "Destination",
          labelOptions = leaflet::labelOptions(
            style = label_font,
            noHide = TRUE,
            direction = "top"
          )
        ) %>%
        # Route Polyline
        leaflet::addPolylines(
          data = googlePolylines::decode(r$route_response$routes$polyline$encodedPolyline)[[1]],
          lat = ~lat, 
          lng = ~lon,
          color = "#000000",
          popup = ~paste0(
            "<div style='", main_font, "'>",
            "<b>", "Selected Gym: </b>", input$selected_gym, "<br>", 
            "<b>Duration:</b> ", route_duration, 
            "<br><b>Distance (m):</b> ", format(r$route_response$routes$distanceMeters, big.mark = ","),
            "</div>"
          ),
        ) %>% 
        # Legend to Show 5 Closest Gyms
        leaflet::addLegend(
          position = "bottomright",
          title = paste0("Time if ", ifelse(input$map_mode == "driving", "Driving:", "Walking:")),
          labels = ~paste0("<b>", r$closest_gyms$Name, ": ", "</b>", r$closest_gyms$Drive_Time),
          colors = ~gym_colors
        ) %>% 
        leaflet::fitBounds(
          ~min(r$gym_results$Longitude),
          ~min(r$gym_results$Latitude),
          ~max(r$gym_results$Longitude),
          ~max(r$gym_results$Latitude)
        )
      }
  })
}
