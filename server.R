library(shiny)
library(plotly)
library(leaflet)
library(httr2)
library(googlePolylines)

server <- function(input, output, session) {
  
  # --- Macro Calculator ---
  
  
  # --- Grocery Optimizer ---
  
  
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
