# global.R
library(tidyverse)
library(readr)
library(stringr)
library(janitor)



food_bridge <- readr::read_csv("data/food_bridge.csv", show_col_types = FALSE) %>% janitor::clean_names() %>% dplyr::mutate(statcan_product = stringr::str_squish(statcan_product))



get_statcan_prices <- function() {
  url <- "https://www150.statcan.gc.ca/n1/tbl/csv/18100245-eng.zip"
  temp <- tempfile(fileext = ".zip")
  download.file(url, temp, quiet = TRUE, mode = "wb")
  prices <- utils::read.csv(unz(temp, "18100245.csv")) %>%
  dplyr::transmute(date = as.Date(paste0(REF_DATE, "-01")),
                   location = GEO,
                   statcan_product = stringr::str_squish(Products),
                   statcan_price = VALUE)
  unlink(temp)
  prices
}

get_cnf_data <- function() {
  url2 <- "https://www.canada.ca/content/dam/hc-sc/migration/hc-sc/fn-an/alt_formats/zip/nutrition/fiche-nutri-data/cnf-fcen-csv.zip"
  temp2 <- tempfile(fileext = ".zip")
  download.file(url2, temp2, quiet = TRUE, mode = "wb")
  food_names <- utils::read.csv(unz(temp2, "FOOD NAME.csv"))
  nutrient_amount <- utils::read.csv(unz(temp2, "NUTRIENT AMOUNT.csv"))
  unlink(temp2)
  list(food_names = food_names, nutrient_amount = nutrient_amount)
}



statcan_price_to_100g <- function(statcan_product, statcan_price) {
  unit_info <- stringr::str_trim(stringr::str_extract(statcan_product, "(?<=,).*"))
  base_name <- stringr::str_trim(stringr::str_remove(statcan_product, ",.*$"))
  qty <- suppressWarnings(readr::parse_number(unit_info))
  qty <- dplyr::if_else(is.na(qty), 1, qty)
  grams <- dplyr::case_when(
    stringr::str_detect(unit_info, stringr::regex("per kilogram", ignore_case = TRUE)) ~ 1000,
    stringr::str_detect(unit_info, stringr::regex("kilograms?", ignore_case = TRUE)) ~ qty * 1000,
    stringr::str_detect(unit_info, stringr::regex("grams?", ignore_case = TRUE)) ~ qty,
    stringr::str_detect(unit_info, stringr::regex("millilitres?", ignore_case = TRUE)) ~ qty,
    stringr::str_detect(unit_info, stringr::regex("litres?", ignore_case = TRUE)) ~ qty * 1000,
    stringr::str_detect(base_name, stringr::regex("^Eggs$", ignore_case = TRUE)) ~ 600,
    stringr::str_detect(base_name, stringr::regex("^Lemons?$", ignore_case = TRUE)) ~ 100,
    stringr::str_detect(base_name, stringr::regex("^Limes?$", ignore_case = TRUE)) ~ 100,
    stringr::str_detect(base_name, stringr::regex("^Cantaloupe$", ignore_case = TRUE)) ~ 1000,
    stringr::str_detect(base_name, stringr::regex("^Avocado$", ignore_case = TRUE)) ~ 200,
    stringr::str_detect(base_name, stringr::regex("^Celery$", ignore_case = TRUE)) ~ 200,
    stringr::str_detect(base_name, stringr::regex("^Cucumber$", ignore_case = TRUE)) ~ 200,
    stringr::str_detect(base_name, stringr::regex("Iceberg lettuce|Romaine lettuce", ignore_case = TRUE)) ~ 150,
    stringr::str_detect(base_name, stringr::regex("^Broccoli$", ignore_case = TRUE)) ~ 200,
    TRUE ~ NA_real_
  )
  (statcan_price / grams) * 100
}


make_cnf_macros <- function(food_names, nutrient_amount) {
  clean_food <- food_names %>%
  dplyr::filter(FoodGroupID %in% c(1,4,5,6,7,8,9,10,11,12,13,14,15,16,18,19,20)) %>%
  dplyr::transmute(food_id = FoodID, food_name = FoodDescription)
  
  macros <- nutrient_amount %>%
  dplyr::filter(NutrientID %in% c(203, 204, 205)) %>%
  dplyr::mutate(macro = dplyr::case_when(NutrientID == 203 ~ "protein_g", NutrientID == 204 ~ "fat_g", NutrientID == 205 ~ "carb_g")) %>%
  dplyr::transmute(food_id = FoodID, macro, value = NutrientValue) %>%
  tidyr::pivot_wider(names_from = macro, values_from = value)
  
  clean_food %>%
  dplyr::left_join(macros, by = "food_id")
}


build_model_foods <- function() {
  prices <- get_statcan_prices()
  cnf <- get_cnf_data()
  cnf_macros <- make_cnf_macros(cnf$food_names, cnf$nutrient_amount)
  prices_latest <- prices %>%
  dplyr::filter(location == "Canada") %>%
  dplyr::group_by(statcan_product) %>% dplyr::filter(date == max(date, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(price_100g = statcan_price_to_100g(statcan_product, statcan_price), food_name = stringr::str_squish(stringr::str_remove(statcan_product, ",.*$"))) %>% 
  dplyr::inner_join(food_bridge, by = "statcan_product") %>%
  dplyr::left_join(cnf_macros %>% dplyr::select(food_id, protein_g, carb_g, fat_g), by = "food_id") %>%
  dplyr::transmute(id = food_id, name = food_name, price_100g = round(price_100g, 4), protein_g, carb_g, fat_g) %>%
  dplyr::filter(!is.na(price_100g), !is.na(protein_g), !is.na(carb_g), !is.na(fat_g)) %>%
  dplyr::group_by(id) %>%
  dplyr::slice_max(order_by = price_100g, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup()
}


final_food_dataset <- build_model_foods()