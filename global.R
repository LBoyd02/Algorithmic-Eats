library(tidyverse)
# Grab Stats Canada Table for Food Prices
url <- "https://www150.statcan.gc.ca/n1/tbl/csv/18100245-eng.zip"
temp <- tempfile(fileext = ".zip")
download.file(url, temp, quiet = TRUE, mode = "wb")
food_prices <- read.csv(unz(temp, "18100245.csv")) %>%
  dplyr::transmute(
    Date = as.Date(paste0(REF_DATE, "-01")),
    Location = GEO,
    Products = Products,
    Value = VALUE
  )
unlink(temp)

url2 <- "https://www.canada.ca/content/dam/hc-sc/migration/hc-sc/fn-an/alt_formats/zip/nutrition/fiche-nutri-data/cnf-fcen-csv.zip"
temp2 <- tempfile(fileext = ".zip")
download.file(url2, temp2, quiet = TRUE, mode = "wb")
food_names <- read.csv(unz(temp2, "FOOD NAME.csv"))
nutrient_amount <- read.csv(unz(temp2, "NUTRIENT AMOUNT.csv"))
unlink(temp2)

clean_food <- food_names %>%
  dplyr::filter(FoodGroupID %in% c(1,4,5,6,7,8,9,10,11,12,13,14,15,16,18,19,20)) %>% dplyr::select(FoodID, FoodDescription)

clean_nutrients <- nutrient_amount %>%
  dplyr::filter(NutrientID %in% c(203, 204, 205)) %>%
  dplyr::mutate(NutrientName = dplyr::case_match(
    NutrientID,
    203 ~ "Protein",
    204 ~ "Fat",
    205 ~ "Carbohydrates"
  )) %>%
  dplyr::select(FoodID, NutrientName, NutrientValue) %>%
  tidyr::pivot_wider(
    names_from = NutrientName,
    values_from = NutrientValue
  )

final <- clean_food %>%
  dplyr::left_join(clean_nutrients, by = "FoodID") %>%
  dplyr::select(-FoodID)


filtered_list <- food_prices %>%
  dplyr::filter(Date == "2025-12-01", Location == "Canada") %>%
  dplyr::filter(str_detect(Products, ",")) %>%
  tidyr::separate(Products, c("Base_Name", "Unit_Info"), sep = ",") %>%
  dplyr::mutate(
    quantity = as.numeric(stringr::str_extract(Unit_Info, "[0-9]+(\\.[0-9]+)?")),
    quantity = ifelse(is.na(quantity), 1, quantity),
    grams = dplyr::case_when(
      stringr::str_detect(Unit_Info, "kilogram|kilograms") ~ quantity * 1000,
      stringr::str_detect(Unit_Info, "gram|grams") ~ quantity,
      stringr::str_detect(Unit_Info, "millilitres") ~ quantity,
      stringr::str_detect(Unit_Info, "litre|litres") ~ quantity * 1000,

      # Edge Cases for "Unit" items
      # From https://www.mistralassociates.com/help/awmain    /weights_of_fruit_and_vegetables.html
      # Eggs - 1 Dozen = 600 grams
      stringr::str_detect(Base_Name, "Eggs") ~ 600,
      # Lemon & Lime = 100 grams
      stringr::str_detect(Base_Name, "Lemons") ~ 100,
      stringr::str_detect(Base_Name, "Limes") ~ 100,
      # Cantaloupe = 1000 grams
      stringr::str_detect(Base_Name, "Cantaloupe") ~ 1000,
      # Avocado = 200 grams
      stringr::str_detect(Base_Name, "Avocado") ~ 200,
      # Celery = 200 grams
      stringr::str_detect(Base_Name, "Celery") ~ 200,
      # Cucumber = 200 grams
      stringr::str_detect(Base_Name, "Cucumber") ~ 200,
      # Lettuce = 150 grams
      stringr::str_detect(Base_Name, "Iceberg Lettuce") ~ 150,
      stringr::str_detect(Base_Name, "Romaine Lettuce") ~ 150,
      # Broccoli
      stringr::str_detect(Base_Name, "Broccoli") ~ 200,
    ),
    Price_100g = round((Value / grams) * 100,2)
  ) %>%
  dplyr::select(Date, Location, Base_Name, Price_100g)
