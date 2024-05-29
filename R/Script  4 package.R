hello <- function() {
  print("Hello, world!")
}

# Load necessary libraries
library(dplyr)
library(readxl)
library(haven)

#Paths to the documents
root <- "C:/Users/HP/Desktop/ISEP2CoursR2024/Nuovo_packages/Prenditi_packages/Buonepackages"
Datawork <- file.path(root, "Base")

inputfile_1 <- file.path(Datawork, "Table de conversion phase 2.xlsx")

inputfile_2 <- file.path(Datawork, "fruits.dta")
produit = "fruits"

# Function to read and prepare data
read_and_prepare_data <- function(produit_path, conversion_path) {
  produit_dta <- read_dta(inputfile_2)
  conversion_data <- read_excel(inputfile_1)
  names(produit_dta)[names(produit_dta) == paste0(produit,"__id")] <- "produitID"
  names(produit_dta)[names(produit_dta) == paste0("s07Bq03b_", produit)] <- "uniteID"
  names(produit_dta)[names(produit_dta) == paste0("s07Bq03c_", produit)] <- "tailleID"
 list(produit = produit_dta, conversion_data = conversion_data)

}


# Read and prepare data
data <- read_and_prepare_data(produit_path, conversion_path)

# Function to merge product data with conversion data
merge_data <- function(product_data, conversion_data, product) {
  # Select relevant columns from conversion data
  conversion_selected <- conversion_data[c("productID", "unitID", "sizeID", "weight")]

  # Rename product ID column in product data
  names(product_data)[names(product_data) == paste0(product, "__id")] <- "productID"

  # Merge product data with conversion data
  product_merged <- merge(product_data, conversion_selected, by = c("productID", "unitID", "sizeID"), all.x = TRUE)

  # Filter out rows with NA values in the specified column
  product_merged <- product_merged %>% filter(!is.na(product_merged[[paste0("s07Bq07a_", product)]]))

  # Convert weight and quantity columns to numeric
  product_merged$weight <- as.numeric(product_merged$weight)
  product_merged[[paste0("s07Bq03a_", product)]] <- as.numeric(product_merged[[paste0("s07Bq03a_", product)]])

  # Calculate quantity consumed in kg
  product_merged$quantity_consumed_kg <- (product_merged$weight * product_merged[[paste0("s07Bq03a_", product)]]) / 1000

  return(product_merged)
}

# Merge product data with conversion data
product_merged <- merge_data(data[["product"]], data[["conversion_data"]], product)
View(product_merged)

# Function to process purchase data
process_purchases <- function(product_merged, product) {
  # Select relevant columns for purchases
  purchase_columns <- c("productID", paste0("s07Bq07a_", product), paste0("s07Bq07b_", product), paste0("s07Bq07c_", product), paste0("s07Bq08_", product), "weight")
  product_purchases <- product_merged[purchase_columns]

  # Rename columns
  colnames(product_purchases)[2:5] <- c("quantity", "unitID", "sizeID", "value")

  # Group by relevant columns and calculate mean value
  product_purchases <- product_purchases %>%
    group_by(productID, quantity, unitID, sizeID) %>%
    mutate(value = mean(value)) %>%
    ungroup() %>%
    unique()

  # Rename quantity column in product_merged
  names(product_merged)[names(product_merged) == paste0("s07Bq03a_", product)] <- "quantity"

  # Merge product_merged with product_purchases
  product_merged <- merge(product_merged, product_purchases, by = c("productID", "unitID", "sizeID", "quantity"), all.x = TRUE)

  # Calculate unmatching rate
  unmatching_rate <- sum(is.na(product_merged$value)) / length(product_merged$value) * 100
  print(unmatching_rate)

  return(product_purchases)
}

# Process purchase data
purchase_data <- process_purchases(product_merged, product)
View(purchase_data)

# Function to handle unmatched data and finalize purchases
finalize_purchases <- function(purchases, conversion_data, merged_data) {
  # Calculate price per unit in grams
  purchases$price_per_gram <- as.numeric(purchases$value) / (as.numeric(purchases$weight) * as.numeric(purchases$quantity))

  # Calculate mean price per unit in grams by productID
  finalized_purchases <- purchases %>%
    select(productID, price_per_gram) %>%
    group_by(productID) %>%
    mutate(price_per_gram = mean(price_per_gram, na.rm = TRUE)) %>%
    distinct()

  # Merge finalized_purchases with merged_data
  merged_data <- merge(merged_data, finalized_purchases, by = "productID", all.x = TRUE)

  # Replace NA values in value column
  merged_data$value[is.na(merged_data$value)] <- merged_data$price_per_gram[is.na(merged_data$value)] * merged_data$weight[is.na(merged_data$value)]

  return(merged_data)
}

# Finalize purchase data
final_product_data <- finalize_purchases(purchase_data, data[["conversion_data"]], product_merged)

