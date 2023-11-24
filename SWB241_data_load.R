# SWB 231 H2H
# Data Transform & Load Procedure

library(readxl)
library(tidyverse)

# Load the raw data and updated column names
pth <- file.path(getwd(), "data", "2019 Members survey full data.xlsx")
results_2019 <- read_excel(pth)

pth <- file.path(getwd(), "data", "2023 Members survey full download.csv")
results_2023 <- read.csv(pth)

pth <- file.path(getwd(), "data", "2019 Column Rename.csv")
columns_2019 <- read.csv(pth)

pth <- file.path(getwd(), "data", "2023 Column Rename.csv")
columns_2023 <- read.csv(pth)


# Overwrite the raw column names with more descriptive alternatives
results_2019 <- results_2019 %>%
  setNames(columns_2019[["clean_column"]])

results_2023 <- results_2023 %>%
  setNames(columns_2023[["clean_column"]])


# Remove false header row generated from output
results_2019 <- results_2019[-1,]


# Function for squishing responses in a column to 1 or 0, passing in name of column
# Calling function with list of multi-valid questions

convert_column_binary <- function(survey, column_name) {
  survey[column_name] <- survey[column_name] %>%
    replace(!is.na(.), "1")
  
  survey[column_name] <- survey[column_name] %>%
    replace(is.na(.), "0")
  
  survey <- survey %>% 
    mutate_at(c(column_name), as.numeric)
  
  return(survey)
}

Multi_Valid_Columns_2019 <- columns_2019 %>%
  filter(multi_valid == "yes") %>%
  .$clean_column

for(column_name in Multi_Valid_Columns_2019) {
  results_2019 <- convert_column_binary(results_2019, column_name)
}

Multi_Valid_Columns_2023 <- columns_2023 %>%
  filter(multi_valid == "yes") %>%
  .$clean_column

for(column_name in Multi_Valid_Columns_2023) {
  results_2023 <- convert_column_binary(results_2023, column_name)
}


# TODO 

# Assigning classes to some of the open responses with overlap in 2023 and 2019 surveys

# Pulling out a rank score in SQ16_Rank_External_Engagement_Approach in 2023 survey


# Exporting data in clean .csvs

pth <- file.path(getwd(), "data", "2019 Members survey clean.csv")
write.csv(results_2019, pth, row.names = FALSE)

pth <- file.path(getwd(), "data", "2023 Members survey clean.csv")
write.csv(results_2023, pth, row.names = FALSE)

