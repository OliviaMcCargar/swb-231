# SWB 231 H2H
# Data Transform & Load Procedure

library(readxl)
library(tidyverse)

rm(list=ls()); cat("\014") # Clear Workspace and Console

# Load the raw data and updated column names
pth <- file.path(getwd(), "data", "2019 Members survey full data.xlsx")
results_2019 <- read_excel(pth)

pth <- file.path(getwd(), "data", "2023 Members survey full download.csv")
results_2023 <- read.csv(pth)

pth <- file.path(getwd(), "data", "2019 Column Rename.csv")
columns_2019 <- read.csv(pth)

pth <- file.path(getwd(), "data", "2023 Column Rename.csv")
columns_2023 <- read.csv(pth)

pth <- file.path(getwd(), "data", "Open Response Classification Map.csv")
open_response_classification_map <- read.csv(pth)


# Overwrite the raw column names with more descriptive alternatives
results_2019 <- results_2019 %>%
  setNames(columns_2019[["clean_column"]])

results_2023 <- results_2023 %>%
  setNames(columns_2023[["clean_column"]])


# Remove false header row generated from output
results_2019 <- results_2019[-1,]

# Coerce empty responses to NA
results_2019 <- results_2019 %>% mutate_if(is.character, list(~na_if(.,"")))
results_2023 <- results_2023 %>% mutate_if(is.character, list(~na_if(.,"")))

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

# Assigning clean values to some of the open responses with overlap in 2023 and 2019 surveys
# The Open Response Classification Map file in the data folder can be updated as needed for this map
# Note this does not reshape the data only directly overwrites one value at a time

assign_class_open_response <- function(year, question, raw_open_response) {
  return(
    open_response_classification_map %>% 
      filter(survey_year == year, column == question) %>%
      filter(raw_response == raw_open_response) %>%
      .$clean_response %>%
      unique()
  )
}

for(year in c(2019,2023)) {
  for(question in unique(open_response_classification_map %>% filter(survey_year == year) %>% .$column)) {
    if(year == 2023) {
      for(numrow in 1:length(results_2023[,question])) {
        raw <- results_2023[numrow,question]
        results_2023[numrow,question] <- assign_class_open_response(year,question,raw)
      }
    } else if (year == 2019) {
      for(numrow in 1:nrow(results_2019[,question])) {
        raw <- results_2019[[numrow,question]]
        results_2019[[numrow,question]] <- assign_class_open_response(year,question,raw)
      }
    }
  }
}


# Pulling out a rank score in SQ16_Rank_External_Engagement_Approach in 2023 survey


# Exporting data in clean .csvs

pth <- file.path(getwd(), "data", "2019 Members survey clean.csv")
write.csv(results_2019, pth, row.names = FALSE)

pth <- file.path(getwd(), "data", "2023 Members survey clean.csv")
write.csv(results_2023, pth, row.names = FALSE)

# clean up the environment when importing
rm(list=setdiff(ls(), c("results_2019", "results_2023")))

