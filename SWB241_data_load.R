# SWB 231 H2H
# Data Transform & Load Procedure

library(readxl)
library(tidyverse)

pth <- file.path(getwd(), "data", "2019 Members survey full data.xlsx")
results_2019 <- read_excel(pth)

pth <- file.path(getwd(), "data", "2023 Members survey full download.csv")
results_2023 <- read.csv(pth)

pth <- file.path(getwd(), "data", "2019 Column Rename.csv")
columns_2019 <- read.csv(pth)

pth <- file.path(getwd(), "data", "2023 Column Rename.csv")
columns_2023 <- read.csv(pth)

results_2019 <- results_2019 %>%
  setNames(columns_2019[["clean_column"]])

results_2023 <- results_2023 %>%
  setNames(columns_2023[["clean_column"]])


