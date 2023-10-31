# SWB 231 H2H
# Data Transform & Load Procedure

library(readxl)
library(tidyverse)

pth <- file.path(getwd(), "data", "2019 Members survey full data.xlsx")
results_2019 <- read_excel(pth)

pth <- file.path(getwd(), "data", "2023 Members survey full download.csv")
results_2023 <- read.csv(pth)

results_2023 %>% colnames()
results_2019 %>% colnames()
