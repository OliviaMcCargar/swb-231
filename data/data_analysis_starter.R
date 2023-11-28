library(stringr)
library(tidyr)
library(dplyr)
library(readxl)

setwd("~/Desktop/SWB/project_231")

survey2019 <- read_excel("2019 Members survey full data.xlsx")

survey2019 = as.data.frame(survey2019)

survey2019colnames <- read.csv("~/Desktop/SWB/project_231/2019 Column Rename.csv")

colnames(survey2019) = as.vector(as.matrix(survey2019colnames["clean_column"]))


#### Pivoting example, combining update preference columns into single column

updatePrefColumns = str_detect(colnames(survey2019), "^Q06")

coalescedUpdatePref = do.call(coalesce, survey2019[updatePrefColumns])

table(coalescedUpdatePref)


survey2023 <- read_excel("2023 Members survey full download.xlsx")

survey2023colnames <- read.csv("~/Desktop/SWB/project_231/2023 Column Rename.csv")

colnames(survey2023) = as.vector(as.matrix(survey2023colnames["clean_column"]))


#### Pivoting example, combining revenue columns into single column

revenueColumns = str_detect(colnames(survey2023), "^Q08")

coalescedRev = do.call(coalesce, survey2023[revenueColumns])

table(coalescedRev)













