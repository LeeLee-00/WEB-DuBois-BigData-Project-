library(openxlsx)
library(tidyverse)
library(scales)

UrlToRead <- "https://www2.census.gov/programs-surveys/cps/tables/time-series/historical-income-families/f05.xlsx"
TestFrame <- read.xlsx(UrlToRead)
TestFrame <- data.frame(TestFrame)

#Removing Unnecessary Rows and Columns (Breaking down income for every 3 years from 2020-2005)
TestFrame <- TestFrame[c(-1:-274,-296:-472,-276:-278,-280:-281,-283:-285,-287:-288,-290:-291,-293:-295),c(-2:-4, -6)]

#Adding Column names
colnames(TestFrame) <- c("Year", "Mean Income(Current Dollars)")

#Converting Income Column from int to Money Data Type
TestFrame$`Mean Income(Current Dollars)` <- as.numeric(gsub("\\$",",", TestFrame$`Mean Income(Current Dollars)`))
TestFrame$`Mean Income(Current Dollars)` <- dollar(TestFrame$`Mean Income(Current Dollars)`)

#Reset Row Numbers
rownames(TestFrame) = NULL