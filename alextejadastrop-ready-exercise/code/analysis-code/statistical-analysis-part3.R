###############################
# analysis script
#
# this script loads the processed, cleaned data, does a simple analysis
# and saves the results to the results folder
###############################

# load needed packages. make sure they are installed.
library(ggplot2) # for plotting
library(broom)   # for cleaning up output from lm()
library(here)    # for data loading/saving
library(dplyr)   # for mutate/case_when

# path to data
data_location <- here::here("data", "processed-data", "processeddata2.rds")
processeddata2 <- readRDS(data_location)

######################################
# Data fitting/statistical analysis
######################################

############################
#### First model fit
# fit linear model using height as outcome, weight as predictor
lmfit1 <- lm(Height ~ Weight, data = mydata)
lmtable1 <- broom::tidy(lmfit1)
print(lmtable1)
table_file1 <- here("results", "tables", "resulttable1.rds")
saveRDS(lmtable1, file = table_file1)

############################
#### Second model fit
# fit linear model using height as outcome, weight and gender as predictor
lmfit2 <- lm(Height ~ Weight + Gender, data = mydata)
lmtable2 <- broom::tidy(lmfit2)
print(lmtable2)
table_file2 <- here("results", "tables", "resulttable2.rds")
saveRDS(lmtable2, file = table_file2)

############################
#### Create BMI and BMI_cat if missing
print("COLUMNS IN mydata:")
print(names(mydata))

if (!("BMI" %in% names(mydata))) {
  mydata <- mydata %>%
    mutate(BMI = Weight / ((Height / 100)^2))
}

if (!("BMI_cat" %in% names(mydata))) {
  mydata <- mydata %>%
    mutate(
      BMI_cat = case_when(
        BMI < 18.5 ~ "underweight",
        BMI < 25   ~ "healthy",
        BMI < 30   ~ "overweight",
        TRUE       ~ "obese"
      ) %>% as.factor()
    )
}

print("COLUMNS AVAILABLE FOR ANALYSIS:")
print(names(mydata))



############################
#### Third model fit
# Height as outcome, BMI and BMI_cat as predictors
lmfit3 <- lm(Height ~ BMI + BMI_cat, data = mydata)
lmtable3 <- broom::tidy(lmfit3)
print(lmtable3)
table_file3 <- here("results", "tables", "resulttable3.rds")
saveRDS(lmtable3, file = table_file3)

print("SAVED resulttable3.rds TO:")
print(table_file3)

