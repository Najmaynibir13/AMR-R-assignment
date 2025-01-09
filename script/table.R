install.packages("easystats")

# load packages
library(tidyverse)
library(gtsummary)
library(gt)
library(readxl)
library(dplyr)
library(easystats)

# load data as excel
data <- read_excel("clean_data/clean_data.xlsx")


# Table 1. Demographic characteristics of study participants
data |> 
  select(1 : 11) |> 
  tbl_summary(statistic = all_continuous() ~ "{mean} {sd}") |> 
  as_gt() |> 
  gtsave("table/Table1.docx")



# Table 2. Major sources of information about antibiotic parents
data |> 
  select(47 : 55) |> 
  tbl_summary(statistic = all_continuous() ~ "{mean} {sd}") |> 
  as_gt() |> 
  gtsave("table/Table2.docx")



# Table 3. Level of knowledge, attitudes, and practices towards antibiotic resistance among parents with school-going children
data |> 
  select(25, 37, 45) |> 
  tbl_summary(statistic = all_continuous() ~ "{mean} {sd}") |> 
  as_gt() |> 
  gtsave("table/Table3.docx")

# re-code knowledge level
data <- data |> 
  mutate(Knowledge_code = case_when(
    Knowledge_Score <= 50 ~ 0,
    Knowledge_Score >=51 ~ 1
  )) |> mutate(across(Knowledge_code, as.factor))

# re-code attitude level
data <- data |> 
  mutate(Attitude_code = case_when(
    Attitude_Score <= 50 ~ 0,
    Attitude_Score >=51 ~ 1
  ))|> mutate(across(Attitude_code, as.factor))

# re-code practice level
data <- data |> 
  mutate(Practice_code = case_when(
    Practice_Score <= 50 ~ 0,
    Practice_Score >=51 ~ 1
  ))|> mutate(across(Practice_code, as.factor))


# Table 4. Factors associated with the level of knowledge among parents of school-going children


# multivariate logistic regression
kn_mv_logreg <- glm(Knowledge_code ~ `Parent’s age (years)`+ `Parent’s sex` +
                      `Parent’s education level`+ `Employment status` + `Family type` +
                      `Your average household income per month (BDT)` + `Child’s sex`
                    + `Child’s age (years)` + `Number of children`,
                    data = data, family = "binomial")
report(kn_mv_logreg)
# multivariate logistic regression but in tabular form
kn_mv_logreg |> 
  tbl_regressi
on(exponentiate = TRUE) |>
  bold_p(t = 0.05) |> 
  as_gt() |> 
  gtsave("table/Table4.docx")


## Table 5. Factors associated with the level of attitudes towards antibiotic resistance among parents of schoolgoing children 

# multivariate logistic regression
at_mv_logreg <- glm(Attitude_code ~ `Parent’s age (years)`+ `Parent’s sex` +
                      `Parent’s education level`+ `Employment status` + `Family type` +
                      `Your average household income per month (BDT)` + `Child’s sex`
                    + `Child’s age (years)` + `Number of children`,
                    data = data, family = "binomial")
report(at_mv_logreg)

# multivariate logistic regression but in tabular form
at_mv_logreg |> 
  tbl_regression(exponentiate = TRUE) |>
  bold_p(t = 0.05) |> 
  as_gt() |> 
  gtsave("table/Table5.docx")



# Table 6. Factors associated with the level of practices regarding antibiotic resistance among parents of schoolgoing children

# multivariate logistic regression
pr_mv_logreg <- glm(Practice_code ~ `Parent’s age (years)`+ `Parent’s sex` +
                      `Parent’s education level`+ `Employment status` + `Family type` +
                      `Your average household income per month (BDT)` + `Child’s sex`
                    + `Child’s age (years)` + `Number of children` +
                      Knowledge_Level + Attitude_Level,
                    data = data, family = "binomial")
report(pr_mv_logreg)

# multivariate logistic regression but in tabular form
pr_mv_logreg |> 
  tbl_regression(exponentiate = TRUE) |>
  bold_p(t = 0.05) |> 
  as_gt() |> 
  gtsave("table/Table6.docx")




