
# load required packages
library(tidyverse)
library(sjPlot)
library(likert)
library(xtable)
library(ggplot2)
library(readxl)
library(dplyr)

# import data
data1 <- read_excel("raw_data/AMR_KAP_DATA.xlsx")

# Figure 1. Distribution of knowledge of antibiotic resistance among parents of school-going children

fig1 <- data |> 
  dplyr::select(12:23) |> 
  mutate(across(everything(), as.factor))

plot_likert(fig1)

ggsave("fig1.png", dpi = 300)

#Figure 2. Attitude towards antibiotic resistance and the misuse of antibiotics among parents of school-going children

fig2 <- data |> 
  dplyr::select(24:33) |> 
  mutate(across(everything(), as.factor))

plot_likert(fig2)

ggsave("fig2.png", dpi = 300)


# Figure 3. Practices among parents of school-going children regarding antibiotic resistance

fig3 <- data |> 
  dplyr::select(34:39) |> 
  mutate(across(everything(), as.factor))

plot_likert(fig3)

ggsave("fig3.png", dpi = 300)



