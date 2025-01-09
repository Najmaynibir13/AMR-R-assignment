#install packages
install.packages(c("ggthemes", "hrbrthemes", "ggsci", "ggpubr", "RcolorBrewer"))

# load packages
library(tidyverse)
library(ggthemes)
library(ggsci)
library(ggpubr)
library(RColorBrewer)
library(sjPlot)
library(likert)
# data import
data <- read_excel("raw_data/AMR_KAP_Data.xlsx")

# Check data structure
glimpse(data)

#figure 1
knowledge_distribution <- data |> 
  select(12:23) |> 
  mutate(across(1:12, as.factor))

 knowledge_distribution |> 
  select(1:12)
plot_likert(knowledge_distribution) 

