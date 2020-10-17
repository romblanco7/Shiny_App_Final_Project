library(readxl)
library(dplyr)
library(janitor)
library(tidyverse)  

# loading datasets for the final project
  Location_data <- read_excel("Milestone_4/place.pop.precip.war.xlsx", sheet = c(3))
  Population_data <- read_excel("Milestone_4/place.pop.precip.war.xlsx", sheet = c(4))
  Precipitation_data <- read_excel("Milestone_4/place.pop.precip.war.xlsx", sheet = c(5))
  War_data <- read_excel("Milestone_4/place.pop.precip.war.xlsx", sheet = c(6))
  
  