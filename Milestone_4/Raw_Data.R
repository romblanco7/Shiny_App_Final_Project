library(readxl)
library(dplyr)
library(janitor)
library(tidyverse)  
Location_data <- read_excel("Milestone_4/place.pop.precip.war.xlsx", sheet = c(3))
  Population_data <- read_excel("Milestone_4/place.pop.precip.war.xlsx", sheet = c(4))
  Precipitation_data <- read_excel("Milestone_4/place.pop.precip.war.xlsx", sheet = c(5))
  War_data <- read_excel("Milestone_4/place.pop.precip.war.xlsx", sheet = c(6))
  
  
  Loc_data_clean <-  Location_data %>%
     clean_names() %>%
     select("primary", "population_data_name", "climate_data_name", "war_data_ccode", "war_data_state_name", "latitude", "longitude")
  
    
  
  
  navbarPage("Population, Precipitation and War", collapsible = TRUE, inverse = TRUE, theme = shinytheme("cerulean"),
             
             #First tab called ABOUT. This page contains the project description and the source of the data used.            
             tabPanel("ABOUT", 
                      h3("Project Description"),
                      h4("This project analyzes and compares data on the population, precipitation and war in 196
countries from 1800 to 2010. The overall aims of the project is to understand
the relationship between precipitation and population as well as precipitation
and war.  Ultimately, goal is to see whether or not there is a strong link
between precipitation the population as well as between precipitation and
conflicts. The result ofth is study can influence the way we view our
environment as a whole and major environmental issues such as global warming."),
                      
                      h4("The datasets used in this study come from the Harvard Dataverse, linked
through the following URL https://dataverse.harvard.edu/dataset.
xhtml?persistentId=doi:10.7910/DVN/ZN1WLF"),
                      
                      #Second lab called Location
                      tabPanel("Location"))) 