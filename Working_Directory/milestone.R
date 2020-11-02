library(shiny)
library(shinythemes)
library(readxl)
library(dplyr)
library(janitor)
library(tidyverse)
library(ggplot2)
library(hablar)

merged <- read_rds("merged.rds")


# LOADING DATASETS

Location_data <- read_xlsx("Raw_Data/Country_locations.xlsx")
Population_data <- read_xls("Raw_Data/Population_2.xls")
War_data <-read_xlsx("Raw_Data/place.pop.precip.war.xlsx", sheet = c(6))
Precip_data <- read_excel("Raw_Data/world_precipitation.xls")


# Cleaning Location Data. 
# We use the clean_names() to remove the unnecessary
# spaces in the column names. Then, we select just the columns that we want
# which are primary, that is the name of the countries, latitude and longitude of each country.

Loc_data_clean <- Location_data %>%
    clean_names() %>%
    select("primary", "latitude", "longitude") %>%
rename(Country = primary, Latitude = latitude, Longitude = longitude)


# Cleaning war dataset. 
# First, we will select the columns that we need. Then we
#  will remove all the rows with missing data.

War_data_clean <- War_data %>%
    select(COUNTRY, StartYear1) %>% 
    drop_na() %>%
    
    
    slice(-c(1:204))%>% # Next thing we will do is, remove the first 204 columns because those data don't have counterparts in the parts in the Precipitation database
    
    mutate(Country = COUNTRY) %>% # After that, we will create a country called mutate to 1) Fix the name which
    # and 2) arrange the order of the column.
    select(-COUNTRY) %>%
    
    
    # We will change the name of the date into "Year" and war code into "War_code
    group_by(Country) %>%
    summarise_all(funs(toString(na.omit(.)))) 

# We will then combine the countries with the same identifiers (ie, same country name)
colnames(War_data_clean)[colnames(War_data_clean) == 'StartYear1'] <- 'Year'

# Cleaning the population data. 
# First we want to remove the years we don't need, given the precipitation only covers as far back as 1963. Then, we will transpose the columns to rows and vice- versa. 

Population_data_2 <- Population_data %>%
    select(-c("1960", "1961", "1962")) 

Pop_data_clean <- t(Population_data_2) 

# We want to change the firt column's name to Year
colnames(Pop_data_clean)[colnames(Pop_data_clean) == 'Country Name'] <- 'Year'


# Cleaning Precipitation dataset


precip_data_clean <- Precip_data %>%
    rename(Country = Year)

# MERGING DATASETS

# Merging War, Location and Population and Data

merged <- precip_data_clean %>%
    left_join(War_data_clean, by = "Country", suffix = c("precip.War_data_clean", "_")) %>%
    left_join(Loc_data_clean, by = "Country") %>%
    rename(War_Year = Year)


# Coding the app. Note that all data used here can be found in the gather.Rmd file

ui <- fluidPage(navbarPage("Population, Precipitation and Wars", collapsible = TRUE, inverse = TRUE, theme = shinytheme("cyborg"),
                           
                           
#First tab called ABOUT. This page contains the project description and the
#source of the data used.
                           
                           tabPanel("ABOUT", 
                                    
                                    source('about_text.R')),
                                    
#Second tab called LOCATIONS containing the latitudinal and longitudinal
#locations of 196 countries
                           
                           tabPanel(title= "LOCATIONS", 
                        
                    h5("Given that the borders of countries change over time
                    due to territorial expansion and contraction, we won't be
                    using the using country names to refer to specific
                    locations in this study. Instead, we will be using
                    standardized locations in the form of latitudes and
                    longitudes."),
                    
                    h5("We have cleaned our location dataset, leaving only the
                    columns that contain the names of the countries, their
                    latitude and their longitude. This page shows the
                    latitude and longitude of 196 countries. Select a country
                    using the sidebar panel to see its standardized location."),
                        
                                    #Sidebar layout
                                    
                                    sidebarLayout(
                                        
                                        #Sidebar Panel to select a country    
                                        
                                        sidebarPanel(
                                            selectInput("Countries", "Select country:", choices = Loc_data_clean$Country)),
                                        
                                        #Main Panel that plot the location of the country selected  
                                        
                                        mainPanel("Country location",
                                                  plotOutput("plot")))),
                           
                    #Third tab called War. Allows one to check the location of wars that happened on certain years.
                           
                              tabPanel(title = "WAR", 
                                   h5("Each country on this page corresponds to a war/wars. The column on the left tells us when those wars took place"),
                                  
                                    #Sidebar layout
                                   
                                   sidebarLayout(
                                       
                                       #Sidebar Panel to select a country    
                                       
                                       sidebarPanel(
                                           selectInput("Countries2", "Select a country:", choices = merged$Country)),
                                       
                                       #Main Panel that plot the location of the country selected  
                                       
                                       mainPanel("Wars",
                                                  textOutput("War_Years"),
                                                  plotOutput("Precip_linegraph")))),
                                    
                
                    #Additional tabs. To be developed. 
                    
                            tabPanel(title = "PRECIPITATION"),
                            tabPanel(title = "POPULATION", 
                                     h4("To be developed")), 
                            tabPanel(title = "DISCUSSION", 
                                    h4("To be developed"))
                           
                           
))


server <- function(input, output) {
    
    # Plot showing  the latitudinal and longitudinal location of 196 countries
    
    output$plot <- renderPlot({
        ggplot(data = Loc_data_clean[Loc_data_clean$Country == input$Countries,], 
               mapping = aes(x = Latitude, y = Longitude)) +
            geom_point(color="blue", size = 5) +
            theme_grey()
    })

    output$War_Years <- renderText({
        merged$War_Year[merged$Country == input$Countries2]
    })
    
    output$Precip_linegraph <- renderPlot({
        ggplot(data = merged[merged$Country == input$Countries2,]), 
               mapping
        
    })
        
}

shinyApp(ui = ui, server = server)