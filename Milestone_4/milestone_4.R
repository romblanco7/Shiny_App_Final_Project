library(shiny)
library(shinythemes)
library(readxl)
library(janitor)
library(tidyverse)

# Cleaning Location data. From this dataset, we just want columns containing the different names
# countries are referred to as well as their latitudinal and longitudinal
# locations.

 Loc_data_clean <-  Location_data %>%
    clean_names() %>%
    select("primary", "population_data_name", "climate_data_name", "war_data_ccode", "war_data_state_name", "latitude", "longitude") 

 
#Coding shiny app 

ui <- fluidPage(navbarPage("Population, Precipitation and Wars", collapsible = TRUE, inverse = TRUE, theme = shinytheme("cyborg"),
         
                                             
#First tab called ABOUT. This page contains the project description and the source of the data used. 

    tabPanel("ABOUT", 
          h3("Project Description"),
          
          h4("This project analyzes and compares data on the population, precipitation
and wars in 196 countries from 1800 to 2010. The goal is to see whether or not
there is a strong link between precipitation the population as well as
precipitation and world conflicts. The result of this study can influence the
way we view our environment as a whole and the way we see major environmental
issues such as global warming."),

         h4("In order to analyze and compare the three datasets mentioned
above (Precipitation, Population and Wars), we will need to group all datasets
by country names and dates. But before we can do that, we have to make the
names and dates uniformed. After grouping all the datasets by country and
year, we will then plot those data to see their latitudinal and longitudinal
locations. Once we have done these, we can use the sidebar panels to set some
parameters (ie. display datasets by spepific year and/or by country) to see the
relationship/s between the datasets better"),
         
          h4("The datasets used in this study come from the Harvard Dataverse and Oregon State Univeristy, linked
through the following URLs (1) https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/ZN1WLF; (2)https//prism.oregonstate.edu/historical/"),
          h5("Repository: https://github.com/romblanco7/Shiny_App_Final_Project.git")),
   
                                 
#Second tab called LOCATIONS containing the latitudinal and longitudinal locations of 196 countries

  tabPanel(title= "LOCATIONS", 
           h5("This page helps us determine the latitudinal and longitudinal locations of the 196 countries.
           These can be used in standardizing the locations of precipitation and wars which we will be looking at"),
           
#Sidebar layout

           sidebarLayout(
              
#Sidebar Panel to select a country    
              
                sidebarPanel(
               selectInput("Countries", "Select country:", choices = Loc_data_clean$primary)),
               
#Main Panel that plot the location of the country selected  

             mainPanel("Country location",
               plotOutput("plot")))),

#Additional tabs. To be developed.   

   tabPanel(title = "PRECIPITATION", 
            h4("To be developed")),
   tabPanel(title = "POPULATION", 
            h4("To be developed")),
   tabPanel(title = "WAR", 
            h4("To be developed")),
   tabPanel(title = "DISCUSSION", 
            h4("To be developed"))


))


server <- function(input, output) {

# Plot showing  the latitudinal and longitudinal location of 196 countries
   
   output$plot <- renderPlot({
   ggplot(data = Loc_data_clean[Loc_data_clean$primary == input$Countries,], mapping = aes(x = latitude, y = longitude)) +
          geom_point(color="blue", size = 5) +
          theme_grey()
         })
   

 }
  
shinyApp(ui = ui, server = server)