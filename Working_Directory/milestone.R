library(shiny)
library(shinythemes)
library(readxl)
library(dplyr)
library(janitor)
library(tidyverse)
library(ggplot2)
library(hablar)

# Coding the app. Note that all data used here can be found in the gather.Rmd file

ui <- fluidPage(navbarPage("FORESTED AREAS", collapsible = TRUE, inverse = TRUE, theme = shinytheme("cyborg"),
                           
                           
#First tab called ABOUT. This page contains the project description and the
#source of the data used.
                           
                           tabPanel("ABOUT", 
                                    
h3("Project Description"),
                                    
h5("This project looks at data on forested areas, unemployment and
crime rates and communicable disease rates in more than 100 countries over
three decades 1990-2020"),

h5("The choice of countries and time period are determined by the availability of
data for the variables mentioned above. The overall goal is challenge existing 
claims regarding relationship between forested areas and each of the three other variables. 
The result of the study can influence the way we view our environment in general, 
and environmental protection and restoration in particular."),


h5("The datasets used in this study come from the Harvard Dataverse and Oregon
State Univeristy, linked through the following URLs (1)
https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/ZN1WLF;
(2)https//prism.oregonstate.edu/historical/ (3)
https://ourworldindata.org/natural-disasters"),

h5("My repository is found on this page: https://github.com/romblanco7/Shiny_App_Final_Project.git")),
                                    
# Tab 1 called Models. Contains plots.                                
                             
                              tabPanel(title = "Models", 
                                   
                                    #Sidebar layout
                                   
                                   sidebarLayout(
                                       
                                       #Sidebar Panel to select a country    
                                       
                                       sidebarPanel(
                                           selectInput("Countries_1", "Select country:", choices = For_v_Unemp$Country),
                                           selectInput("Countries_2", "Select country:", choices = For_v_Unemp$Country),
                                           selectInput("Countries_3", "Select country:", choices = For_v_Crime$Country)),
                                       
                                       #Main Panel that plot the location of the country selected  
                                       
                                       mainPanel("Plots",
                                                 
                                                 plotOutput("plot_1"),
                                                 plotOutput("plot_2"),
                                                 plotOutput("plot_3"),
                                                 plotOutput("plot_4")))),
                       
                              tabPanel(title = "Discussion")
))
                                                 
                                    
server <- function(input, output) {
    
    output$plot_1 <- renderPlot ({
            ggplot(data = For_v_Unemp[For_v_Unemp$Country == input$Countries_1,], 
                   mapping = aes(x = forested_area, y = Unemployed)) +
            geom_point(color = "green") + 
            geom_smooth(method = lm, color = "red") +
            theme_bw() +
             labs(title = "Forested Area vs Number of People Unemployed",
                  subtitle = "Number of people unemployed decreases as forested area increases",
                  x = "Forested Area (sq. km)",
                  y = "Disease rate")
        })
    
    
    output$plot_2 <- renderPlot ({
            ggplot(data = For_v_Crime[For_v_Crime$Country == input$Countries_2,], 
                   mapping = aes(x = forested_area, y = crime_rate)) +
            geom_point(color = "green") + 
            geom_smooth(method = lm, color = "red") +
            theme_bw() +
            labs(title = "Forested Area vs Crime Rates", 
                 subtitle = "Crime rate decreases as forested area increases",
                 x = "Forested Area (sq. km)",
                 y = "Crime rate")
    }) 
    
    output$plot_3 <- renderPlot ({
            ggplot(data = For_v_Dis[For_v_Dis$Country == input$Countries_3,], 
                   mapping = aes(x = forested_area, y = disease_rate)) +
            geom_point(color = "green") + 
            geom_smooth(method = lm, color = "red") +
            theme_bw() +
            labs(title = "Forested Area vs Communicable Disease Rate",
                 subtitle = "Disease rate decreases as forested area increases",
                 x = "Forested Area (sq. km)",
                 y = "Disease rate")
    })
    
    output$plot_4 <- renderPlot ({
        ggplot(data = Forest_v_disasters,
                   mapping = aes(x = World, y = Disaster_count)) +
        geom_point(color = "green") + 
            geom_smooth(method = lm, color = "red") +
        theme_bw() +
        labs(title = "Forested Area vs Disaster Count Globally",
             subtitle = "Disaster count decreases as forested area increases",
             x = "Forested Area (sq. km)",
             y = "Number of Natural Disasters")
    })
}

shinyApp(ui = ui, server = server)