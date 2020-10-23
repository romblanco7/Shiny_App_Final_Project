library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)

# Coding the app. Note that all data used here can be found in the gather.Rmd file

ui <- fluidPage(navbarPage("Population, Precipitation and Wars", collapsible = TRUE, inverse = TRUE, theme = shinytheme("cyborg"),
                           
                           
                           #First tab called ABOUT. This page contains the project description and the source of the data used. 
                           
                           tabPanel("ABOUT", 
                                    h3("Project Description"),
                                    
                                    h4("This project analyzes and compares data on the population, precipitation
and wars in 196 countries. The overall goal is to see whether or not
there is a strong link between precipitation and population as well as
precipitation and wars. The result of this study can influence the
way we view our environment as a whole and the way we see major environmental
issues such as global warming as contributors to population decline and world conflicts"),
                                    
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
                           
                           
                           #Second tab called LOCATIONS containing the
                           #latitudinal and longitudinal locations of 196
                           #countries
                           
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
                                            selectInput("Countries", "Select country:", choices = Loc_data_clean$primary)),
                                        
                                        #Main Panel that plot the location of the country selected  
                                        
                                        mainPanel("Country location",
                                                  plotOutput("plot")))),
                           
                    #Third tab called War. Allows one to check the location of wars that happened on certain years.
                           
                              tabPanel(title = "WAR", 
                                    h5("Each date on this page corresponds to a war/wars. On the right hand side is/are the country/ies where those wars took place"),
                                    
                                    h5("Moving forward, our goal is to couple these countries with their latitudinal and longitudinal location which can be plotted easily just like what we see on the previous page. Except in this case, each dot will represent a war. We will then plot alongside those war dots the precipitation data and population data. The population and precipitation dots will appear in different sizes depending on the size of the population and precipitation. By plotting these data in this manner, we will be able to see if they tend congregate for each selected year, in which case there is a possible link."),
                                    
                                    tableOutput(outputId = "table")),
                                    
                
                    #Additional tabs. To be developed. 
                    
                            tabPanel(title = "PRECIPITATION", 
                                    h4("To be developed")),
                            tabPanel(title = "POPULATION", 
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
    
    output$table <- renderTable(War_data_clean)
}

shinyApp(ui = ui, server = server)