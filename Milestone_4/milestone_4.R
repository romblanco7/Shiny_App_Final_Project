library(shiny)
library(shinythemes)
library(readxl)
library(janitor)

# Cleaning Location data 

 Loc_data_clean <-  Location_data %>%
    clean_names() %>%
    select("primary", "population_data_name", "climate_data_name", "war_data_ccode", "war_data_state_name", "latitude", "longitude") 


ui <- fluidPage(navbarPage("Population, Precipitation and War", collapsible = TRUE, inverse = TRUE, theme = shinytheme("cyborg"),
                           
#First tab called ABOUT. This page contains the project description and the source of the data used.            
    tabPanel("ABOUT", 
          h3("Project Description"),
          h4("This project analyzes and compares data on the population, precipitation
and war in 196 countries from 1800 to 2010. The goal is to see whether or not
there is a strong link between precipitation the population as well as 
precipitation and world conflicts. The result of this study can influence the way we
view our environment as a whole and the way we see major environmental issues such as global
warming."),
          h4("The datasets used in this study come from the Harvard Dataverse, linked
through the following URL https://dataverse.harvard.edu/dataset.
xhtml?persistentId=doi:10.7910/DVN/ZN1WLF")),
                                    
#Second tab called LOCATIONs containing the latitudimal and longitudinal locations of 196 countries
  tabPanel(title= "LOCATIONS", 
           h4("This page helps us determine the latitudinal and longitudinal locations of the 196 countries.
           These can be used in standardizing the locations of precipitation and wars which we will be looking at"),
           
             sidebarLayout(
             sidebarPanel(
               selectInput("Countries", "Select country:", choices = Loc_data_clean$primary)),
             mainPanel("Country location",
               plotOutput("plot")))),
#Additional tabs. To be developed.
   tabPanel(title= "POPULATION", h4("To be developed")),
   tabPanel(title= "WAR", h4("To be developed")),
   tabPanel(title= "PRECIPITATION", h4("To be developed")),
   tabPanel(title = "DISCUSSION", h4("To be developed"))

))


server <- function(input, output) {
  
  
# Plot showing  the latitudinal and longitudinal location of 196 countries
   output$plot <- renderPlot({
   ggplot(data = Loc_data_clean[Loc_data_clean$primary==input$Countries,], mapping = aes(x = latitude, y = longitude)) +
          geom_point(color="blue", size = 5) +
     theme_grey()
     
   })
   
   
   
 }
  



shinyApp(ui = ui, server = server)