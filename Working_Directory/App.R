library(shiny)
library(shinythemes)
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)

# Adding some of my wrangling work here because my app wouldn't deploy
# properly if just put them in my gather.Rmd file. 
# All of these are also in my gather.Rmd file.

# LOADING DATASETs

forested_area_raw <- read.csv("Raw_Data/forest-area-km.csv")
urban_population_raw <- read_excel("Raw_Data/Urban_population.xls")


# CLEANING DATASETS

# Cleaning forested area dataset. We want to select the columns we want--they
# are Entity (Country), Year and forested area. Then we want to rename Entity;
# we will change it to Country so we can merge this dataset with the urban
# population dataset by Country.

forested_area <- forested_area_raw %>%
  select(Entity, Year, Forest.area) %>%
  rename(Country = Entity, forest_area = Forest.area)


# Cleaning urban population dataset. We want to subset our dataset, selecting
# only 1990 to 2019 because we want the time period to match that of the
# forested area dataset. We also change Country Name to Country to match the
# other dataset.

urban_population <- urban_population_raw %>%
  select(`Country Name`, c(`1990`:`2019`)) %>%
  rename(Country = `Country Name`) %>%

  # Finally, we will use pivot_longer() to consolidate all urban population data
  # into one column called urban population. After we do this, we should see each
  # country appearing multiple times under the Country column because there a
  # different value for every year from 1990 to 2019.

  pivot_longer(cols = `1990`:`2019`,
               names_to = "Year",
               values_to = "urban_pop") %>%
  mutate(Year = as.numeric(Year)) %>%  # We want to make sure Year is expressed as dbl so we can plot it later on.

  # We will then remove the rows we don't want. For some reason there are dataset
  # contains rows summarizing certain countries so we want to take those out and
  # just leave the individual countries. To do that, we will use pivot wider and
  # turn countries into columns, then use select() along with "-". I initially
  # tried filter() and "!" but it didn't work to remove the rows so now we will
  # remove them as columns.
  
  pivot_wider(names_from = Country, values_from = urban_pop) %>%
  select(-c(`Arab World`, `Central Europe and the Baltics`, `East Asia & Pacific`, `Europe & Central Asia (excluding high income)`, `East Asia & Pacific (excluding high income)`, `Early-demographic dividend`, `Europe & Central Asia`, `European Union`, `Fragile and conflict affected situations`, `Euro area`, `High income`, `Heavily indebted poor countries (HIPC)`, `IBRD only`, `IDA & IBRD total`, `IDA total`, `IDA blend`, `IDA only`, `Not classified` ,`Latin America & Caribbean (excluding high income)`, `Latin America & Caribbean`, `Least developed countries: UN classification`, `Low income`, `Middle East & North Africa`, `Lower middle income`, `Low & middle income`, `Late-demographic dividend`, `Middle income`, `Middle East & North Africa (excluding high income)`, `OECD members`, `Pre-demographic dividend`, `Post-demographic dividend`, `Sub-Saharan Africa (excluding high income)`, `East Asia & Pacific (IDA & IBRD countries)`,`Europe & Central Asia (IDA & IBRD countries)`, `Latin America & the Caribbean (IDA & IBRD countries)`, `Middle East & North Africa (IDA & IBRD countries)`, `South Asia (IDA & IBRD)`, `Sub-Saharan Africa (IDA & IBRD countries)`)) %>%

  # Then we can use pivot_longer again to return to the format we want:
  pivot_longer(cols = Aruba:Zimbabwe,
               names_to = "Country",
               values_to = "urban_pop")

# Below is a function we created to display linear equation and r-squared which
# appears on our plots as we will see in our shiny app.

lm_eqn <- function(df){
  m <- lm(df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","
                   ~~italic(r)^2~"="~r2,
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}


# Merging cleaned datasets for plotting data for each country. We want to join by Country and Year to 
# make you sure our data line up properly

pop_and_forest <- urban_population %>%
  left_join(forested_area, by = c("Country", "Year")) %>%
  drop_na()

# With out datasets merged, we can now plot urban population and forested area 
# (per country) together and see what their relationship is. 

# Note that even if we are creating a plot for each country,
# We are not filtering to a specific country because we want shiny app
# to do that. Our app has a feature that allows us to select a country to graph.


# Cleaning original datasets again so we can plot our global data (not per country 
# this time)
# Let's start with urban population. First, we want to take our urban
# population data and find the sum  of all countries. This will allow us to
# see how the urban population globally has changed overtime.

# Let's begin by pasting the codes we used earlier to clean our urban population dataset.

World_urban_pop <- urban_population_raw %>%
  select(`Country Name`, c(`1990`:`2019`)) %>%
  rename(Country = `Country Name`) %>%
  pivot_longer(cols = `1990`:`2019`,
               names_to = "Year",
               values_to = "urban_pop") %>%

# Since there is already a global summary for urban population in our raw
# dataset under a "World", we will just filter to that.

  filter(Country == "World") %>%

# We then select World summary and the year, the plot it ("Year" on the x axis
# and "urban_pop" on the y). But to plot the Year on the x-axis, we have to
# express "Year" as numeric.
  select(Year, urban_pop) %>%
  mutate(Year = as.numeric(Year))

# with our global urban population data clean, we can not plot it to see how it 
# has changed over time.


# Now we will do the same with global forested area.

World_forested_area <- forested_area %>%
  filter(Country == "World") %>%
  mutate(Year = as.numeric(Year)) %>%
  select(Year, forest_area)

# with our global forested area data clean, we can not plot it to see how it 
# has changed over time.


# Now, to see how global forested area and global urban population
# correlate, we need to plot them together. 
# We will begin by merging the two datasets we cleaned above.

World_pop_forest <- World_forested_area %>%
  left_join(World_urban_pop, by = "Year") %>%
  select(urban_pop, forest_area,  Year) %>%
  drop_na()

# Now we can plot urban population on the x-axis and forested area on the y-axis.


# Below are our model outputs. We are pasting them here because R wouldn't read 
# them straight from gather.Rmd. Codes for these models arefound in gather.Rmd.

Model_1 <- tibble(Predictors = c("Urban Population", "Year"),
                  Median = c("1.500000e-04", "-1.655202e+04"))

# I converted exported the output because it takes a while to load every time we
# run stan_glm()
model_output <- read_excel("Raw_Data/Model_output.xlsx")


# Coding the app. Note that all data used here can be found in the gather.Rmd file

ui <- fluidPage(navbarPage("URBAN POPULATION & FORESTED AREA", collapsible = TRUE, inverse = TRUE, theme = shinytheme("cyborg"),
                           
                           
# Tab 1: The ABOUT section. This page contains the project description, the aim
# of the project, the link to my github page and relevant sources.

tabPanel("ABOUT", 
                       

h3(tags$b("Project Description"), align = "center"), br(),
    
h5("In 2010, scientific journal Nature Geoscience released a study of the
relationship between forest area and urban population. They looked at data
from 41 countries in the humid tropic between 2000 and 2005 and used the linear
regression method of analysis to assess the correlation between the two sets of data. 
Their study revealed that forest loss is positively correlated with urban population
growth. They concluded that the strong trend in movement of people to cities in
the tropics is, counter-intuitively, likely to be associated with greater
pressures for clearing tropical forests"),

h5("This project aims to re-create and extend this study by looking at data on
forested areas and urban population in 227 countries and territories around, 
as opposed to 41 countries in the tropics, from 1990-2019, as oppposed to 2000-2005"),

h5("The aim of the project is two-fold: to test whether the negative
correlation still exist after 10 years and to see whether it is true for all countries.
Understanding these will give us an insight on how movement of people to cities affect 
forested areas around the world, which may give us an idea on how the problem of 
deforestaion could be addressed"), br(), br(),

h5("NOTE:"),
h6("The choice of countries and time period are determined by the availability
of the two types of data mentioned above."), br(),
h5("GITHUB REPO:"),
h6("hhttps://github.com/romblanco7/Shiny_App_Final_Project.git"), br(),
h5("SOURCES:"),
h6("Ritchie, Hannah, and Max Roser. “Forested Area.” 
Our World in Data, 3 June 2014, ourworldindata.org/natural-disasters."), 
h6(" World Bank. “Urban Population.” Data, data.worldbank.org/indicator/SP.URB.TOTL?view=chart."),
h6("Defries, Ruth & Rudel, Thomas & Uriarte, María & Hansen, Matthew. (2010). 
   Deforestation Drive by Urban Population Growth and Agricultural Trade in the 
   Twenty-First Century. Nature Geoscience - NAT GEOSCI. 3. 178-181. 10.1038/ngeo756.")),


                                    
# Tab 2: GLOBAL DATA. Contains plots showing how forested area and urban
# population have changed in the last three decades. There is a sub-panel which
# shows the model for the global data


                            tabPanel(title = "GLOBAL DATA", 
                                     
                              tabsetPanel(
                                tabPanel(title = "Global Data",
                                  h5("The last three decades have witnessed a steady decrease in global forested forested area
   and a steady increase in urban population"),
                                  
                                  # This is a scatter plot showing how global urban population has changed from 1990-2019.
                                  # The plot contains a regression model. 
                                  plotOutput("plot_1", width = "100%", height = "100%"),
                                  br(),
                                  
                                  # This is a scatter plot showing how global forested changed from 1990-2019.
                                  # The plot contains a regression model.
                                  plotOutput("plot_2", width = "100%", height = "100%"), 
                                  br()),
                        
                                tabPanel(title = "Model",
                                  h4("Correlation Plot"),
                                  
                                  # This plot shows the correlation between in urban population and forested area
                                  # as they changed over time.
                                  plotOutput("plot_3", width = "100%", height = "100%"), 
                                  br(),
                                  
                                  h6("Model output"),
                                  # This is a with table showing the output for my global data model. 
                                  # It shows two values: intercept and urban population.
                                  tableOutput("table"), 
                                  br(),
                                  
                                  h6("The table above shows two parameters: Urban Population and Year. The Urban Population	value of 1.500000e-04 tells us that for every one unit increase in urban population, global forested area increases by 1.500000e-04 sq. km.
                                    The Year value of -1.655202e+04 tells us that every year, global forested area decreases by 1.655202e+04 sq. km. 
                                    Over all, these values tell us that the decrease in global forested area from 1990 to 2019, which is what we are seeing in the graph above, cannot be attributed to the increase in globral urban population.
                                    This is different from what we expected: that is as urban population increases, forested area decreases. This is true for our global data, but is it true for each country?
                                    To find out, we applied the same model for each country. Check Data Per Country tab.")))),
                                 

# Tab 3:DATA PER COUNTRY. Contains plots showing how forested area and urban
# population has changed over time for each country. There is a sub-panel
# showing correlation between forested area and urban population for each
# country.

                            tabPanel(title = "DATA PER COUNTRY", 
                                   
                                    tabsetPanel(
                                      tabPanel(title = "Trend",
                                               
                                     sidebarLayout(
                                      
                                       sidebarPanel(
                                           h6("Choose a country to see how urban population and forest area has changed from 1990 to 2019. Plot may take time to load.", align = "center"),
                                           selectInput("Countries", "Select country:", choices =  pop_and_forest$Country)),
                                       
                                       # These plots show how urban population and forested area changed from 1990-2019
                                       # for each country.
                                       mainPanel("Change in urban population and forested area per country",
                                                 plotOutput("plot_4"),
                                                 plotOutput("plot_5")))),
                                               
                                    
                                   tabPanel(title = "Model",
                                        sidebarPanel(
                                          h6("Choose a country to see the relationship between  urban population and forested area. Plot may take time to load.", align = "center"),
                                            selectInput("Countries", "Select country:", choices =  pop_and_forest$Country)),
                                        mainPanel("Visual Representation of the Model", 
                                                  
                                                  # This is an interactive plot showing the correlation between urban population and
                                                  # forested area for each country.
                                          
                                           plotOutput("plot_6")), 
                                        h6("Notice that the relationship between the two variables is not the same for all countries.", align = "center"),
                                        h6("Some exhibit negative correlation while some exhibit positive correlation.", align = "center"),
                                        br(),
                                        
                                                  # This table contains values showing how the change in forested area for each country.
                                        
                                        h4("Model Output"),
                                        h6("*To calculate total change in forested per one unit"),
                                        h6("of increase in urban population in a particular country"),
                                        h6("take median value of the country that you want to look "),
                                        h6("at then add it the urban population value. See last row of the table"),
                                        br(),
                                        h6("Positive median value means that forested area increases as urban"),
                                        h6("population increases and and negative median value means that"),
                                        h6("forested area decreases as urban population increases"),
                                        h6("Notice that not all countries have a negative median value."),
                                          tableOutput("table_2")))),
                                            
                              tabPanel(title = "DISCUSSION",

h3(tags$b("GLOBAL DATA"), align = "center"), 
br(),
h4("Between 1990 and 2019, global urban population witnessed a steady increase while forested area witnessed a steady decrease.
Urban population during this period can be modeled by Y = 1959 + (1.4e-08)X. Forested area can be modeled by Y = 2717-.00017X.
Urban population and forested area seem to exhibit a strong negative correlation. It is also worth mentioning that these equations have imitations. 
We cannot plug in negative values for urban population and we cannot plug in a year before 1990."),
br(),

h4("Using stan_glm to see how change in urban population affects forested area, we discovered that every year, forested area decreases by 	-1.655202e+04, confirming the negative  trend we saw earlier. 
However, this doesn't seem to be a result of the change in urban population because our median is a positive value instead of of negative. It is 1.500000e-04, which means that for every one unit increase in urban population, forested area increases by 1.500000e-04. 
It is possible that our model is not showing the relationship we expected to see because we are looking at different countries which greatly differ in terms of urban population size and forested area. The data we are using are not standardized. 
It is better, therefore, to look at the countries individually."), 
br(),

h3(tags$b("DATA PER COUNTRY"), align = "center"), 
br(),

h4("Looking at each country, we notice varrying trends in terms of 
urban population and forested area; some exhibit a positive trend and others negative. 
We also noticed that  in some countries, change in urban population affects forested area as indicated by negative median values, while in others, change urban population does not affect forested area as indicated by positive median values"))
            
))
                                                 
                                    
server <- function(input, output, session) {
  
     output$plot_1 <- renderPlot({
        ggplot(data = World_urban_pop,
               mapping = aes(x = Year, y = urban_pop)) +
         geom_point(color = "mediumaquamarine", size = 4) +
         geom_smooth(method = "lm", formula = y~x, se = FALSE, color = "cyan3") +
         labs(title = "Change in urban population globally (1990-2019)", 
              subtitle = "Urban population globally increased between 1990 and 2019",
              x = "Year",
              y = "Urban Population",
              caption = "Source: The World Bank | Data \nOur World in Data") +
         theme_minimal() +
         scale_y_continuous(labels = scales::comma) +
         geom_text(data =World_urban_pop, aes(x = 2000, y = 4000000000, label = lm_eqn(World_urban_pop)), parse = TRUE)
         }, height = 400, width = 700 )
    
    output$plot_2 <- renderPlot({
        ggplot(data = World_forested_area,
               mapping = aes(x = Year, y = forest_area)) +
        geom_point(color = "palegreen4", size = 4) +
        geom_smooth(formula = y~x, method = "lm", se = FALSE, color = "chartreuse3") +
        labs(title = "Forested area globally (1990-2019)", 
             subtitle = "forest are globally decreased between 1990 and 2019",
             x = "Year",
             y = "Forested Area (sq.)",
             caption = "Source: The World Bank | Data \nOur World in Data") +
        theme_minimal() +
        scale_y_continuous(labels = scales::comma) +
        geom_text(aes(x = 2000, y = 4100000, label = lm_eqn(World_forested_area)), parse = TRUE)
        }, height = 400, width = 700 )
    
    
    output$plot_3 <- renderPlot ({
      ggplot(data = World_pop_forest, 
             mapping = aes(x = urban_pop, y = forest_area)) +
        geom_point(color = "orange", size = 4) +
        geom_smooth(formula = y~x, method = "lm", se = FALSE, color = "chartreuse3") +
        labs(title = "Correlation Between Forested Area and Urban Population", 
             subtitle = "There is a negative correlation between Forested Area and Urban Population (1990-2019)",
             x = "Urban Population",
             y = "Forested Area (sq. miles)",
             caption = "Source: The World Bank | Data \nOur World in Data") +
        theme_minimal() +
        scale_x_continuous(labels = scales::comma) +
        scale_y_continuous(labels = scales::comma)
        }, height = 400, width = 700 )
    
    output$table <- renderTable(Model_1)
    
  output$plot_4 <- renderPlot ({
  ggplot(data = pop_and_forest[pop_and_forest$Country == input$Countries,],
         mapping = aes(x = Year, y = forest_area)) +
    geom_point(size = 2, color = "palegreen4") +
    geom_smooth(formula = y~x, method = "lm", se = FALSE, color = "chartreuse3") +
    labs(title = "Forested area in Selected Country (1990-2019)",
         x = "Year",
         y = "Forested Area (sq. km)",
         caption = "Source: The World Bank | Data") +
    theme_minimal() +
    scale_y_continuous(labels = scales::comma)
    }, height = 300, width = 500)

  output$plot_5 <- renderPlot ({
   ggplot(data = pop_and_forest[pop_and_forest$Country == input$Countries,],
          mapping = aes(x = Year, y = urban_pop)) +
    geom_point(size = 2, color = "mediumaquamarine") +
    geom_smooth(formula = y~x, method = "lm", se = FALSE, color = "cyan3") +
    labs(title = "Urban population in Selected Country (1990-2019)",
         x = "Year",
         y = "Urban Population",
         caption = "Source: The World Bank | Data; \nOur World in Data") +
    theme_minimal() +
    scale_y_continuous(labels = scales::comma)
      }, height = 300, width = 500 )

  output$plot_6 <- renderPlot ({
    ggplot(data = pop_and_forest[pop_and_forest$Country == input$Countries,],
           mapping = aes(x = urban_pop, y = forest_area)) +
      geom_point(size = 3, color = "orange") +
      geom_smooth(se = FALSE, method = "lm", formula =  y ~ x, color = "chartreuse3") +
      labs(title = "Correlation Between Forested Area and Urban Population",
           x = "Urban Population",
           y = "Forested Area (sq. miles)",
           caption = "Source: The World Bank | Data \nOur World in Data") +
      theme_minimal() +
      scale_x_continuous(labels = scales::comma) +
      scale_y_continuous(labels = scales::comma)
      }, height = 300, width = 500 )

     output$table_2 <- renderTable(model_output, digits = 6)

}

shinyApp(ui = ui, server = server)