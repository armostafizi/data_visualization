#### Animation/Interaction Self Directed Study

# For this assignment, I plan to study the house price distribution, varried by the year
# that the house was built. To do this, I'm going to use the house price dataset that I
# previously used for Assignment 3 and I possibly use it again for Assignment 4.

# The data and its description can be found here:
# https://www.kaggle.com/c/house-prices-advanced-regression-techniques

# Creating Plots --------
# This is one of the original plots that I had in Assignemnt 3 that I'm trying to improve
# The plot shows the relationship between total area, sale price, adn the overal quality

# ggplot(data) +
#   geom_point(aes(TotalArea, SalePrice, color = OverallQual), alpha = 0.7) +
#   scale_x_continuous(labels = comma) +
#   scale_y_continuous(labels = dollar) +
#   coord_cartesian(xlim = c(0, 7500), ylim = c(0, 500000)) +
#   scale_color_viridis() +
#   scale_fill_viridis() +
#   theme_minimal(18)


##### Shiny --------

install.packages("shiny")
library()

# Define UI for the House Price app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("House Price"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
      
      # Input: Overall Quality ----
      sliderInput("OverallQual", "Integer:",
                  min = min(data$OverallQual), max = max(data$OverallQual),
                  value = 1, animate = TRUE)
                  #animate = animationOptions(interval = 300, loop = TRUE))
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: The plot that shows the relationship between area and price
      #         for each overall quality level ----
      plotOutput("plot")
    )
  )
)


server <- function(input, output) {
  # Libraries ----------
  library(tidyverse)
  library(ggplot2)
  library(viridis)
  library(scales)
  
  # Data import -----------------------------
  # Downloaded date: 5/17/2018
  # Data Sescription,
  # https://www.kaggle.com/c/5407/download/data_description.txt
  data <- read_csv('train.csv')
  # Replace NA values with 0
  data <- data %>%
    replace(., is.na(.), 0)
  
  # Data Manipulation -------
  # Add Total Area as defined in Assignemnt 3
  data <- data %>%
    mutate(TotalArea = `1stFlrSF` + `2ndFlrSF` + `3SsnPorch` +
             TotalBsmtSF + EnclosedPorch +
             MasVnrArea + OpenPorchSF)
  # select the features of interest
  data <- data %>%
    select(TotalArea, SalePrice, OverallQual, BedroomAbvGr)
  
  
  output$plot <- renderPlot({
    filtered_data <- data %>%
                      filter(OverallQual == input$OverallQual)
    # draw the plot for the specified Overall Quality
    ggplot(filtered_data) +
      geom_point(aes(TotalArea, SalePrice, color = OverallQual), alpha = 0.7) +
      scale_x_continuous(labels = comma) +
      scale_y_continuous(labels = dollar) +
      coord_cartesian(xlim = c(0, 7500), ylim = c(0, 500000)) +
      scale_color_viridis() +
      scale_fill_viridis() +
      theme_minimal(18)
  })
  
}

shinyApp(ui = ui, server = server)