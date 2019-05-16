# Libraries ----------
library(tidyverse)
library(ggplot2)
library(viridis)
library(scales)
library(shiny)

# The relationship between the price and the size for each overall quality level
# The data and its description can be found here:
# https://www.kaggle.com/c/house-prices-advanced-regression-techniques

# Data import -----------------------------
# Downloaded date: 5/17/2018
# Data Sescription,
# https://www.kaggle.com/c/5407/download/data_description.txt
data <- read_csv('train.csv')
# Replace NA values with 0
data <- data %>%
  replace(., is.na(.), 0)

x <- data %>%
  mutate(YearRemodAdd = ifelse(YearRemodAdd == 0, YearBuilt, YearRemodAdd))

# Data Manipulation -------
# Add Total Area as defined in Assignemnt 3
data <- data %>%
  mutate(TotalArea = `1stFlrSF` + `2ndFlrSF` + `3SsnPorch` +
           TotalBsmtSF + EnclosedPorch +
           MasVnrArea + OpenPorchSF)

# The correlation of numerical vars - To find the most influential independent variabes
numeric_data <- data %>%
                  select_if(is.numeric)
numeric_cor <- as.tibble(cor(numeric_data, method = "pearson"), rownames = NA) %>%
                  select(SalePrice)
# Looking at this, these are the most influential variables:
#     Overall Quality - Year Built - Year Remodeled - #Bathrooms - #Rooms - #Garage Cars - Total Area

# select the features of interest
data <- data %>%
  select(OverallQual, YearBuilt, YearRemodAdd, FullBath, TotalArea,
         TotRmsAbvGrd, GarageCars, TotalArea, SalePrice)

# UI
ui <- fluidPage(
  
  title = "House Price Predictor",
  
  # INPUTS
  titlePanel("House Price Predictor"),
  
  div(
    p("This visualization intends to predict a house price, based on the number of bedrooms, number of bathrooms,
       the capacity of the garage, Overall Quality of the house, and finally the total area in square feet of a house.
       In particular, filtered by the input variables, this visualization fit a linear model to the Sale Price of the house,
       based on its size. The left panel shows the histogram of the house price, depending on the input variables. And the right graph shows the
       relationship of the price and the size of the house (each dot is an observation), fitting a linear regression model (blue). The average house price
       and the predicted price are shown in red and black lines respectively, and the yellow line marks the 95% confidence interval.
       In addition, to capture any possible clusters on size and price, there is density plot in the background."),
    
    p("This work has been done using the",
      a(href = "https://ww2.amstat.org/publications/jse/v19n3/decock.pdf", "Ames Housing Dataset"),
      "that can be found",
      a(href = "https://www.kaggle.com/c/house-prices-advanced-regression-techniques/data", "here.")
    )
  ),
  
  fluidRow(
    column(4,
           # Input: Slider for the Number of Rooms ----
           sliderInput(inputId = "room",
                       label = "Number of Rooms:",
                       min = min(data$TotRmsAbvGrd),
                       max = max(data$TotRmsAbvGrd),
                       value = c(min(data$TotRmsAbvGrd), max(data$TotRmsAbvGrd)),
                       step = 1,
                       animate = FALSE),
           # Input: Slider for the Overall Quality ----
           sliderInput(inputId = "qual",
                       label = "Overall Quality:",
                       min = min(data$OverallQual),
                       max = max(data$OverallQual),
                       value = c(min(data$OverallQual), max(data$OverallQual)),
                       step = 1,
                       animate = FALSE)
    ),
    column(4,
           # Input: Slider for the Number of Bathrooms ----
           sliderInput(inputId = "bathroom",
                       label = "Number of Bathrooms:",
                       min = min(data$FullBath),
                       max = max(data$FullBath),
                       value = c(min(data$FullBath), max(data$FullBath)),
                       step = 1,
                       animate = FALSE),
           # Input: Slider for the Overal Quality ----
           sliderInput(inputId = "area",
                       label = "Total Area:",
                       min = 500,
                       max = 7500,
                       value = 2000,
                       step = 1000,
                       animate = FALSE)
    ),
    column(4,
           # Input: Slider for the Capacity of Garage ----
           sliderInput(inputId = "car",
                       label = "Garage Capacity:",
                       min = min(data$GarageCars),
                       max = max(data$GarageCars),
                       value = c(min(data$GarageCars), max(data$GarageCars)),
                       step = 1,
                       animate = FALSE)
    )
  ),
  
  hr(),
  # OUTPUTS: Plots ----
  fluidRow(
    column(width = 3, plotOutput(outputId = "priceDist")),
    column(width = 7, plotOutput(outputId = "totalArea")),
    column(width = 2, textOutput("test"))
  )
)



# Define server logic required to draw a histogram ----
server <- function(input, output){
  library(ggplot2)
  
  filtered_data <- reactive({
    d <- data %>%
      filter((TotRmsAbvGrd >= input$room[1]) & (TotRmsAbvGrd <= input$room[2]) &
             (FullBath >= input$bathroom[1]) & (FullBath <= input$bathroom[2]) &
             (GarageCars >= input$car[1]) & (GarageCars <= input$car[2]) &
             (OverallQual >= input$qual[1]) & (OverallQual <= input$qual[2]))
    validate(
      need(nrow(d) > 0, "Not enough data to make the plot! Choose another combination please ..")
    )
    d
  })
  
  linmod <- reactive({
    d <- data %>%
      filter((TotRmsAbvGrd >= input$room[1]) & (TotRmsAbvGrd <= input$room[2]) &
               (FullBath >= input$bathroom[1]) & (FullBath <= input$bathroom[2]) &
               (GarageCars >= input$car[1]) & (GarageCars <= input$car[2]) &
               (OverallQual >= input$qual[1]) & (OverallQual <= input$qual[2]) &
               (TotalArea >= 500) & (TotalArea <= 7500) &
               (SalePrice >= 0) & (SalePrice <= 500000))
    validate(
      need(nrow(d) > 0, "Not enough data to make the plot! Choose another combination please ..")
    )
    l <- lm(d$SalePrice ~ d$TotalArea)
    (summary(l)$coefficients[1] + (summary(l)$coefficients[2] * input$area))
  })
  
  ci <- reactive({
    d <- data %>%
      filter((TotRmsAbvGrd >= input$room[1]) & (TotRmsAbvGrd <= input$room[2]) &
               (FullBath >= input$bathroom[1]) & (FullBath <= input$bathroom[2]) &
               (GarageCars >= input$car[1]) & (GarageCars <= input$car[2]) &
               (OverallQual >= input$qual[1]) & (OverallQual <= input$qual[2]) &
               (TotalArea >= 500) & (TotalArea <= 7500) &
               (SalePrice >= 0) & (SalePrice <= 500000))
    validate(
      need(nrow(d) > 0, "Not enough data to make the plot! Choose another combination please ..")
    )
    sp <- d$SalePrice
    ta <- d$TotalArea
    l <- lm(sp ~ ta)
    newdata = data.frame(ta = input$area)
    predict(l, newdata, interval = "confidence")
  })
  
  output$totalArea <- renderPlot({
    ggplot(filtered_data(), aes(TotalArea, SalePrice)) +
      geom_density_2d() +
      stat_density_2d(aes(fill = ..level..), geom = "polygon") +
      geom_point(alpha = 0.3) +
      geom_smooth(method = "lm", se = TRUE, fullrange = TRUE) +
      geom_segment(aes(x = input$area, y = 0,
                       xend = input$area, yend = linmod()), color = "black") +
      geom_segment(aes(x = 500, y = linmod(),
                       xend = input$area, yend = linmod()), color = "black") +
      geom_hline(aes(yintercept = mean(filtered_data()$SalePrice)), size = 1, color = "red") +
      annotate("text", 1100, mean(filtered_data()$SalePrice), label = "Average Price", vjust = -0.5, size = 6, color = "red") +
      geom_hline(aes(yintercept = linmod()), size = 1, color = "black") +
      annotate("text", 6800, linmod(), label = "Predicted Price", vjust = -0.5, size = 6, color = "black") +
      geom_errorbar(aes(x = input$area, ymin = ci()[2], ymax=ci()[3]), color = "yellow") +
      scale_x_continuous(limits = c(500, 7500), labels = comma, expand = c(0, 0)) +
      scale_y_continuous(limits = c(0, 500000), labels = dollar, minor_breaks =  NULL) +
      scale_fill_viridis() +
      theme_minimal(18) +
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            legend.position = "none") +
      xlab("Total Area (sq ft)")
  })
  
  output$priceDist <- renderPlot({
    ggplot(filtered_data(), aes(SalePrice)) +
      geom_histogram() +
      scale_x_continuous(limits = c(0, 500000), labels = dollar, minor_breaks = NULL) +
      scale_y_continuous(breaks = trans_breaks(identity, identity, n = 3), minor_breaks = NULL) +
      coord_flip() +
      theme_minimal(18) +
      geom_vline(aes(xintercept = mean(filtered_data()$SalePrice)), size = 1, color = "red") +
      geom_vline(aes(xintercept = linmod()), size = 1, color = "black") +
      ylab("Count") +
      xlab("Sale Price")
  })
  
  #output$yearBuiltRemodeled <- renderPlot({
  #  ggplot(filtered_data(), aes(YearBuilt, YearRemodAdd, z = SalePrice)) +
  #    stat_summary_2d(bins = 4) +
  #    scale_fill_viridis() +
  #    theme_minimal(18)
  #})
  
  output$test <- renderText({ 
    paste("On average, a house with [", input$room[1], ",", input$room[2], "] rooms,",
                                      "[", input$bathroom[1], ",", input$bathroom[2], "] bathrooms,",
                                      "[", input$car[1], ",", input$car[2], "] parking spots in the garage,",
                                      "overall quality of [", input$qual[1], ",", input$qual[2], "],",
                                      "and size of", input$area, "square feet, with 95% confidence is between",
                                      round(ci()[2], digits = -2), "and", round(ci()[3], digits = -2),
                                      "dollars, with the average of", round(ci()[1], digits = -2), ".")
  })
  
}

shinyApp(ui = ui, server = server)