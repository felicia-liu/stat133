# ===============================================
# Fill in the following fields
# ===============================================
# Title: Project 2: Investment Simulator Shiny App
# Description: Simulates behavior and returns from 
# investing in a total stock market index fund
# Author: Felicia Liu
# Date: 04/13/2021


# ===============================================
# Required packages
# ===============================================
library(rsconnect)
library(shiny)
library(dplyr)
library(ggplot2)
library(reshape2)
library(forestmangr)
library(matrixStats)

# ===============================================
# Define UserInterface "ui" for application
# ===============================================

ui <- fluidPage(
  
  titlePanel("Total stock market index fund investment simulator"),
  fluidRow(
    # Inputs for initial amount, and number of years
    column(3,
           numericInput(inputId = "initial",
                        label = "Initial investment amount ($):",
                        value = 1000),
           sliderInput(inputId = "years",
                       label = "Number of years:",
                       min = 0, 
                       max = 50,
                       value = 10)  
    ),
    
    # Inputs for periodic contributions
    column(3,
           numericInput(inputId = "contribution",
                        label = "Periodic contribution amount ($):",
                        value = 360), 
           radioButtons(inputId = "contributionchoice",
                        label = "Choice of periodic contribution:",
                        choices = list("End of each month" = 1,
                                       "End of each year" = 2),
                        selected = 2)
    ),
    
    # Inputs for Avg annual return, and avg annual volatility
    column(3,
           sliderInput(inputId = "return",
                       label = "Average annual return (%):",
                       min = 5, 
                       max = 25,
                       value = 10),
           sliderInput(inputId = "volatility",
                       label = "Average annual volatility (%):",
                       min = 5, 
                       max = 25,
                       value = 18) 
    ),
    
    # Inputs for number of simulations, and random seed
    column(3,
           sliderInput(inputId = "simulations",
                       label = "Number of simulations:",
                       min = 1, 
                       max = 100,
                       value = 50),
           numericInput(inputId = "seed",
                        label = "Random seed:",
                        value = 12345,
                        step = 1)
    )
  ),
  
  hr(),
  h4('Timeline of Balances'),
  plotOutput('plot'),
  
  fluidRow(
  hr(),
  h4('Summary Statistics for Simulated Results'),
  column(5, verbatimTextOutput('table')),
  column(5, verbatimTextOutput("table2")),
  column(5, verbatimTextOutput("table3"))
  )
)



# ===============================================
# Define Server "server" logic
# ===============================================

server <- function(input, output) {
  
  options(scipen = 999)
  
  # you may need to create reactive objects
  # (e.g. data frame to be used for graphing purposes)
  simdata <- reactive({
    # replace the code below with your code!!!
    set.seed(input$seed)
    avg_return <- input$return/100
    avg_volatility <- input$volatility/100
  
    if (input$contributionchoice == 1) {
      k <- 12
    }
    else {
      k <- 1
    }
    
    simdata <- data.frame(matrix(ncol = input$simulations + 1, 
                                 nrow = k*input$years + 1))
    rownames(simdata) <- c(0:(nrow(simdata) - 1))
    colnames(simdata) <- c("period", paste0("sim", 1:input$simulations))
    
    for (i in 2:(input$simulations + 1)){
      simdata[,1] <- c(0:(k*input$years))
      simdata[1,i] <- input$initial
      amount <- input$initial
      r <- rnorm(1, mean = avg_return, sd = avg_volatility)
      for (j in 2:(k*input$years + 1)){
        if (k == 1){ # when k = 1
          r <- rnorm(1, mean = avg_return, sd = avg_volatility)
          amount <- amount*(1 + r) + input$contribution
        } else { # when k = 12
          if ((j - 1) %% 12 == 0) {
            r <- rnorm(1, mean = avg_return, sd = avg_volatility)
            amount <- amount*(1 + r/k) + input$contribution
          } else {
            amount <- amount*(1 + r/k) + input$contribution
          }
        }
        simdata[j,i] <- amount
      }
    } 
    
    simdata
    
  })
  

  # code for graph
  output$plot <- renderPlot({
    # replace the code below with your code!!!
    data <- melt(simdata(), id = 1, varnames = c("period"), 
                 variable.name = "simulations")
    
    quantiles <- data %>% group_by(period) %>% 
      summarize(q10 = quantile(value, 0.1), q90 = quantile(value, 0.9))
    
    average <- data %>% group_by(period) %>% summarize(avg = mean(value))
    
    if (input$contributionchoice == 2){
      ggplot(data, aes(x = period, y = value)) +
      geom_line(aes(group = simulations), color = "lightgray") +
      geom_line(data = quantiles, aes(x = period, y = q10), 
                size = 1, color = "red") +
      geom_line(data = quantiles, aes(x = period, y = q90), 
                  size = 1, color = "red") +
      geom_line(data = average, aes(x = period, y = avg),
                size = 2, color = "black") +
      geom_abline(aes(intercept = input$initial, 
                      slope = input$contribution), 
                  linetype = "longdash", size = 1, color = "blue") +
      labs(title = "Contributions made at the end of each year",
           subtitle = paste(input$simulations, 
                            "Simulations for", input$years, "years"), 
           x = "Year", y = "Balance of Investment ($)",
           caption = "10th and 90th percentiles in red, average balance at each 
             year in black, base contribution without investment in dashed blue") +
      theme_minimal()
    } else {
      ggplot(data, aes(x = period, y = value)) +
        geom_line(aes(group = simulations), color = "lightgray") +
        geom_line(data = quantiles, aes(x = period, y = q10), 
                  size = 1, color = "red") +
        geom_line(data = quantiles, aes(x = period, y = q90), 
                  size = 1, color = "red") +
        geom_line(data = average, aes(x = period, y = avg),
                  size = 2, color = "black") +
        geom_abline(aes(intercept = input$initial, 
                        slope = input$contribution), 
                    linetype = "longdash", size = 1, color = "blue") +
        labs(title = "Contributions made at the end of each month",
             subtitle = paste(input$simulations, 
                              "Simulations for", input$years, "years"),
             x = "Month", y = "Balance of Investment ($)",
             caption = "10th and 90th percentiles in red, average balance at each 
             year in black, base contribution without investment in dashed blue") +
        theme_minimal()
    }
  })
  
  
  # code for statistics
  output$table <- renderPrint({
    # replace the code below with your code!!!
    end_amt <- tail(simdata(), 1) %>% select(-c(1))
    simmatrix <- data.matrix(end_amt, rownames.force = NA)
    row_quantiles <- rowQuantiles(simmatrix, probs = 
                                    seq(from = 0, to = 1, by = 0.1))
    quantile_df <- as.data.frame(as.table(row_quantiles)) 
    renamed_quantile_df <- rename(quantile_df, Quantile = Var1, EndAmount = Freq)
    renamed_quantile_df
  })
  
  
  output$table2 <- renderPrint({
    end_amt <- tail(simdata(), 1) %>% select(-c(1))
    simmatrix <- data.matrix(end_amt, rownames.force = NA)
    mean <- as.data.frame(mean(simmatrix))
    colnames(mean) <- c("Mean of End Amounts ($)")
    mean
  })
  
  output$table3 <- renderPrint({
    end_amt <- tail(simdata(), 1) %>% select(-c(1))
    simmatrix <- data.matrix(end_amt, rownames.force = NA)
    
    if (input$contributionchoice == 2) {
      no_invest <- input$initial + input$contribution*input$years
    } else {
      no_invest <- input$initial + input$contribution*input$years*12
    }
    
    counter <- 0
    for (n in simmatrix) {
      if (n >= no_invest) {
        counter = counter + 1
      } else {
        counter = counter
      }
    }
    counter
    
    counter_matrix <- data.matrix(counter, rownames.force = NA)
    counter_df <- as.data.frame(counter_matrix)
    colnames(counter_df) <- c("# Simulations w/ End Amount Greater Than Baseline Contributions")
    counter_df
  })
    
}



# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)

