# ===============================================
# Fill in the following fields
# ===============================================
# Title: Text Analysis of R Package Names
# Description: Visualize the results of a basic 
# text analysis performed on the names and titles
# of R packages
# Author: Felicia Liu
# Date: 05/01/2021


# ===============================================
# Required packages
# ===============================================
library(dplyr)
library(ggplot2)
library(stringr)


# ===============================================
# Import data
# ===============================================
# import csv of package names and descriptions


# for demo purposes of the "template", we use data starwars
# (but you will have to replace this with the data in "rpackages.csv")
# dat <- dplyr::starwars

dat <- read.csv("rpackages.csv",
                stringsAsFactors = FALSE)
dat <- dat %>% mutate(Initial = str_sub(Package, 1, 1)) %>% 
    mutate(Length = str_count(Title, "[[:alnum:]]"))

# ===============================================
# Define UserInterface "ui" for application
# ===============================================

ui <- fluidPage(
    
    titlePanel("Text Analysis of R Package Names"),
    # Inputs for main filters
    column(3,
           p(em("Main filter")),
           radioButtons(inputId = "initial", 
                        label = "Initial letter of name", 
                        choices = c("lower case" = "ini_lower",
                                    "upper case" = "ini_upper",
                                    "both (lower & upper)" = "ini_both"), 
                        selected = "ini_both")
    ),
    
    # Inputs for more filters
    column(3,
           p(em("More filters")),
           selectInput(inputId = "numbers", 
                       label = "Name contains numbers",
                       choices = c("optional" = "num_opt",
                                   "only numbers" = "num_yes",
                                   "no numbers" = "num_no")),
           selectInput(inputId = "dot", 
                       label = "Name contains dot(s)",
                       choices = c("optional" = "dot_opt",
                                   "yes dot(s)" = "dot_yes",
                                   "no dot(s)" = "dot_no"))
    ),
    
    # Inputs for bar chart options 
    column(3,
           p(em("Barchart options")),
           radioButtons(inputId = "arrange", 
                        label = "Order bars by:", 
                        choices = c("decreasing freq" = "arr_dec",
                                    "increasing freq" = "arr_inc",
                                    "alphabetical a-z" = "arr_a2z",
                                    "alphabetical z-a" = "arr_z2a"),
                        selected = "arr_dec")
    ),
    
    # Additional input widgets for histogram options
    column(3,
           p(em("Histogram options")),
           sliderInput(inputId = "binwidth",
                       label = "Binwidth",
                       min = 1,
                       max = 20,
                       value = 1),
           checkboxInput(inputId = "facets",
                         label = strong("Facet by letter"),
                         value = FALSE)
    ),
    #),
    
    hr(),
    tabsetPanel(type = "tabs",
                tabPanel("Barchart",
                         h3("Number of packages by initial letter"),
                         plotOutput("barplot"),
                         
                         fluidRow(
                             hr(),
                             h3("Summary tables of frequencies by initial letter"),
                             column(5, verbatimTextOutput('bartable1')),
                             column(5, verbatimTextOutput('bartable2')))),
                tabPanel("Histogram", 
                         h3("Length of package names"),
                         plotOutput("histogram"),
                         
                         fluidRow(
                             hr(),
                             h3("Summary tables of lengths of package names"),
                             verbatimTextOutput('histtable1')))
                
    )
)


# ===============================================
# Define Server "server" logic
# ===============================================

server <- function(input, output) {
    
    # you may need to create reactive objects
    # data frame to be used in bar chart
    dat_bar <- reactive({
        # replace the code below with your code!!!
        
        if (input$initial == "ini_lower"){
            dat_bar <- dat %>% filter(Initial %in% letters) 
        } else if (input$initial == "ini_upper") {
            dat_bar <- dat %>% filter(Initial %in% LETTERS) 
        } else {
            dat_bar <- dat %>% mutate(Initial = toupper(Initial)) 
        }
        
        
        if (input$numbers == "num_yes"){
            dat_bar <- dat_bar %>% filter(str_detect(Package, "[[:digit:]]"))
        } else if (input$numbers == "num_no") {
            dat_bar <- dat_bar %>% filter(!str_detect(Package, '[[:digit:]]'))
        } else {
            dat_bar
        }
        
        
        if (input$dot == "dot_yes"){
            dat_bar <- dat_bar %>% filter(str_detect(Package, "\\."))
        } else if (input$dot == "dot_no") {
            dat_bar <- dat_bar %>% filter(!str_detect(Package, "\\."))
        } else {
            dat_bar
        }
        
        dat_bar <- dat_bar %>% group_by(Initial) %>% summarize(Count = n())
        
        dat_bar
        
    })
    
    # data frame to be used in histogram
    dat_hist <- reactive({
        # replace the code below with your code!!!
        
        if (input$initial == "ini_lower"){
            dat_hist <- dat %>% filter(Initial %in% letters) 
        } else if (input$initial == "ini_upper") {
            dat_hist <- dat %>% filter(Initial %in% LETTERS)  
        } else {
            dat_hist <- dat %>% mutate(Initial = toupper(Initial)) 
        }
        
        if (input$numbers == "num_yes"){
            dat_hist <- dat_hist %>% filter(str_detect(Package, "[[:digit:]]"))
        } else if (input$numbers == "num_no") {
            dat_hist <- dat_hist %>% filter(!str_detect(Package, '[[:digit:]]'))
        } else {
            dat_hist
        }
        
        if (input$dot == "dot_yes"){
            dat_hist <- dat_hist %>% filter(str_detect(Package, "\\."))
        } else if (input$dot == "dot_no") {
            dat_hist <- dat_hist %>% filter(!str_detect(Package, "\\."))
        } else {
            dat_hist
        }
        
        dat_hist  
        
    })
    
    
    # ===============================================
    # Outputs for the first TAB (i.e. barchart)
    # ===============================================
    
    # code for barplot
    output$barplot <- renderPlot({
        # replace the code below with your code!!!
        if (input$arrange == "arr_dec") {
            ggplot(data = dat_bar(), aes(x = reorder(Initial, -Count), y = Count)) +
                geom_col() + 
                geom_hline(yintercept = mean(dat_bar()$Count), color = "red") +
                labs(title = "Frequency in decreasing order",
                     x = "Initial letter",
                     y = "Frequency",
                     caption = "Red line shows average frequency,
             bars for letters with zero frequency are eliminated") +
                theme_minimal(base_size = 15)
            
        } else if (input$arrange == "arr_inc") {
            ggplot(data = dat_bar(), aes(x = reorder(Initial, Count), y = Count)) +
                geom_col() + 
                geom_hline(yintercept = mean(dat_bar()$Count), color = "red") +
                labs(title = "Frequency in increasing order",
                     x = "Initial letter",
                     y = "Frequency",
                     caption = "Red line shows average frequency,
             bars for letters with zero frequency are eliminated") +
                theme_minimal(base_size = 15)
            
        } else if (input$arrange == "arr_a2z") {
            ggplot(data = dat_bar(), aes(x = Initial, y = Count)) +
                geom_col() + 
                geom_hline(yintercept = mean(dat_bar()$Count), color = "red") +
                labs(title = "Frequency in alphabetical order",
                     x = "Initial letter",
                     y = "Frequency",
                     caption = "Red line shows average frequency,
             bars for letters with zero frequency are eliminated") +
                theme_minimal(base_size = 15)
            
        } else {
            ggplot(data = dat_bar(), aes(x = Initial, y = Count)) +
                geom_col() + 
                geom_hline(yintercept = mean(dat_bar()$Count), color = "red") +
                labs(title = "Frequency in reverse alphabetical order",
                     x = "Initial letter",
                     y = "Frequency",
                     caption = "Red line shows average frequency,
             bars for letters with zero frequency are eliminated") +
                scale_x_discrete(limits = rev) +
                theme_minimal(base_size = 15)
        }
        
        
    })
    
    # code for numeric summaries of frequencies
    output$bartable1 <- renderPrint({
        # replace the code below with your code!!!
        print("Frequencies by initial letter")
        print(dat_bar(), n = 26)
    })
    
    output$bartable2 <- renderPrint({
        # replace the code below with your code!!!
        print("Summary statistics of frequencies across letters")
        summary(dat_bar()$Count)
    })
    
    # ===============================================
    # Outputs for the second TAB (i.e. histogram)
    # ===============================================
    
    # code for histogram
    output$histogram <- renderPlot({
        # replace the code below with your code!!!
        if (input$facets == TRUE){
            dat_hist2 <- dat_hist() %>% group_by(Initial) %>% 
                summarize(mean = mean(Length))
            
            ggplot(data = dat_hist(), aes(x = Length)) +
                geom_histogram(col = "white", binwidth = input$binwidth) +
                labs(x = "Number of alphanumeric characters in package title",
                     y = "Number of packages",
                     caption = "Blue line(s) shows average number of alphanumeric characters in package title,
             histogram(s) for letters that don't appear are eliminated") +
                facet_wrap(~ Initial, scale = "free_y") +
                geom_vline(data = dat_hist2, mapping = aes(xintercept = mean), 
                           col = "blue") +
                theme_minimal(base_size = 15)
            
        } else {
            ggplot(data = dat_hist(), aes(x = Length)) +
                geom_histogram(col = "white", binwidth = input$binwidth) +
                geom_vline(xintercept = mean(dat_hist()$Length), col = "blue") +
                labs(x = "Number of alphanumeric characters in package title",
                     y = "Number of packages",
                     caption = "Blue line(s) shows average number of alphanumeric characters in package title") +
                theme_minimal(base_size = 15)
        }
    })
    
    # code for statistics
    
    output$histtable1 <- renderPrint({
        # replace the code below with your code!!!
        
        if (input$facets == TRUE){
            print("Statistical summary of lengths by initial letter")
            table <- dat_hist() %>% group_by(Initial) %>% 
                summarize(Min = min(Length),
                          FirstQuartile = quantile(Length, 0.25),
                          Median = median(Length),
                          Mean = mean(Length),
                          StdDev = sd(Length),
                          ThirdQuartile = quantile(Length, 0.75),
                          Max = max(Length))
            print(table, n = 26)
            
        } else {
            print("Summary statistics for lengths")
            summary(dat_hist()$Length) 
            
        }
        
    })
    
}



# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)

