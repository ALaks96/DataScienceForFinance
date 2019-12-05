# Created by Alexis Laks

library(shiny)
library(dplyr)
library(plotly)
library(shinythemes)
#library(ndtv)
#library(networkD3)
library(feather)


data <- read_feather("~/Desktop/HEC Paris/Cours/DataScienceFinance/DataScienceForFinance/data/Data/filt_fund_2016.feather")
var_num <- names(data %>% 
                   select_if(is.numeric))

ui <- fluidPage(theme = shinytheme('sandstone'),
                title = "Data Science applied to finance EDA",
                
                ################################################## GRAPH VIEW ################################################### 
                navbarPage(tags$strong("Data Science applied to finance EDA",
                                       style = "font-size: 28px"),
                           
                           ################################################## HISTOGRAM VIEW ################################################### 
                           
                           tabPanel('Scatter plot',
                                    sidebarPanel(
                                      style = "font-size: 16px",
                                      selectInput('y',
                                                  "Choose your y variable",
                                                  choices = var_num),
                                      selectInput('col',
                                                  "Choose how you color the graphs",
                                                  choices = c("day", "month", "year"),
                                                  selected = 'year'),
                                      selectInput('facet_row',
                                                  'Create a graph for each caourse or sector inline',
                                                  c(None = '.', c("day","month","year")),
                                                  selected = "clarity"),
                                      selectInput('facet_col',
                                                  'Create a graph for each course or sector incolumn',
                                                  c(None = '.', c("day","month","year")),
                                                  selected = "clarity"),
                                      sliderInput('plotHeight',
                                                  'Adjust the height of the plot', 
                                                  min = 100,
                                                  max = 5000,
                                                  value = 830),
                                      sliderInput('plotWidth',
                                                  'Adjust the width of the plot',
                                                  min = 100,
                                                  max = 5000,
                                                  value = 1100)),
                                    mainPanel(plotlyOutput("scatter", height = "250%"))),
                                    
                                    
                                    tabPanel('Boxplot view',
                                             sidebarPanel(
                                               style = "font-size: 16px",
                                               selectInput('y2',
                                                           "Choose your y variable",
                                                           choices = var_num,
                                                           selected = "bm"),
                                               selectInput('col2',
                                                           "Choose how you color the graphs",
                                                           choices = c("day", "month", "year"),
                                                           selected = 'year'),
                                               selectInput('facet_row2',
                                                           'Create a graph for each caourse or sector inline',
                                                           c(None = '.', c("day","month","year")),
                                                           selected = "clarity"),
                                               selectInput('facet_col2',
                                                           'Create a graph for each course or sector incolumn',
                                                           c(None = '.', c("day","month","year")),
                                                           selected = "clarity"),
                                               sliderInput('plotHeight2',
                                                           'Adjust the height of the plot', 
                                                           min = 100,
                                                           max = 5000,
                                                           value = 830),
                                               sliderInput('plotWidth2',
                                                           'Adjust the width of the plot',
                                                           min = 100,
                                                           max = 5000,
                                                           value = 1100)),
                                             mainPanel(plotlyOutput("boxplot", height = "250%")))
                           
                           )
                
                           )





# Define server logic
server <- function(input, output) {
  
  ############################################# HISTOGRAM VIEW ###################################################
  
  output$scatter <- renderPlotly({
    p <- ggplot(data,
                aes_string(x = data$date,
                           y = input$y, 
                           fill = input$col)) +
      geom_point() +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    
    facets <- paste(input$facet_row, '~', input$facet_col)
    if (facets != '. ~ .') p <- p + facet_grid(facets)
    
    ggplotly(p,
             height = input$plotHeight,
             width = input$plotWidth,
             autosize = TRUE)
    
  })
  
  output$boxplot <- renderPlotly({
    p2 <- ggplot(data,
                aes_string(y = log(input$y2), 
                           fill = input$col2)) +
      geom_boxplot() +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    
    facets <- paste(input$facet_row2, '~', input$facet_col2)
    if (facets != '. ~ .') p <- p + facet_grid(facets)
    
    ggplotly(p2,
             height = input$plotHeight,
             width = input$plotWidth,
             autosize = TRUE)
    
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

