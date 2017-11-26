#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# required packages
library(shiny)
library(ggvis)
library(dplyr)
library(ggplot2)

# convert some variables as factors, for barcharts
raw$Grade <- as.factor(raw$Grade)

# Variable names for histograms
continuous <- c('HW1','HW2','HW3','HW4','HW5','HW6','HW6','HW7','HW8','HW9',
                'Att','QZ1','QZ2','QZ3','QZ4','EX1','EX2','Test1','Test2','Homework',
                'Quiz','Lab','Overall')


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Grade Visualizer"),
  
  # Sidebar with different widgets depending on the selected tab
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(condition = "input.tabselected==1",
                       h3("Grades Distribution"),
                       tableOutput("freq")),              
      conditionalPanel(condition = "input.tabselected==2",
                       selectInput("var1", "X-axis variable", continuous, 
                                   selected = "HW1"),
                       sliderInput("width", "Bin Width", 
                                   min = 1,max = 10, value = 10)),
      conditionalPanel(condition = "input.tabselected==3",
                       selectInput("var2", "X-axis variable",continuous, 
                                   selected = "Test1"),
                       selectInput("Y","Y-axis variable", continuous, 
                                   selected="Overall"),
                       sliderInput("Opa","Opacity",
                                   min= 0, max= 1, value = 0.5),
                       radioButtons("line", "Show line:",
                                    c("none" = "none",
                                      "lm" = "lm",
                                      "lowess" = "lowess"),
                                    selected = "none")
      )
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Barchart", value = 1, 
                           ggvisOutput("barchart")),
                  tabPanel("Histogram", value = 2, 
                           ggvisOutput("histogram"),
                           h3("Summary Statistics"),
                           verbatimTextOutput("summary")),
                  tabPanel("Scatterplot", value = 3, 
                           ggvisOutput("scatterplot"),
                           h3("Correlation"),
                           verbatimTextOutput("correlation")),
                  id = "tabselected")
    )
  )
)


# Define server logic
server <- function(input, output) {
  
  #Barchart (for 1st tab)
  vis_barchart <- reactive({
    raw %>% 
      ggvis(x = ~Grade, fill := "#75AADB") %>% 
      layer_bars(stroke := 'white',  fillOpacity := 0.8, fillOpacity.hover := 1) %>%
      add_axis("y", title = "frequency")
  })
  vis_barchart %>% bind_shiny("barchart")
  
  #Summary Frequency table (for 1st tab)
  frame <- table(raw$Grade)
  Grade <- row.names(frame)
  row.names(frame) <- NULL
  Freq <- frame
  frame <- cbind(Grade = Grade,Freq = Freq,Prop=round(frame/sum(frame),digits = 2))
  output$freq <- renderTable({
    frame
  })
  
  #Histogram (for 2nd tab)
  vis_histogram <- reactive({
    var1 <- prop("x", as.symbol(input$var1))
    raw %>% 
      ggvis(x = var1, fill := "#abafb5") %>% 
      layer_histograms(stroke := 'white', width = input$width,
                 fillOpacity := 1, fillOpacity.hover := 1) %>%
      add_axis("y", title = "count")
  })
  vis_histogram %>% bind_shiny("histogram")
  
  
  #Summary Stats for Historgram
  datasetInput <- reactive({
    switch(input$var1,
           "HW1" = raw$HW1,
           "HW2" = raw$HW2,
           'HW3' = raw$HW3,
           'HW4' = raw$HW4,
           'HW5' = raw$HW5,
           'HW6' = raw$HW6,
           'HW7' = raw$HW7,
           'HW8' = raw$HW8,
           'HW9' = raw$HW9,
           'Att' = raw$ATT,
           'QZ1' = raw$QZ1,
           'QZ2' = raw$QZ2,
           'QZ3' = raw$QZ3,
           'QZ4' = raw$QZ4,
           'EX1' = raw$EX1,
           'EX2' = raw$EX2,
           'Test1' = raw$Test1,
           'Test2' = raw$Test2,
           'Homework' = raw$Homework,
           'Quiz' = raw$Quiz,
           'Lab' = raw$Lab,
           'Overall' = raw$Overall
    )
  })
  output$summary <- renderPrint({
    print_stats(datasetInput())
  })
  
  #Scatterplot (for 3rd tab)
  datasetInputIV <- reactive({
    switch(input$line,
           "none" = "none",
           "lm" = "lm",
           "lowess" = "lowess")
  })
  vis_scatterplot <- reactive({
    var2 <- prop("x", as.symbol(input$var2))
    var3 <- prop('y', as.symbol(input$Y))
    if (datasetInputIV()=="none") {raw %>% 
      ggvis(x = var2, y = var3, fill := "black",opacity := input$Opa) %>%
      layer_points() }
    else if (datasetInputIV()=="lm") {
      raw %>% 
        ggvis(x = var2, y = var3, fill := "black",opacity := input$Opa) %>%
        layer_points() %>% layer_model_predictions(model = "lm")
    }
    else if (datasetInputIV()=="lowess") {
      raw %>% 
        ggvis(x = var2, y = var3, fill := "black",opacity := input$Opa) %>%
        layer_points() %>% layer_smooths()
    }
  })
  vis_scatterplot %>% bind_shiny("scatterplot")
  
  #Correlation (for 3rd tab)
  datasetInputII <- reactive({
    switch(input$var2,
           "HW1" = raw$HW1,
           "HW2" = raw$HW2,
           'HW3' = raw$HW3,
           'HW4' = raw$HW4,
           'HW5' = raw$HW5,
           'HW6' = raw$HW6,
           'HW7' = raw$HW7,
           'HW8' = raw$HW8,
           'HW9' = raw$HW9,
           'Att' = raw$ATT,
           'QZ1' = raw$QZ1,
           'QZ2' = raw$QZ2,
           'QZ3' = raw$QZ3,
           'QZ4' = raw$QZ4,
           'EX1' = raw$EX1,
           'EX2' = raw$EX2,
           'Test1' = raw$Test1,
           'Test2' = raw$Test2,
           'Homework' = raw$Homework,
           'Quiz' = raw$Quiz,
           'Lab' = raw$Lab,
           'Overall' = raw$Overall
    )
  })
  datasetInputIII <- reactive({
    switch(input$Y,
           "HW1" = raw$HW1,
           "HW2" = raw$HW2,
           'HW3' = raw$HW3,
           'HW4' = raw$HW4,
           'HW5' = raw$HW5,
           'HW6' = raw$HW6,
           'HW7' = raw$HW7,
           'HW8' = raw$HW8,
           'HW9' = raw$HW9,
           'Att' = raw$ATT,
           'QZ1' = raw$QZ1,
           'QZ2' = raw$QZ2,
           'QZ3' = raw$QZ3,
           'QZ4' = raw$QZ4,
           'EX1' = raw$EX1,
           'EX2' = raw$EX2,
           'Test1' = raw$Test1,
           'Test2' = raw$Test2,
           'Homework' = raw$Homework,
           'Quiz' = raw$Quiz,
           'Lab' = raw$Lab,
           'Overall' = raw$Overall
    )
  })
  output$correlation <- renderPrint({
    cat(cor(datasetInputII(),datasetInputIII()))
  })
}



# Run the application 
shinyApp(ui = ui, server = server)
