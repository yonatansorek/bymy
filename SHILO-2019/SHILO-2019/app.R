library(ggplot2)
library(dplyr)
library(shiny)
library(shinythemes)
library(ggthemes)

shilo <- readRDS("shilo2019.rds")

shilo$row<-factor(shilo$row)
shilo$treatment<-factor(shilo$treatment)

shilo<-shilo%>%
  select(date,treatment,row,tissue,A,GS,E,DOY,osm,po,popd,posm)
levels(shilo$treatment)->list_variables


ui <- fluidPage(theme = shinytheme("cerulean"),
  titlePanel(title=h3("grape", align="center")),
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    # Inputs: Select variables to plot
    sidebarPanel(
      # Select variable for y-axis
      
      selectInput(inputId = "y", label = "Y-axis:",
                  choices = c("A", "E", "GS", "po", "osm"),
                  selected = "po"),
           # Select variable for x-axis
    selectInput(inputId = "x", label = "X-axis:",
                  choices = c("DOY", "date", "popd"),
                  selected = "DOY"),
     # Select variable for color
    checkboxGroupInput(inputId = "treatment", label = "Treatment:",
                       choices = list_variables, 
                       selected = list_variables[1]),
    dateRangeInput("date", 
              h3("Date input"), 
              start = "2019-05-01",end= "2019-10-01")
    ),
    # Output: Show scatterplot
    mainPanel(
      plotOutput(outputId = "plot2"))
  )
)

server <- shinyServer(function(input, output) {  
    # Create scatterplot object the plotOutput function is expecting
  output$plot2 <- renderPlot({
    ggplot(data = shilo, aes_string(x = input$x, y = input$y,group=input$treatment)) +
      geom_point()+
      geom_line()+
      theme_gdocs()
  },height = 400,width = 600)
})

shinyApp(ui = ui, server = server)
