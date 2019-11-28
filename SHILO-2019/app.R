library(ggplot2)
library(dplyr)
library(shiny)
library(shinythemes)
library(ggthemes)

shilo <- readRDS("shilo2019.rds")
#shilo<-read.csv("SHILO-2019/data/shilo.csv")
shilo$row<-factor(shilo$row)
shilo$treatment<-factor(shilo$treatment)

shilo<-shilo%>%
  select(date,treatment,row,tissue,A,GS,E,DOY,osm,po,popd,posm)%>%
  filter(DOY!="127",tissue=="s")%>%
  filter(!DOY%in% c("203","210","224")|!treatment=="liz")
levels(shilo$treatment)->list_variables
shilo$treatment<-factor(shilo$treatment, levels =c("liz","A","E") ,labels = c("Super irrigation","Mild Irrigation","No irrigation"))


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
                       choices = c("Super irrigation","Mild Irrigation","No irrigation"), 
                       selected = "No irrigation"),
    dateRangeInput("date", 
              h3("Date input"), 
              start = min(shilo$date),end= max(shilo$date))
    ),
    # Output: Show scatterplot
    mainPanel(
      plotOutput(outputId = "plot2"))
  )
)

server <- shinyServer(function(input, output) {  
  
  #Function to subset df based on user selected states 
  
    color.groups <- c ('Super irrigation' = "#E41A1C",'Mild Irrigation' = '#377EB8', 'No irrigation' = '#4DAF4A')
    line.types <- c('Super irrigation' = 1, 'Mild Irrigation' = 4, 'No irrigation' = 5)
    
    # Create scatterplot object the plotOutput function is expecting
  output$plot2 <- renderPlot({
     sub <- shilo[shilo$treatment %in% input$treatment,]

     graph<-ggplot(data = sub, aes_string(x = input$x, y = input$y)) +
      geom_point(aes(color=treatment))+
      stat_summary( fun.y=mean, colour="black", geom="line",aes(linetype=treatment,group = treatment))+
              scale_color_manual(values = color.groups)+
        scale_linetype_manual(values = line.types)+
              theme_gdocs()
  
  
  if (input$y == "date") {
    graph + xlim( c(input$date[1], input$date[2]))
    graph
  }
  else {graph}
  },height = 400,width = 600)
})

shinyApp(ui = ui, server = server)
