library(shiny)

ui <- fluidPage(
  sliderInput(inputId = "num", 
              label = tags$em("Choose a number"), 
              value = 25, min = 1, max = 100),
  plotOutput("hist"),
  verbatimTextOutput("stats"),
  "Text",
  tags$h1(tags$strong("Heading 1 is nested")),
  tags$h2("Heading 2"),
  tags$h3("Heading 3"),
  tags$h4("Heading 4"),
  tags$h5("Heading 5"),
  tags$h6("Heading",tags$strong("6"), "is nested"),
  tags$em("This is text")
)

server <- function(input, output) {
  output$hist <- renderPlot({
    hist(rnorm(input$num))
  })
  output$stats <- renderPrint({
    summary(rnorm(input$num))
  })
}

shinyApp(ui = ui, server = server)