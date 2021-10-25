library('shiny')
library(deSolve)
ui <- fluidPage(
  sliderInput(inputId = "beta",
              label = "Choose an Infection Probability",
              value = 1.4247, min = 0.5, max = 1),
  sliderInput(inputId = "gamma",
              label = "Choose a Recovery Probability",
              value = 0.14286, min = 0, max = 0.5),
  numericInput(inputId = "finalt", label = "Stop Time",
               value = 70, min = 1, max = 360),
  numericInput(inputId = "infprop", label = "Initial Proportion Infected",
               value = 1e-6, min = 0, max = 0.3),
  plotOutput(outputId = "SIR")
)


server <- function(input, output) {
  sir <- function(time, state, parameters) {
    with(as.list(c(state, parameters)), {
      dS <- -beta * S * I
      dI <- beta * S * I - gamma * I
      dR <- gamma * I
      
      return(list(c(dS, dI, dR)))
    })
  }
  output$SIR <- renderPlot({
    init <- c(S = 1-input$infprop, I = input$infprop, 0.0)
    parameters <- c(beta = input$beta, gamma = input$gamma)
    times <- seq(0, input$finalt, by = 1)
    out <- as.data.frame(ode(y = init, times = times, func = sir, parms = parameters))
    out$time <- NULL
    
    matplot(times, out, type = "l", xlab = "Time", ylab = "S, I, R Proportions", main = "SIR Model", lwd = 1, lty = 1, bty = "l", col = 2:4)
    legend("topright", col=2:4, legend=c("S", "I", "R"), lwd=1)
  })
  
}

shinyApp(ui = ui, server = server)
