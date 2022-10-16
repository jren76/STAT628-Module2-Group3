library(shiny)
library(shinythemes)


ui = fluidPage(
  titlePanel("Body Fat Calculator"),
  theme = shinytheme("superhero"),
  sidebarPanel(
  numericInput("weight", "Weight (lbs) ", 150,min = 100, max = 275, step = 1),
  helpText("~Range between 100-400 lbs"),
  numericInput("abdomen", "Abdomen (cm)", 90,min = 65, max = 160, step = 5),
  helpText("~Range between 65-160 cm"),
  numericInput("wrist","Wrist (cm)", 18,min = 10,max=25,step=0.25),
  helpText("~Range between 10-25 cm")),
  mainPanel(
    h4("Your Estimated Body Fat is:"),
    verbatimTextOutput("bodyfat")
  )
)
BodyFat = function(weight,abdomen,wrist){
  return(-22.74303 - 0.08581*weight + 0.87580*abdomen - 1.31266*wrist)
}
server = function(input,output) {
  output$bodyfat = renderPrint({
    weight = input$weight
    abdomen = input$abdomen
    wrist = input$wrist
  if (weight < 100 | weight > 400 | abdomen < 65 | abdomen > 160 | wrist < 10 | wrist > 25){
    "Error: Values are out of range, please try again."
  } else {
   BodyFat(input$weight,input$abdomen,input$wrist)
  }
})
}

shinyApp(ui,server)
