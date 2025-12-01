
library(shiny)


ui <- fluidPage(
  titlePanel('Substance Use Versus Demographic Features'),
  
  tabsetPanel(
    tabPanel('Alcohol',
             
             ),
    tabPanel('Summary'
             
             )
  )
)

server <- function(input, output) {


}

shinyApp(ui = ui, server = server)
