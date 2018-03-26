#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

   # Application title
   titlePanel("Sampling Distribution"),

   # Sidebar with a slider input for number of bins
   sidebarLayout(
     sidebarPanel(
       numericInput("size", "Sample size:", 50),

       radioButtons("distType",
                    "Population Distribution Type",
                    choices = list("Normal" = 1,
                                   "Uniform" = 2,
                                   "Beta" = 3,
                                   "Binomial"=4),
                    selected = 1),

       radioButtons("pestim",
                    "Point Estimator",
                    choices = list("mean" = 1,
                                   "median" = 2,
                                   "variance" = 3,
                                   "IQR"=4),
                    selected = 1),


       actionButton("go", "Go")
     ),

      # Show a plot of the generated distribution
      mainPanel(
        p("The Population Distribution"),
        plotOutput("popdistPlot"),
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  pop.data <- eventReactive(input$go,
                        {
                          if (input$distType==1) {
                            return(rnorm(10000))
                          }

                          if (input$distType==2) {
                            return(runif(10000))
                          }

                          if (input$distType==3) {
                            return(rbeta(10000,2,5))
                          }

                          if (input$distType==4) {
                            return(rbinom(input$size, size=10000, prob=0.5))
                          }

                        }
  )

  output$popdistPlot <- renderPlot({
    # Make a histogram of the data
    hist(pop.data(), col = 'lightblue', border = 'white', main='Histogram of the Data', xlab='Data Values', freq=FALSE);
    dens <- density(pop.data());
    lines(dens, col='red');
  })


}

# Run the application
shinyApp(ui = ui, server = server)

