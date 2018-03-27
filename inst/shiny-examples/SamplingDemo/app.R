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
       sliderInput("size", "Sample size:", min=5, max=100, value=30, step=10),

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
                                   "sd"=4,
                                   "IQR"=5),
                    selected = 1),


       actionButton("go", "Go")
     ),

      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("popdistPlot"),
        plotOutput("samplingdistPlot")
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
    hist(pop.data(), col = 'lightblue', border = 'white', main='Population Distribution', xlab='Data Values', freq=FALSE);
    dens <- density(pop.data());
    lines(dens, col='blue');


  })

  output$samplingdistPlot <- renderPlot({
    # Make a histogram of the data
    my.pop.data=sample(pop.data(), size=input$size)
    if (input$pestim==1) {
      estimate.data=replicate(10000, mean(sample(my.pop.data, size=input$size, replace=TRUE)))
    }
    if (input$pestim==2) {
      estimate.data=replicate(10000, median(sample(my.pop.data, size=input$size, replace=TRUE)))
    }
    if (input$pestim==3) {
      estimate.data=replicate(10000, var(sample(my.pop.data, size=input$size, replace=TRUE)))
    }
    if (input$pestim==4) {
      estimate.data=replicate(10000, sd(sample(my.pop.data, size=input$size, replace=TRUE)))
    }
    if (input$pestim==5) {
      estimate.data=replicate(10000, IQR(sample(my.pop.data, size=input$size, replace=TRUE)))
    }

    hist(estimate.data, col = 'coral', border = 'white', main='Sampling Distribution', xlab='Data Values', freq=FALSE);
    dens <- density(estimate.data);
    lines(dens, col='red');

    if (input$pestim==1) {
      line.place=mean(pop.data())
      sample.place=mean(estimate.data)
      abline(v=line.place, col='blue', lwd=3)
      abline(v=sample.place, col='black', lwd=3)
      val1=quantile(estimate.data, 0.025)
      val2=quantile(estimate.data, 0.975)
      abline(v=val1, col='red', lwd=3, lty=2)
      abline(v=val2, col='red', lwd=3, lty=2)
    }
    if (input$pestim==2) {
      line.place=median(pop.data())
      sample.place=mean(estimate.data)
      abline(v=line.place, col='blue', lwd=3)
      abline(v=sample.place, col='black', lwd=3)
      val1=quantile(estimate.data, 0.025)
      val2=quantile(estimate.data, 0.975)
      abline(v=val1, col='red', lwd=3, lty=2)
      abline(v=val2, col='red', lwd=3, lty=2)
    }

    if (input$pestim==3) {
      line.place=var(pop.data())
      sample.place=mean(estimate.data)
      abline(v=line.place, col='blue', lwd=3)
      abline(v=sample.place, col='black', lwd=3)
      val1=quantile(estimate.data, 0.025)
      val2=quantile(estimate.data, 0.975)
      abline(v=val1, col='red', lwd=3, lty=2)
      abline(v=val2, col='red', lwd=3, lty=2)
    }
    if (input$pestim==4) {
      line.place=sd(pop.data())
      sample.place=mean(estimate.data)
      abline(v=line.place, col='blue', lwd=3)
      abline(v=sample.place, col='black', lwd=3)
      val1=quantile(estimate.data, 0.025)
      val2=quantile(estimate.data, 0.975)
      abline(v=val1, col='red', lwd=3, lty=2)
      abline(v=val2, col='red', lwd=3, lty=2)
    }
    if (input$pestim==5) {
      line.place=IQR(pop.data())
      sample.place=mean(estimate.data)
      abline(v=line.place, col='blue', lwd=3)
      abline(v=sample.place, col='black', lwd=3)
      val1=quantile(estimate.data, 0.025)
      val2=quantile(estimate.data, 0.975)
      abline(v=val1, col='red', lwd=3, lty=2)
      abline(v=val2, col='red', lwd=3, lty=2)
    }
  })




}

# Run the application
shinyApp(ui = ui, server = server)

