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
   sidebarLayout(position = "right",
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


       actionButton("go", "Draw Sample")
     ),

      # Show a plot of the generated distribution
      mainPanel(
        p("The population distribution for the Schreiner heights example is the distribution of the heights of every single student. This is almost always
          unknown! However we can use statistics to make an inference about some characteristic of this distribution through random sampling. "),
        p("You can change the Population distribution button to change the shape of the population distribution. Our estimates should not depedent directly on
          the shape of the population distribution as this is almost always unknown!"),
        plotOutput("popdistPlot"),
        p("The sample size slider varies the number of random students we ask about their heights before quitting and computing our estimate for the population
          parameter. "),
        p("The point estimate buttons allow you to change the population parameter we are trying to measure. The mean button means we are trying to estimate
        the average height of Schreiner students through a sample, the median tries to estimate the median height of Schreiner students, ..., the IQR tries to estimate
          the Inner Quartile Range of the height distribution using the sample. "),
        p("Click the Draw Sample button to generate a random sample of N students. Notice each time you click this the estimate changes as the students randomly chosen
          for the survey changes. To understand this random variable we plot its probability distribution (the sampling distribution)"),
        plotOutput("samplingdistPlot"),
        p("The black vertical line shows our best estimate for the point estimator (mean, median, var, etc) and the blue line shows the actual value (if we sampled the entire population). "),
        p("Notice each time you hit the Draw Sample button the black line changes and the blue line is fixed. This is beacuse our sample is random and
          the population value is fixed. "),
        p(h3("Estimator Statistics:")),
        textOutput("point_estimate"),
        textOutput("sd_error")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  pop.data <- eventReactive(input$distType,
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


  estimate.data<- eventReactive(input$go,
                                {
                                  my.pop.data=sample(pop.data(), size=input$size)
                                  sample.size=1000;
                                  if (input$pestim==1) {
                                    return(replicate(sample.size, mean(sample(my.pop.data, size=input$size, replace=TRUE))))
                                  }
                                  if (input$pestim==2) {
                                    return(replicate(sample.size, median(sample(my.pop.data, size=input$size, replace=TRUE))))
                                  }
                                  if (input$pestim==3) {
                                    return(replicate(sample.size, var(sample(my.pop.data, size=input$size, replace=TRUE))))
                                  }
                                  if (input$pestim==4) {
                                    return(replicate(sample.size, sd(sample(my.pop.data, size=input$size, replace=TRUE))))
                                  }
                                  if (input$pestim==5) {
                                    return(replicate(sample.size, IQR(sample(my.pop.data, size=input$size, replace=TRUE))))
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

    hist(estimate.data(), col = 'coral', border = 'white', main='Sampling Distribution', xlab='Data Values', freq=FALSE);
    dens <- density(estimate.data());
    lines(dens, col='red');

    if (input$pestim==1) {
      line.place=mean(pop.data())
      sample.place=mean(estimate.data())
      abline(v=line.place, col='blue', lwd=3)
      abline(v=sample.place, col='black', lwd=3)
      val1=quantile(estimate.data(), 0.025)
      val2=quantile(estimate.data(), 0.975)
      abline(v=val1, col='red', lwd=3, lty=2)
      abline(v=val2, col='red', lwd=3, lty=2)
    }
    if (input$pestim==2) {
      line.place=median(pop.data())
      sample.place=mean(estimate.data())
      abline(v=line.place, col='blue', lwd=3)
      abline(v=sample.place, col='black', lwd=3)
      val1=quantile(estimate.data(), 0.025)
      val2=quantile(estimate.data(), 0.975)
      abline(v=val1, col='red', lwd=3, lty=2)
      abline(v=val2, col='red', lwd=3, lty=2)
    }

    if (input$pestim==3) {
      line.place=var(pop.data())
      sample.place=mean(estimate.data())
      abline(v=line.place, col='blue', lwd=3)
      abline(v=sample.place, col='black', lwd=3)
      val1=quantile(estimate.data(), 0.025)
      val2=quantile(estimate.data(), 0.975)
      abline(v=val1, col='red', lwd=3, lty=2)
      abline(v=val2, col='red', lwd=3, lty=2)
    }
    if (input$pestim==4) {
      line.place=sd(pop.data())
      sample.place=mean(estimate.data())
      abline(v=line.place, col='blue', lwd=3)
      abline(v=sample.place, col='black', lwd=3)
      val1=quantile(estimate.data(), 0.025)
      val2=quantile(estimate.data(), 0.975)
      abline(v=val1, col='red', lwd=3, lty=2)
      abline(v=val2, col='red', lwd=3, lty=2)
    }
    if (input$pestim==5) {
      line.place=IQR(pop.data())
      sample.place=mean(estimate.data())
      abline(v=line.place, col='blue', lwd=3)
      abline(v=sample.place, col='black', lwd=3)
      val1=quantile(estimate.data(), 0.025)
      val2=quantile(estimate.data(), 0.975)
      abline(v=val1, col='red', lwd=3, lty=2)
      abline(v=val2, col='red', lwd=3, lty=2)
    }
  })

  output$sd_error <- renderText({
    paste("Standard Error: ", sd(estimate.data()))
  })

  output$point_estimate <- renderText({
    paste("Point Estimate: ", mean(estimate.data()))
  })




}

# Run the application
shinyApp(ui = ui, server = server)

