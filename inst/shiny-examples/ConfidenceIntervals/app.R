require(shiny)


# Define UI for dataset viewer application

# Define UI for application
ui<- fluidPage(

  # Application title
  titlePanel("Interpreting Confidence Intervals"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(position = "right",
                sidebarPanel(
                    numericInput(inputId = "nsamp"
                                 ,label=strong("Sample Size")
                                 ,value=100
                                 ,min=5
                                 ,max=10000),


                    numericInput(inputId = "conf.level"
                                  ,label=strong("Confidence Level")
                                  ,value=95
                                  ,min=1
                                  ,max=99),

                    actionButton("go", "Run")
                ),

    mainPanel(
      p("In this app we demonstrate how confidence intervals should be interpreted through simulations. In this example we are using the sample mean
        to try and estimate the population mean. Each of the lines below shows the confidence interval formed from a random sample of the population. "),
      p("Notice that the confidence intervals formed each time are different are different! If we take two different samples from the same population the
        confidence interval formed will be different each time. This is because the confidence interval is formed using the data, and the data is a random
        sample!"),
      plotOutput('conf.plot'),
      p("1. The red lines show the confidence intervals which do not contain the actual population mean. How does the number of these misses change as you
        adjust the confidence level? Can you see a connection between the confidence level and the expected number of errors?"),
      p("2. How does changing the sample size effect the number of errors made (misses)?"),
      p(("3. How does the width (length) of the intervals change as the sample size increases?")),
      p("4. How does the width(length) of the confidence intervals change as the confidence level increases?")
    )
  )
)




#---------------------------------------------------------------------


# Define server logic required to summarize and view the selected dataset
server<- function(input, output) {

  estimate.data<- eventReactive(input$go, {
    return(matrix(rnorm(input$nsamp*100, mean=0.0, sd=1.0), nrow=100, byrow=TRUE))
  })

  output$conf.plot<-renderPlot({

    n=input$nsamp
    pop.mean=0.0
    pop.sd=1.0
    conf.lvl=input$conf.level/100

    plot(NULL
         ,xlim = c(pop.mean-pop.sd,pop.mean+pop.sd)
         ,ylim = c(0,100)
         ,yaxt = 'n'
         ,xlab = (conf.lvl)
         ,ylab = (n)
          ,main = "Confidence Intervals of 100 Samples"
    )

    abline(v = pop.mean, col = 'blue')
    mtext(expression(mu), cex = 2, at = pop.mean)

    num.errors=0;
    for (i in 1:100){
      x <- estimate.data()[i,]
      test <- t.test(x,conf.level=conf.lvl)
      interval <- test$conf.int

      if(pop.mean>interval[1] & pop.mean<interval[2]){
        lines(c(interval[1],interval[2]),c(i,i), lwd=2,col='black')
      }
      else{
        lines(c(interval[1],interval[2]),c(i,i), lwd=2,col='red' )
        num.errors=num.errors+1;
      }
    }

    mtext(paste("Number of Errors: ", num.errors), side=4)

  })


}




# Run the application
shinyApp(ui = ui, server = server)
