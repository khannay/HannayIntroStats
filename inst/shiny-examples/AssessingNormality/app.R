#This is an app to demonstrate the accessing normality procedures used in MATH 2330 at Schreiner University

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Accessing Normality of Data"),
   
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         numericInput("size", "Sample size:", 50),
         
         radioButtons("distType", 
                            "Distribution Type", 
                            choices = list("Normal" = 1, 
                                           "Uniform" = 2, 
                                           "Beta" = 3,
                                            "Binomial"=4),
                            selected = 1),
         actionButton("go", "Go")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        p("This is some text explaining the concept"),
        
        p("1. Make a histogram of the data and look for a rough mound shape in the data"),
         plotOutput("distPlot"),
        p("2. Compute the inner-quartile range divided by the standard deviation this should be near 1.3 for a normal distribution
          "),
        textOutput("selected_var"),
        p(""),
        p("3. Make a Quantile-Quantile plot of the data to see if the quantiles of the data agree roughly with those of a normal distribution.
          the main question here is whether the data points shown as circles lie along the line. If they do then we say the distribution is roughly
          normally distributed. "),
        plotOutput("qqplot")
        
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  data <- eventReactive(input$go,
    {
    if (input$distType==1) {
      return(rnorm(input$size))
    }
    
    if (input$distType==2) {
      return(runif(input$size))
    }
    
    if (input$distType==3) {
      return(rbeta(input$size,2,5))
    }
    
    if (input$distType==4) {
      return(rbinom(input$size, size=100, prob=0.5))
    }
    
  }
  )
  
   
   output$distPlot <- renderPlot({
      # Make a histogram of the data
      hist(data(), col = 'lightblue', border = 'white', main='Histogram of the Data', xlab='Data Values', freq=FALSE);
     dens <- density(data());
     lines(dens, col='red');
   })
   
   output$selected_var <- renderText({ 
     paste("IQR/sd= ", IQR(data())/sd(data()))
   })
   
   output$qqplot <- renderPlot({
     # Make a qq norm plot of the data
     qqnorm(data());
     qqline(data(), col='red');
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

