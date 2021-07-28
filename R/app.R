library(shiny)
library(shinyjs)
source("map.R")
# Define UI for app that draws a histogram ----



print("load data")
cov_data <- read.csv("RKI_COVID19.csv")
cov_data %>%
  group_by(IdLandkreis) %>%
  summarize(datapoints=sum(AnzahlTodesfall)) -> cov_data
cov_data
print("loaded data")
names(cov_data)[names(cov_data) == 'deaths'] <- 'datapoints'



ui <- fluidPage(
  useShinyjs(),
  # App title ----
  titlePanel("COVID Analysis"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      selectInput("var",
                  label = "Choose a variable to display",
                  choices = c("Map of deaths per district", "Percent Black")
                 ),
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)

    ),

      # Main panel for displaying outputs ----
      mainPanel(

        # Output: Histogram ----
        #plotOutput(outputId = "distPlot1"),
        #plotOutput(outputId = "distPlot2")
        conditionalPanel(
          condition = "input.var == 'Percent White'",
          plotOutput('distPlot2')
        ),
        conditionalPanel(
          condition = "input.var == 'Percent Black'",
          plotOutput('distPlot1')

      )
    )
  )
)

server <- function(input, output, session) {

  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot2 <- renderPlot({

    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")

  })

  output$distPlot3 <- renderPlot({

    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogramamamamamia of waiting times")

  })

  output$distPlot1 <- renderPlot({
    plot_map(cov_data, "krasser title", "krasse legende")
  })
  #observeEvent(input$var,{
  #  req(input$var)
  #  if(input$var == "Percent White"){
  #    withProgress(message = 'Calculation in progress', {
  #      print("load again")
  #      output$distPlot1 <- renderPlot({
  #        plot_map(cov_data, "krasserer title", "krasse legende")
  #      })
  #      hide("distPlot2")
  #      show("distPlot1")
  #      incProgress(1)
  #    })
  #  } else if (input$var == "Percent Black"){
  #    withProgress(message = 'Calculation in progress', {
  #      output$distPlot2 <- renderPlot({

  #        x    <- faithful$waiting
  #        bins <- seq(min(x), max(x), length.out = input$bins + 1)
#
#          hist(x, breaks = bins, col = "#75AADB", border = "white",
#               xlab = "Waiting time to next eruption (in mins)",
#               main = "Histogram of waiting times")
#
#        })
#        hide("distPlot1")
#        show("distPlot2")
#        incProgress(1)
#      })
#    }
#  })


}

shinyApp(ui = ui, server = server)
