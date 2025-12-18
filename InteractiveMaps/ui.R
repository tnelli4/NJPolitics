
library(shiny)
# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("Intereactive 2017 Election Margin"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
      
      sidebarPanel(
        actionButton("reset","Reset Map Zoom")
        ),
      
        # Show a plot of the generated distribution
      mainPanel(
        plotOutput(
          "margin_map",
          height = 800,
          dblclick = "map_click",
          brush = brushOpts(id = "map_brush", resetOnNew = TRUE)
        ),
        textOutput("county_text")
      )
      
    )
)
