
library(shiny)
# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("Intereactive 2017 & 2025 Election Margin"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
      
      sidebarPanel(
        actionButton("reset","Reset Map Zoom")
        ),
      
        # Show a plot of the generated distribution
      mainPanel(
        fluidRow(
          column(6,
                 plotOutput(
                   "margin_map17",
                   height = 600,
                   hover = hoverOpts(id = "map_hover17"),
                   dblclick = "map_click17",
                   brush = brushOpts(id = "map_brush17", resetOnNew = TRUE)
                 )
          ),
          column(6,
                 plotOutput(
                   "margin_map25",
                   height = 600,
                   hover = hoverOpts(id = "map_hover25"),
                   dblclick = "map_click25",
                   brush = brushOpts(id = "map_brush25", resetOnNew = TRUE)
                 )
          )
        ),
        width = 10
      )
      
    )
)
