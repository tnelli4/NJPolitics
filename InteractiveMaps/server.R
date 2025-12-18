

library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {
  
  zoomValues <- reactiveValues(
    xmin = NULL,
    xmax = NULL,
    ymin = NULL,
    ymax = NULL
  )
  
  observeEvent(input$map_brush,{
    print(input$map_brush)
    zoomValues$xmin <- input$map_brush$xmin
    zoomValues$xmax <- input$map_brush$xmax
    zoomValues$ymin <- input$map_brush$ymin
    zoomValues$ymax <- input$map_brush$ymax
  })
  
  observeEvent(input$reset,{
    zoomValues$xmin <- NULL
    zoomValues$xmax <- NULL
    zoomValues$ymin <- NULL
    zoomValues$ymax <- NULL
  })
  
  selected <- reactiveValues(
    county = NULL,
    margin = NULL
  )
  
  observeEvent(input$map_click,{
    map <- nj_2017_results_sf
    
    point <- st_as_sf(data.frame(x = input$map_click$x, y = input$map_click$y),
                      coords = c("x", "y"))
    
    st_crs(point) <-st_crs(map)
    
    hit <- st_within(point, map)[[1]]
    
    if(length(hit) == 0){
      selected$county <- NULL
      selected$margin <- NULL
    } else {
      a <- hit[1]
      selected$county <- map$COUNTY[a]
      selected$margin <- map$margin[a]
    }
  })
  
    output$county_text <-renderText({
      if(!is.null(selected$county)){
        paste(selected$county, "had a margin of ", selected$margin)
      }
    })
  
    output$margin_map <- renderPlot({
      
      map <- nj_2017_results_sf

      g <- ggplot(map)+
        geom_sf(aes(fill = margin))+
        scale_fill_gradient2(
          low = "red",
          mid = "whitesmoke",
          high = "blue",
          midpoint = 0,
          limits = c(-80,80),
          breaks = seq(-80,80,20)
        )+
        theme_void()+
        labs(
          title = "2017 NJ Governor Election Margin",
          subtitle = "Democratic (blue) vs Republican (red)",
          fill = "Margin win (%)",
          caption = "Source: NJ DOE"
        )
      
      if(!is.null(zoomValues$xmin)){
        g <- g +
          coord_sf(xlim=c(zoomValues$xmin,zoomValues$xmax),
                   ylim=c(zoomValues$ymin,zoomValues$ymax))
      }
      g
  
    })

}
