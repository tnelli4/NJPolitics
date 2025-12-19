

library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {
  
  zoomValues <- reactiveValues(
    xmin = NULL,
    xmax = NULL,
    ymin = NULL,
    ymax = NULL
  )
  
  hovered17 <- reactiveValues(
    county = NULL,
    margin = NULL,
    x = NULL,
    y = NULL
  )
  
  selected17 <- reactiveValues(
    county = NULL,
    margin = NULL,
    x = NULL,
    y = NULL
  )
  
  hovered25 <- reactiveValues(
    county = NULL,
    margin = NULL,
    x = NULL,
    y = NULL
  )
  
  selected25 <- reactiveValues(
    county = NULL,
    margin = NULL,
    x = NULL,
    y = NULL
  )

# -------------------------------------------------------------------------


  
  observeEvent(input$reset,{
    zoomValues$xmin <- NULL
    zoomValues$xmax <- NULL
    zoomValues$ymin <- NULL
    zoomValues$ymax <- NULL
    session$resetBrush("map_brush17")
    session$resetBrush("map_brush25")
    
    
  })

# 2017 observers ----------------------------------------------------------

  observeEvent(input$map_brush17,{
    print(input$map_brush)
    zoomValues$xmin <- input$map_brush17$xmin
    zoomValues$xmax <- input$map_brush17$xmax
    zoomValues$ymin <- input$map_brush17$ymin
    zoomValues$ymax <- input$map_brush17$ymax
    
    session$resetBrush("map_brush17")
    
  })
  
  observeEvent(input$map_hover17, {
    map <- nj_2017_results_sf
    
    if (is.null(input$map_hover17)) {
      hovered17$county <- NULL
      hovered17$margin <- NULL
      hovered17$x <- NULL
      hovered17$y <- NULL
      return()
    }
    
    pt <- st_as_sf(
      data.frame(x = input$map_hover17$x, y = input$map_hover17$y),
      coords = c("x", "y")
    )
    st_crs(pt) <- st_crs(map)
    
    hit <- st_within(pt, map)[[1]]
    
    if (length(hit) == 0) {
      hovered17$county <- NULL
      hovered17$margin <- NULL
      hovered17$x <- NULL
      hovered17$y <- NULL
    } else {
      i <- hit[1]
      hovered17$county <- map$COUNTY[i]
      hovered17$margin <- map$margin[i]
      hovered17$x <- input$map_hover17$x
      hovered17$y <- input$map_hover17$y
    }
  })
  
  observeEvent(input$map_click17, {
    map <- nj_2017_results_sf
    
    pt <- st_as_sf(
      data.frame(x = input$map_click17$x, y = input$map_click17$y),
      coords = c("x", "y")
    )
    st_crs(pt) <- st_crs(map)
    
    hit <- st_within(pt, map)[[1]]
    
    if (length(hit) == 0) {
      return()
    } else {
      i <- hit[1]
      clicked_county <- map$COUNTY[i]
      
      if (!is.null(selected17$county) && selected17$county == clicked_county) {
        selected17$county <- NULL
        selected17$margin <- NULL
        selected17$x <- NULL
        selected17$y <- NULL
        # cat("DEselected17:", clicked_county, "\n")
      } else {

                selected17$county <- clicked_county
        selected17$margin <- map$margin[i]
        selected17$x <- input$map_click17$x
        selected17$y <- input$map_click17$y
        # cat("selected17:", selected17$county, selected17$margin, "\n")
      }
    }
  })

# 2025 observers ----------------------------------------------------------
  observeEvent(input$map_brush25,{
    print(input$map_brush)
    zoomValues$xmin <- input$map_brush25$xmin
    zoomValues$xmax <- input$map_brush25$xmax
    zoomValues$ymin <- input$map_brush25$ymin
    zoomValues$ymax <- input$map_brush25$ymax
    
    session$resetBrush("map_brush25")
    
  })
  
  observeEvent(input$map_hover25, {
    map <- nj_2017_results_sf
    
    if (is.null(input$map_hover25)) {
      hovered25$county <- NULL
      hovered25$margin <- NULL
      hovered25$x <- NULL
      hovered25$y <- NULL
      return()
    }
    
    pt <- st_as_sf(
      data.frame(x = input$map_hover25$x, y = input$map_hover25$y),
      coords = c("x", "y")
    )
    st_crs(pt) <- st_crs(map)
    
    hit <- st_within(pt, map)[[1]]
    
    if (length(hit) == 0) {
      hovered25$county <- NULL
      hovered25$margin <- NULL
      hovered25$x <- NULL
      hovered25$y <- NULL
    } else {
      i <- hit[1]
      hovered25$county <- map$COUNTY[i]
      hovered25$margin <- map$margin[i]
      hovered25$x <- input$map_hover25$x
      hovered25$y <- input$map_hover25$y
    }
  })
  
  observeEvent(input$map_click25, {
    map <- nj_2017_results_sf
    
    pt <- st_as_sf(
      data.frame(x = input$map_click25$x, y = input$map_click25$y),
      coords = c("x", "y")
    )
    st_crs(pt) <- st_crs(map)
    
    hit <- st_within(pt, map)[[1]]
    
    if (length(hit) == 0) {
      return()
    } else {
      i <- hit[1]
      clicked_county <- map$COUNTY[i]
      
      if (!is.null(selected25$county) && selected25$county == clicked_county) {
        selected25$county <- NULL
        selected25$margin <- NULL
        selected25$x <- NULL
        selected25$y <- NULL
        # cat("DEselected25:", clicked_county, "\n")
      } else {
        
        selected25$county <- clicked_county
        selected25$margin <- map$margin[i]
        selected25$x <- input$map_click25$x
        selected25$y <- input$map_click25$y
        # cat("selected25:", selected25$county, selected25$margin, "\n")
      }
    }
  })

  
  

# output of margin map 17 -------------------------------------------------

  
    output$margin_map17 <- renderPlot({
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
      
      if (!is.null(hovered17$county)) {
        hovered17_data <- filter(map, COUNTY == hovered17$county)
        g <- g + geom_sf(data = hovered17_data, 
                         linewidth = 1, 
                         color = "black", 
                         fill = NA)
      }
      
      if(!is.null(zoomValues$xmin)){
        g <- g +
          coord_sf(xlim=c(zoomValues$xmin,zoomValues$xmax),
                   ylim=c(zoomValues$ymin,zoomValues$ymax),
                   expand = FALSE,
                   clip = "off")
      } else {
        g <- g + coord_sf(expand = FALSE, clip = "off")
      }
      
      if (!is.null(selected17$county)) {
        g <- g + annotate(
          "label",
          x = selected17$x,
          y = selected17$y,
          fill = "grey",
          label = paste0(selected17$county, ": ", round(selected17$margin, 1), "%")
        )
      }
      
      if (!is.null(hovered17$county)) {
        hovered17_data <- filter(map, COUNTY == hovered17$county)
        centroid <- st_centroid(st_geometry(hovered17_data))
        coords <- st_coordinates(centroid)
        
        g <- g + annotate(
          "label",
          x = coords[1],
          y = coords[2],
          label = paste0(hovered17_data$COUNTY, ": ", round(hovered17_data$margin, 1), "%"),
          size = 4,
          fill = "grey",
          fontface = "bold"
        )
      }
      g
    })

# output of margin map 25 -------------------------------------------------
  output$margin_map25 <- renderPlot({
    map <- nj_2025_results_sf
    
    g2 <- ggplot(map)+
      geom_sf(aes(fill = margin_2025))+
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
        title = "2025 NJ Governor Election Margin",
        subtitle = "Democratic (blue) vs Republican (red)",
        fill = "Margin win (%)",
        caption = "Source: NJ DOE"
      )
    
    if (!is.null(hovered25$county)) {
      hovered25_data <- filter(map, COUNTY == hovered25$county)
      g2 <- g2 + geom_sf(data = hovered25_data, 
                       linewidth = 1, 
                       color = "black", 
                       fill = NA)
    }
    
    if(!is.null(zoomValues$xmin)){
      g2 <- g2 +
        coord_sf(xlim=c(zoomValues$xmin,zoomValues$xmax),
                 ylim=c(zoomValues$ymin,zoomValues$ymax),
                 expand = FALSE,
                 clip = "off")
    } else {
      g2 <- g2 + coord_sf(expand = FALSE, clip = "off")
    }
    
    if (!is.null(selected25$county)) {
      g2 <- g2 + annotate(
        "label",
        x = selected25$x,
        y = selected25$y,
        fill = "grey",
        label = paste0(selected25$county, ": ", round(selected25$margin, 1), "%")
      )
    }
    
    if (!is.null(hovered25$county)) {
      hovered25_data <- filter(map, COUNTY == hovered25$county)
      centroid <- st_centroid(st_geometry(hovered25_data))
      coords <- st_coordinates(centroid)
      
      g2 <- g2 + annotate(
        "label",
        x = coords[1],
        y = coords[2],
        label = paste0(hovered25_data$COUNTY, ": ", round(hovered25_data$margin, 1), "%"),
        size = 4,
        fill = "grey",
        fontface = "bold"
      )
    }
    g2
  })
  
  
}