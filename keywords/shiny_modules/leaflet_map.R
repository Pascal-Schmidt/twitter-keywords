map_ui <- function(id) {
  
  ns <- shiny::NS(id)
  
  shiny::tagList(
    div(
      class = "row",
      div(
        class = "col-sm-12 panel",
        div(class = "panel-heading", h5("Map")),
        div(class = "panel-body", leaflet::leafletOutput(outputId = ns("map"), height = 400)
        )
      )
    )
  )
  
}


map_server <- function(id, location) {
  
  shiny::moduleServer(
    id,
    
    function(input, output, session) {
      output$map <- renderLeaflet({
        
        req(location())
        
        map_data <- location()$point %>%
          dplyr::bind_rows() %>%
          dplyr::bind_cols(dplyr::tibble(location = location()$place))
        
        
        map_data %>%
          leaflet() %>%
          setView(map_data$lng, map_data$lat, zoom = 4) %>%
          addTiles() %>%
          addMarkers(~lng, ~lat, popup = ~as.character(location), label = ~as.character(location))
        
      })
    }
    
  )
  
}