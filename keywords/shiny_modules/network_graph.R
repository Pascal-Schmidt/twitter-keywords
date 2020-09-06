network_ui <- function(id) {
  
  ns <- shiny::NS(id)
  
  shiny::tagList(
    div(class = "outer",
        
        # network graph output
        networkD3::forceNetworkOutput(outputId = ns("graph"), width = "100%", height = "100%"),
        shiny::absolutePanel(
          style = "top:90px;left:10px;",
          fixed = TRUE,
          draggable = TRUE,
          div(
            id = "controls",
            class = "panel panel-default",
            div(
              class="panel-body",
              div(
                id = ns("hide_slider_network"),
                # Network Slider UI ----
                shiny::sliderInput(inputId = ns("slider_network"),
                                   label   = "Choose Network Size",
                                   min     = 1,
                                   max     = 500,
                                   value   = 75)
              ),
              div(
                
                # Network Action Button Toggle UI ----
                div(class = "pull-right",
                    shiny::actionButton(
                      inputId = ns("toggle_network"),
                      label   = NULL,
                      class   = "btn-default",
                      icon    = shiny::icon("cog")
                    )
                )
              )
            )
          )
        )
    )
  )


}

# • Server
network_server <- function(id, common_words, twitter_text, twitter_data) {
  
  shiny::moduleServer(
    id,
    
    function(input, output, session) {
      
      # • hide network slider if requested
      shiny::observeEvent(input$toggle_network, {
        
        shiny::req(nrow(twitter_data()) > 0)
        shinyjs::toggle(id = "hide_slider_network", anim = TRUE)
        
      })
      
      
      withProgress(message = 'Network Graph',
                   detail = 'This may take a while...', value = 0, {
                     
                     # • Compute bigram data frame ----
                     bigram_data <- shiny::reactive({
                       
                       shiny::req(nrow(common_words()) > 0)
                       
                       bigram_data <- twitter_text() %>%
                         bigram(df = ., stop_words_pck = stop_words_pck) %>%
                         dplyr::rename(weight = n)
                       return(bigram_data)
                       
                     })
                     
                     # • update slider range ----
                     shiny::observe({
                       
                       # only renders when requested twitter data is available and
                       # the submit button is clicked on first page or
                       # the submit button is clicked on second page
                       shiny::req(nrow(common_words()) > 0)
                       
                       # if less than 500 rows for bigram data, decrease slider range
                       max_slider_input <- min(500, nrow(bigram_data()))
                       
                       # update slider based on requested data set
                       shiny::updateSliderInput(session, 
                                                inputId = "slider_network",
                                                min     = 1,
                                                max     = max_slider_input,
                                                value   = max_slider_input*0.25)
                       
                       
                     })
                     
                     # • plot network ----
                     output$graph <- networkD3::renderForceNetwork({
                       
                       bigram_data() %>%
                         .[1:max(1, input$slider_network), ] %>%
                         igraph::graph_from_data_frame() %>%
                         
                         # my own defined function in shiny_helpers
                         network_graph()
                       
                     })  
                     
                   })
      
    }
    
    
  )
  
}


