table_ui <- function(id) {
  
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::column(
      width = 12,
      div(
        class = "pull-right",
        shiny::downloadButton(outputId = ns("twitter_data"),
                              label    = NULL,
                              class    = "btn-default")
      ),
      br(),
      br(),
      DT::dataTableOutput(ns("table"), width = "100%")
    )
  )
  
}

table_server <- function(id, twitter_data, location, query) {
  
  shiny::moduleServer(
    id,
    
    function(input, output, session) {
      
      # Twitter data table
      output$table <- DT::renderDataTable({
        
        
        withProgress(message = 'Creating Table', value = 0, {
                       req(nrow(twitter_data()) > 0)
                       
                       twitter_data() %>%
                         dplyr::mutate(Link = paste0("<a href='",
                                                     "https://twitter.com/", screen_name, "/status/", status_id,
                                                     "' target='_blank'>", "Tweet","</a>")) %>%
                         dplyr::select(Text = text, Link)
                       
                     })
        
      }, escape = FALSE)
      
      # download data ----
      output$twitter_data<- shiny::downloadHandler(
        
        filename = function() {
          paste0('twitter_data_', query(), "_", location(), '.csv')
        },
        content = function(con) {
          twitter_data() %>%
            dplyr::mutate(Link = paste0("<a href='",
                                        "https://twitter.com/", screen_name, "/status/", status_id,
                                        "' target='_blank'>", "Tweet","</a>")) %>%
            dplyr::select(Text = text, Link) -> twitter_down
          readr::write_csv(twitter_down, con)
        }
        
      )
      
      
    }
  )
  
}