# 1.0 UI ----
wordcloud_ui <- function(id) {
  
  ns <- shiny::NS(id)
  
  shiny::tagList(
    div(class = "outer",
        shiny::plotOutput(outputId = ns("wordcloud"), height = "100%", width = "100%"),
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
                id = ns("hide_radio_wordcloud"),
                
                # • Word cloud Slider ----
                shiny::sliderInput(inputId = ns("wordcloud_slider"),
                                   label   = "Choose Wordcloud Size",
                                   min     = 1,
                                   max     = 10,
                                   value   = 5),
                
                # • Word cloud Radio Buttons ----
                shiny::radioButtons(inputId = ns("radio_wordcloud"),
                                    label    = "Choose Wordcloud",
                                    selected = "Words",
                                    choices  = c("Words", "Hashtags", "Handles")
                )
              ),
              div(
                
                # • Word cloud Download Button Toggle ----
                div(class = "pull-right",
                    shiny::downloadButton(outputId = ns("wordclouds"),
                                          label    = NULL,
                                          class    = "btn-default"),
                    
                    # • Word Cloud Action Button ----
                    shiny::actionButton(
                      inputId = ns("toggle_wordcloud"),
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

# 2.0 Server ----
wordcloud_server <- function(id, twitter_data, common_words, location, query) {
  
  shiny::moduleServer(
    id,
    
    function(input, output, session) {
      
      # • hide network slider if requested ----
      shiny::observeEvent(input$toggle_wordcloud, {
        
        shiny::req(nrow(twitter_data()) > 0)
        shinyjs::toggle(id = "hide_radio_wordcloud", anim = TRUE)
        
      })
      
      withProgress(message = 'Creating Word Clouds',
                   detail = 'This may take a while...', value = 0, {
                     
                     # • get word cloud type ----
                     df_wordcloud <- shiny::reactive({
                       
                       shiny::req(nrow(common_words()) > 0)
                       
                       if(input$radio_wordcloud == "Words") {
                         
                         df_wordcloud <- common_words()
                         
                       } else if(input$radio_wordcloud == "Hashtags") {
                         
                         df_wordcloud <- twitter_data()$hashtags %>%
                           unlist() %>%
                           na.omit() %>%
                           dplyr::tibble(word = .) %>%
                           dplyr::count(word, sort = TRUE)
                         
                       } else {
                         
                         df_wordcloud <- twitter_data()$mentions_screen_name %>%
                           unlist() %>%
                           na.omit() %>%
                           dplyr::tibble(word = .) %>%
                           dplyr::count(word, sort = TRUE)
                         
                       }
                       
                       return(df_wordcloud)
                       
                     })   
                     
                     # • choose slider range ----
                     shiny::observe({
                       
                       shiny::req(nrow(common_words()) > 0)
                       
                       # if less than 100 rows for bigram data, decrease slider range
                       max_slider_input_wc <- min(100, nrow(df_wordcloud()))
                       
                       
                       # update slider based on requested data set
                       shiny::updateSliderInput(session, 
                                                inputId = "wordcloud_slider",
                                                min     = 1,
                                                max     = max_slider_input_wc,
                                                value   = max_slider_input_wc*0.25)
                       
                       
                     })
                     
                     wordcloud_df_obj <- shiny::reactive({
                       
                       plot <- ggplot(df_wordcloud()[1:max(1, input$wordcloud_slider), ], aes(label = word, size = n, col = as.character(n))) +
                         geom_text_wordcloud(rm_outside = TRUE, max_steps = 1,
                                             grid_size = 1, eccentricity = .9) +
                         scale_size_area(max_size = 14) +
                         # scale_color_brewer(palette = "paired", direction = -1) +
                         theme_void()
                       
                       return(plot)
                       
                     })
                     
                     # • plot word clouds ----               
                     output$wordcloud <- shiny::renderPlot({
                       
                       wordcloud_df_obj()
                       
                     })
                     
                   })
      
      # • download word clouds ----
      output$wordclouds <- shiny::downloadHandler(
        
        filename = function() {
          paste0('wordcloud_', query(), "_", location(), '.png')
        },
        content = function(con) {
          ggsave(con, plot = wordcloud_df_obj(), device = "png")
        }
        
      )
      
    }
  )
  
}
