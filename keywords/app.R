library(shiny)
library(rtweet)
library(config)
library(shinythemes)
library(plotly)
library(leaflet)
library(tidyverse)
library(plotly)
library(shinyjs)
library(ggwordcloud)
library(forcats)
library(RColorBrewer)
library(igraph)
library(ggraph)
library(sf)
library(networkD3)
library(here)
library(DT)

# stop words
stop_words_pck <- rtweet::stopwordslangs %>% 
    dplyr::filter(p >= 0.98)

source("shiny_helpers/text_cleaning.R")
source("shiny_helpers/language_distribution.R")
source("shiny_helpers/bigram.R")
source("shiny_helpers/network_graph.R")
source("shiny_helpers/common_words.R")


Sys.setenv(R_CONFIG_ACTIVE = "default")
config <- config::get(file = "credentials.yml")
key <- config$google_api_key

token <- rtweet::create_token(
    app             = "STAT240DataScience",
    consumer_key    = config$api_key,
    consumer_secret = config$api_secret_key,
    access_token    = config$access_token,
    access_secret   = config$access_token_secret
)

# location <- rtweet::lookup_coords(address = "United States",
#                                      apikey  = key)
#     
# twitter_data <- rtweet::search_tweets(
#     q           = paste0(c("bomb scare", ""), collapse = " "),
#     n           = 200,
#     include_rts = FALSE, 
#     geocode     = location,
#     token       = token
# ) 
# 
# bigram_cleaning(twitter_data, stop_words_pck = stop_words_pck) %>% View()

# Define UI for application that draws a histogram
ui <- shiny::bootstrapPage(
    
    shinyjs::useShinyjs(),
    
    tags$head(
        # Include our custom CSS
        includeCSS("styles.css")
    ),
    shiny::navbarPage(
        
        title       = "Keyword Anaysis",
        collapsible = TRUE,
        inverse     = TRUE, 
        theme       = shinytheme("paper"),
        
        
        shiny::tabPanel(
            
            # Keywords Analysis
            title = "Keyword Tracker",
            shiny::sidebarLayout(
                shiny::sidebarPanel(
                    shiny::textInput(inputId = "query", label = "Keyword", placeholder = "Your Keyword"),
                    shiny::sliderInput(
                        inputId = "n_tweets",
                        label   = "Number of tweets:",
                        min     = 1,
                        max     = 2000,
                        value   = 5),
                    shiny::textInput(inputId = "location", label = "Location", placeholder = "World"),
                    shiny::selectInput(inputId  = "select_lang",
                                       label    = "Select Language",
                                       selected = "All",
                                       choices  = c("All", sort(twitter_languages$lang_long[-1]))),
                    div(
                        shiny::checkboxGroupInput(inputId = "filters", label = "Apply Filters",
                                                  choices = c("verified", "news", "media"),
                                                  inline = TRUE)   
                    ),
                    div(
                        shiny::actionButton(inputId = "submit", label = "Submit", class = "btn-primary"),
                        div(
                            class = "pull-right",
                            shiny::actionButton(
                                inputId = "toggle",
                                label   = NULL,
                                class   = "btn-primary",
                                icon    = shiny::icon("cog")
                            )
                        )
                    ),
                    br(),
                    div(
                        id = "common_words",
                        shiny::sliderInput(inputId = "more_words",
                                           label   = "Display More Words:",
                                           min     = 10,
                                           max     = 20,
                                           value   = 10)
                    ) %>% shinyjs::hidden(),
                ),
                
                # Show a plot of the generated distribution
                shiny::mainPanel(
                    div(
                        class = "row",
                        div(
                            class     = "col-sm-6 panel",
                            div(class = "panel-heading", h5("Word Counts")),
                            div(class = "panel-body", plotly::plotlyOutput(outputId = "plotly", height = "400px"))
                        ),
                        div(
                            class     = "col-sm-6 panel",
                            div(class = "panel-heading", h5("Language Distribution")),
                            div(class = "panel-body", plotly::plotlyOutput(outputId = "language", height = "400px"))
                        )
                    ),
                    div(
                        class = "row",
                        div(
                            class = "col-sm-12 panel",
                            div(class = "panel-heading", h5("Map")),
                            div(class = "panel-body", leaflet::leafletOutput(outputId = "map", height = 400))
                        )
                    )
                    
                )
            )
        ),
        
        # Network Panel UI ----
        shiny::tabPanel(
            title = "Network Graph",
            div(class = "outer",

                # network graph output
                networkD3::forceNetworkOutput(outputId = "graph", width = "100%", height = "100%"),
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
                                id = "hide_slider_network",
                                # Network Slider UI ----
                                shiny::sliderInput(inputId = "slider_network",
                                                   label   = "Choose Network Size",
                                                   min     = 1,
                                                   max     = 500,
                                                   value   = 75)
                            ),
                            div(

                            # Network Action Button Toggle UI ----
                            div(class = "pull-right",
                                shiny::actionButton(
                                    inputId = "toggle_network",
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
        ),

        shiny::tabPanel(
            title = "Wordclouds",
            div(class = "outer",
                shiny::plotOutput(outputId = "wordcloud", height = "100%", width = "100%"),
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
                                id = "hide_radio_wordcloud",
                                # Word cloud Slider UI ----
                                shiny::sliderInput(inputId = "wordcloud_slider",
                                                   label   = "Choose Wordcloud Size",
                                                   min     = 1,
                                                   max     = 10,
                                                   value   = 5),

                                # Word cloud Radio Buttons UI ----
                                shiny::radioButtons(inputId = "radio_wordcloud",
                                                    label    = "Choose Wordcloud",
                                                    selected = "Words",
                                                    choices  = c("Words", "Hashtags", "Handles")
                                )
                            ),
                            div(

                                # Word cloud Action Button Toggle UI ----
                                div(class = "pull-right",
                                    shiny::actionButton(
                                        inputId = "toggle_wordcloud",
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
        ),
        shiny::tabPanel(
            title = "Twitter Data",
            shiny::fluidRow(
                shiny::column(12,
                              DT::dataTableOutput("table", width = "100%")
                )
            )
        )
    )
)

# 2.0 Server ----
server <- function(input, output, session) {
    
    # show word cloud slider if requested
    shiny::observeEvent(input$toggle, {
        
        shiny::req(nrow(rv$twitter_data) > 0)
        shinyjs::toggle(id = "common_words", anim = TRUE)
        
    })
    
    # hide network slider if requested
    shiny::observeEvent(input$toggle_network, {
        
        shiny::req(nrow(rv$twitter_data) > 0)
        shinyjs::toggle(id = "hide_slider_network", anim = TRUE)
        
    })
    
    # hide network slider if requested
    shiny::observeEvent(input$toggle_wordcloud, {
        
        shiny::req(nrow(rv$twitter_data) > 0)
        shinyjs::toggle(id = "hide_radio_wordcloud", anim = TRUE)
        
    })
    
    # when nothing has been submitted but the
    # toggle button has been clicked show modal dialog
    shiny::observeEvent(input$toggle, {
        
        shiny::req(nchar(input$query) == 0, nchar(input$location) == 0)
        
        shiny::modalDialog(
            title = "No Query Specified",
            easyClose = TRUE,
            div(
                tags$p("Please specify a query and location.")
            )
        ) %>% shiny::showModal()
        
    })
    
    # when no query has been specified but
    # the submit button has been clicked, show modal
    shiny::observeEvent(input$submit, {
        
        shiny::req(nchar(input$query) == 0 | nchar(input$location) == 0) 
            
            shiny::modalDialog(
                title = "No Query Specified",
                easyClose = TRUE,
                div(
                    tags$p("Please specify a query and location.")
                )
            ) %>% shiny::showModal()
        
    })
    
    # 2.1 Setup Reactive Values ----
    rv <- shiny::reactiveValues()
    
    shiny::observeEvent(input$submit, {
        
        shiny::req(nchar(input$query) > 0, nchar(input$location) > 0)
        
        withProgress(message = 'Downloading Twitter Data',
                     detail = 'This may take a while...', value = 0, {
        
        # 2.2 Specify Location ----
        rv$location <- rtweet::lookup_coords(address = input$location,
                                             apikey  = key)
        
        if(length(input$filters) > 0) {
            
            rv$filters <- paste0("filter:", input$filters, collapse = " ")
            
        } else {
            
            rv$filters <- ""
            
        }
    
        rv$tweet_language <- base::ifelse(input$select_lang == "All", 
                                          NULL,
                                          twitter_languages$value[which(input$select_lang == twitter_languages$lang_long)])
        
        # 2.3 Pull in Tweets ----
        rv$twitter_data <- rtweet::search_tweets(
                q           = paste0(c(input$query, rv$filters), collapse = ", ") %>%
                    stringr::str_squish(),
                n           = input$n_tweets,
                include_rts = FALSE, 
                geocode     = rv$location,
                token       = token,
                lang        = rv$tweet_language
            )   
        
        
                     })
        
        withProgress(message = 'Creating Plots + Map',
                     detail = 'This may take a while...', value = 0, {
        
        if(nrow(rv$twitter_data) > 0) {
            
            # 2.5 Cleaning Twitter Text ----
            rv$twitter_text <- clean_text(rv$twitter_data)
            rv$common_words <- rv$twitter_text %>%
                common_words(df = ., query = input$query, stop_words_pck = stop_words_pck)
            
            # 2.7 Common Words Plot ----
            output$plotly <- renderPlotly({
                
                plot <- ggplot(rv$common_words %>%
                                   head(input$more_words), aes(x = word, y = n)) +
                    geom_col() +
                    coord_flip()
                
                ggplotly(plot)
                
            })
            
            
            
            # 2.8 Leaflet Map -----
            output$map <- renderLeaflet({

                req(rv$location)

                map_data <- rv$location$point %>%
                    dplyr::bind_rows() %>%
                    dplyr::bind_cols(dplyr::tibble(location = rv$location$place))

                map_border <- list.files("all_maps")[which(stringr::str_detect(paste0(stringr::str_to_lower(input$location), ".rds"), list.files("all_maps")))]
                if(length(map_border) == 1) {

                    rv$map_border <- base::readRDS(paste0("all_maps/", map_border))

                    map_data %>%
                        leaflet() %>%
                        setView(map_data$lng, map_data$lat, zoom = 4) %>%
                        addTiles() %>%
                        addMarkers(~lng, ~lat, popup = ~as.character(location), label = ~as.character(location)) %>%
                        addPolygons(data = rv$map_border,
                                    weight = 0.5)

                } else {

                    map_data %>%
                        leaflet() %>%
                        setView(map_data$lng, map_data$lat, zoom = 4) %>%
                        addTiles() %>%
                        addMarkers(~lng, ~lat, popup = ~as.character(location), label = ~as.character(location))

                }
            })
    
            } else {
                
                shiny::modalDialog(
                title = "No Search Results",
                easyClose = TRUE,
                div(
                    tags$p("Your Twitter query could not retrieve any data.")
                )
            ) %>% shiny::showModal()
            
            }
                         
                     })
        
    })
    
    # 2.9 Wordcloud Plot ----
    shiny::observe({

        shiny::req(nrow(rv$common_words) > 0)
        
        withProgress(message = 'Creating Word Clouds',
                     detail = 'This may take a while...', value = 0, {
                         
        if(input$radio_wordcloud == "Words") {
            
            rv$df_wordcloud <- rv$common_words
            
        } else if(input$radio_wordcloud == "Hashtags") {
            
            rv$df_wordcloud <- rv$twitter_data$hashtags %>%
                unlist() %>%
                na.omit() %>%
                dplyr::tibble(word = .) %>%
                dplyr::count(word, sort = TRUE)
            
        } else {
            
            rv$df_wordcloud <- rv$twitter_data$mentions_screen_name %>%
                unlist() %>%
                na.omit() %>%
                dplyr::tibble(word = .) %>%
                dplyr::count(word, sort = TRUE)
            
        }
        
        
        # if less than 100 rows for bigram data, decrease slider range
        rv$max_slider_input_wc <- min(100, nrow(rv$df_wordcloud))
        
        
        # update slider based on requested data set
        shiny::updateSliderInput(session, 
                                 inputId = "wordcloud_slider",
                                 min     = 1,
                                 max     = rv$max_slider_input_wc,
                                 value   = rv$max_slider_input_wc*0.25)
        
        output$wordcloud <- shiny::renderPlot({
            
            ggplot(rv$df_wordcloud[1:max(1, input$wordcloud_slider), ], aes(label = word, size = n, col = as.character(n))) +
                geom_text_wordcloud(rm_outside = TRUE, max_steps = 1,
                                    grid_size = 1, eccentricity = .9) +
                scale_size_area(max_size = 14) +
                #scale_color_brewer(palette = "paired", direction = -1) +
                theme_void()

        })
        
                     })

    })
    
    # 2.10 Language Distribution Graph ----
    shiny::observe({
        
        shiny::req(nrow(rv$common_words) > 0)
        
        output$language <- plotly::renderPlotly({
            
            language_distribution(rv$twitter_data) %>%
                ggplotly()
            
        })
        
    })
    
    
    # Plot Network D3 Graph ----
    shiny::observe({
        
        withProgress(message = 'Network Graph',
                     detail = 'This may take a while...', value = 0, {
        
        # only renders when requested twitter data is available and
        # the submit button is clicked on first page or
        # the submit button is clicked on second page
        shiny::req(nrow(rv$common_words) > 0)
        
        
        # data cleaning for bigram
        rv$bigram_data <- rv$twitter_text %>%
            bigram(df = ., stop_words_pck = stop_words_pck) %>%
            dplyr::rename(weight = n)
        
        # if less than 500 rows for bigram data, decrease slider range
        rv$max_slider_input <- min(500, nrow(rv$bigram_data))
        
        # update slider based on requested data set
        shiny::updateSliderInput(session, 
                                 inputId = "slider_network",
                                 min     = 1,
                                 max     = rv$max_slider_input,
                                 value   = rv$max_slider_input*0.25)
        
        
        output$graph <- networkD3::renderForceNetwork({
            
            rv$bigram_data %>%
                .[1:max(1, input$slider_network), ] %>%
                igraph::graph_from_data_frame() %>%
                    
                # my own defined function in shiny_helpers
                network_graph()
            
        })  
        
                     })
        
    })
    
    # Twitter data table
    output$table <- DT::renderDataTable({
        
        req(nrow(rv$twitter_data) > 0)
        
        rv$twitter_data %>%
            dplyr::mutate(Link = paste0("<a href='",
                                        "https://twitter.com/", screen_name, "/status/", status_id,
                                        "' target='_blank'>", "Tweet","</a>")) %>%
            dplyr::select(Text = text, Link) 
        
    }, escape = FALSE)
    
}

shinyApp(ui, server)
