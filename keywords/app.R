# 1.0 Set up ----

# • load libraries ----
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
library(networkD3)
library(DT)
library(shinycssloaders)
library(jsonlite)
library(rvest)
library(shinyWidgets)


# • load stop words ----
stop_words_pck <<- rtweet::stopwordslangs %>% 
    dplyr::filter(p >= 0.98)

# • load shiny modules ----
list.files("shiny_modules") %>%
    purrr::map(~ source(paste0("shiny_modules/", .)))

# • load shiny helper functions ----
list.files("shiny_helpers") %>%
    purrr::map(~ source(paste0("shiny_helpers/", .)))

# • set up API credentials for Twitter and Google ----
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

# location <- rtweet::lookup_coords(address = "India",
#                                      apikey  = key)
# 
# twitter_data <- rtweet::search_tweets(
#      q           = "fire",
#      n           = 1000,
#      include_rts = FALSE,
#      geocode     = location,
#      token       = token
#  )
#  twitter_data %>%
#      dplyr::mutate(date = lubridate::ymd(stringr::str_remove(as.character(twitter_data$created_at), " .*"))) %>%
#      dplyr::count(date) -> plotly_plot
# 
#  plotly_plot[, "cat"] = sample(x = c("A", "B"), size = nrow(plotly_plot), replace = T)
# 
#  sample(c("A", "B"), nrow(plotly_plot), replace = TRUE)
# 
# 
# plot_ly(plotly_plot, x = ~ date) %>%
#     add_lines(y = ~fitted(loess(n ~ as.numeric(date))), color = ~ cat) %>%
#     add_markers(y = ~n, color = ~ cat) %>%
#     layout(showlegend = FALSE,
#            xaxis = list(rangeslider = list(type = "date")))


# 
# twitter_data %>%
#     dplyr::mutate(date = lubridate::floor_date(created_at, unit = "day")) %>%
#     dplyr::count(date) %>%
#     plot_ly(x = ~ date) %>%
#     add_lines(y = ~n, color = I("#2196f3")) %>%
#     add_markers(y = ~n, color = I("#2196f3")) %>%
#     layout(showlegend = FALSE,
#            xaxis = list(rangeslider = list(type = "date")))
#     
# View(twitter_data)
# 
# twitter_data %>%
#     dplyr::pull(created_at) %>%
#     range() %>%
#     as.character() %>%
#     stringr::str_remove(" .*")
# 
# bigram_cleaning(twitter_data, stop_words_pck = stop_words_pck) %>% View()

# 2.0 Shiny UI ----
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
        theme       = shinythemes::shinytheme("paper"),
        
        
        shiny::tabPanel(
            
            # Keywords Analysis
            title = "Keyword Tracker",
            shiny::sidebarLayout(
                shiny::sidebarPanel(
                    div(
                        shiny::dateRangeInput(inputId = "old_tweets_date", 
                                              label = tags$strong("Select Date Range"))  
                    ),
                    shiny::textInput(inputId = "query", 
                                     label = tags$strong("Keyword"), 
                                     placeholder = "Your Keyword"),
                    shiny::sliderInput(
                        inputId = "n_tweets",
                        label   = tags$strong("Number of Tweets"),
                        min     = 1,
                        max     = 5000,
                        value   = 500),
                    shiny::textInput(inputId = "location", 
                                     label = tags$strong("Location"), 
                                     placeholder = "World"),
                    shiny::selectInput(inputId  = "select_lang",
                                       label    = tags$strong("Select Language"),
                                       selected = "All",
                                       choices  = c("All", sort(twitter_languages$lang_long[-1]))),
                    div(
                        shiny::checkboxGroupInput(inputId = "filters", label = tags$strong("Apply Filters"),
                                                  choices = c("verified", "news", "media"),
                                                  inline = TRUE)   
                    ),
                    div(
                        shiny::checkboxInput(
                            inputId = "old_tweets", label = "Get Old Tweets") 
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
                    
                    shiny::uiOutput("info_cards"),
                    
                    div(
                        class = "row",   
                        shiny::uiOutput("wordcounts_large"),
                    ),
                    
                    # • Map UI Module ----
                    map_ui("map")
                    
                )
            )
        ),
        
        # • Network Graph Tab + Module UI ----
        shiny::tabPanel(
            title = "Network Graph",
            network_ui("network")
        ),
    
        # • Word Cloud Tab + Module UI ----
        shiny::tabPanel(
            title = "Wordclouds",
            wordcloud_ui("wordclouds")
        ),
        
        # • Table Tab + Module UI ----
        shiny::tabPanel(
            title = "Twitter Data",
            shiny::fluidRow(
                table_ui("tweet_data")
            )
            
        )
    )
)

# 3.0 Server ----
server <- function(input, output, session) {
    
    output$wordcounts_large <- shiny::renderUI({
        
        if(sum(input$submit) == 0 | length(unique(rv$twitter_data$lang)) > 1) {
            
            shiny::tagList(
                div(
                    class     = "col-sm-6 panel",
                    div(class = "panel-heading clearfix", 
                        div(
                            shiny::tags$h5(class = "pull-left",
                                           "Word Counts"),
                            div(class = "pull-right",
                                shiny::downloadButton(outputId = "words_down",
                                                      label    = NULL,
                                                      class    = "btn-primary")
                            )
                        )
                        
                    ),
                    div(class = "panel-body",
                        plotly::plotlyOutput(outputId = "plotly", height = "400px") %>%
                            shinycssloaders::withSpinner()
                    )
                    
                    
                    
                ),
                div(
                    id        = "language_graph",
                    class     = "col-sm-6 panel",
                    div(class = "panel-heading", h5("Language Distribution")),
                    div(class = "panel-body", plotly::plotlyOutput(outputId = "language", height = "400px") %>%
                            shinycssloaders::withSpinner()
                        )
                )
            )
            
        } else {
            
            div(
                class     = "col-sm-12 panel",
                div(class = "panel-heading clearfix", 
                    div(
                        shiny::tags$h5(class = "pull-left",
                                       "Word Counts"),
                        div(class = "pull-right",
                            shiny::downloadButton(outputId = "words_down",
                                                  label    = NULL,
                                                  class    = "btn-primary")
                        )
                    )
                    
                ),
                div(class = "panel-body",
                    plotly::plotlyOutput(outputId = "plotly", height = "400px") %>%
                        shinycssloaders::withSpinner()
                )
                
                
                
            )
            
        }
        
    })
    
    output$plotly <- renderPlotly({
    })
    # output$map <- renderPlotly({
    # })
    output$language <- renderPlotly({
    })
    
    # show word cloud slider if requested
    shiny::observeEvent(input$toggle, {
        
        shiny::req(nrow(rv$twitter_data) > 0)
        shinyjs::toggle(id = "common_words", anim = TRUE)
        
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
    rv <<- shiny::reactiveValues()
    
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
                                                           "",
                                                           twitter_languages$value[which(twitter_languages$lang_long %in% input$select_lang)])
                         
                         
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
    
        
        rv$tweets_archived <- get_old_tweets(input$old_tweets_date[1], input$old_tweets_date[2], rv$tweet_language, rv$location,
                                             input$n_tweets, input$query, access_token = token) %>%
            dplyr::mutate(data_source = "Archived")
        
        rv$counts_archived <- nrow(rv$tweets_archived)
        rv$counts_api <- nrow(rv$twitter_data)
        
        rv$twitter_data %>%
            dplyr::mutate(data_source = "API") %>%
            dplyr::bind_rows(rv$tweets_archived) %>%
            dplyr::distinct(text, .keep_all = TRUE) -> rv$twitter_data
    
        
        if(length(unique(rv$twitter_data$lang)) == 1) {
            
            shinyjs::hide(id = "language_graph", anim = TRUE)
            
        } else {
            
            shinyjs::show(id = "language_graph", anim = TRUE)
            
        }
        
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
                                     geom_col(fill = "#2196f3") +
                                     coord_flip()
                                 
                                 ggplotly(plot)
                                 
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
    
    output$info_cards <- shiny::renderUI({
        
        shiny::req(!is.null(nrow(rv$twitter_data)))
        
        rv$dates <- rv$twitter_data %>%
            dplyr::pull(created_at) %>%
            range() %>%
            as.character() %>%
            stringr::str_remove(" .*")
        rv$most_tweets <- rv$twitter_data$screen_name %>%
            table() %>%
            sort() %>%
            .[length(.)] %>% 
            names()
        
        shiny::tagList(
            shiny::fluidRow(
                id = "show_info_cards",
                column(
                    width = 4,
                    div(
                        class = "panel panel-default",
                        style = "background-color: #2196f3;",
                        
                        div(
                            class = "panel-body",
                            div(
                                style = "color: white;",
                                tags$h5(tags$small("Tweets Retrieved (API)", style = "color: white;"),
                                        shiny::HTML("<br/>"),
                                        tags$strong(rv$counts_api), 
                                        shiny::HTML("<br/>"), 
                                        tags$small("Tweets Retrieved (Archived)", style = "color: white;"),
                                        shiny::HTML("<br/>"), 
                                        tags$strong(rv$counts_archived),
                                        style = "color: white;"),
                                shiny::icon("twitter", class = "fa-5x") %>%
                                    div(class = "pull-right")
                            )
                        )
                    )
                ),
                column(
                    width = 4,
                    div(
                        class = "panel panel-default",
                        style = "background-color: #2196f3;",
                        
                        div(
                            class = "panel-body",
                            div(
                                style = "color: white;",
                                tags$h5(tags$small("Earliest Tweet on ", style = "color: white;"), 
                                        shiny::HTML("<br/>"),
                                        tags$strong(rv$dates[1], style = "color: white;"),
                                        shiny::HTML("<br/>"),
                                        tags$small("Latest Tweet on ", style = "color: white;"), 
                                        shiny::HTML("<br/>"),
                                        tags$strong(rv$dates[2], style = "color: white;")),
                                shiny::icon("twitter", class = "fa-5x") %>%
                                    div(class = "pull-right")
                            )
                        )
                    )
                ),
                column(
                    width = 4,
                    div(
                        class = "panel panel-default",
                        style = "background-color: #2196f3;",
                        
                        div(
                            class = "panel-body",
                            div(
                                style = "color: white;",
                                tags$h5(tags$small("Most Tweets by ", style = "color: white;"), 
                                        shiny::HTML("<br/>"),
                                        tags$strong(rv$most_tweets, style = "color: white;")),
                                shiny::icon("twitter", class = "fa-5x") %>%
                                    div(class = "pull-right")
                            )
                        ) 
                    ) 
                )
            ),
            div(
                class = "row",
                div(
                    class = "col-sm-12 panel",
                    div(class = "panel-heading", h5("Tweet Volume")),
                    div(class = "panel-body", plotly::plotlyOutput(outputId = "tweet_volume", height = 400) %>%
                            shinycssloaders::withSpinner()
                    )
                )
            )
        )
        
        
    })
    

    output$tweet_volume <- plotly::renderPlotly({
        
        rv$twitter_data %>%
            dplyr::mutate(date = lubridate::floor_date(created_at, unit = "day")) %>%
            dplyr::count(date, data_source) %>%
            plot_ly(x = ~ date) %>%
            add_lines(y = ~fitted(loess(n ~ as.numeric(date))), color = ~ data_source) %>%
            add_markers(y = ~n, color = ~ data_source) %>%
            layout(showlegend = FALSE,
                   xaxis = list(rangeslider = list(type = "date")))
        
    })
    
    # 2.10 Language Distribution Graph ----
    shiny::observe({
        
        shiny::req(nrow(rv$common_words) > 0)
        
        output$language <- plotly::renderPlotly({
            
            language_distribution(rv$twitter_data) %>%
                ggplotly()
            
        })
        
    })
    
    # • Map ----
    map_server("map", reactive(rv$location))
    
    # • Network Graph + Module Server ----
    network_server("network", 
                   common_words = reactive(rv$common_words), 
                   twitter_text = reactive(rv$twitter_text), 
                   twitter_data = reactive(rv$twitter_data))
    
    # • Word Cloud Plot ----
    wordcloud_server("wordclouds", 
                     twitter_data = reactive(rv$twitter_data), 
                     common_words = reactive(rv$common_words), 
                     location = reactive(rv$location), 
                     query = reactive(input$query))
    
    # • Tweet Table ----
    table_server("tweet_data", 
                 twitter_data = reactive(rv$twitter_data), 
                 location = reactive(rv$location), 
                 query = reactive(input$query))
    
    # download common words ----
    output$words_down <- shiny::downloadHandler(
        
        filename = function() {
            paste0('common_words_', input$query, "_", rv$location, '.csv')
        },
        content = function(con) {
            readr::write_csv(rv$common_words, con)
        }
        
    )
    
}

shinyApp(ui, server)
