library(tidytext)

common_words <- function(df, query, stop_words_pck = stop_words_pck) {
  
  other <- stringr::str_replace_all(stop_words$word, pattern = "’", "'")
  
  # remove the search term itself
  search_query <- stringr::str_split(query, pattern = " ") %>%
    purrr::flatten_chr() %>%
    stringr::str_to_lower()
  
  df %>%
    tidytext::unnest_tokens(word, text, token = "tweets") %>%
    dplyr::select(word) %>%
    
    # remove stop words
    dplyr::filter(!word %in% stop_words$word,
                  !word %in% other,
                  !word %in% stop_words_pck$word,
                  !word %in% str_remove_all(stop_words$word, "'"),
                  !word %in% str_remove_all(stop_words$word, "’"),
                  !word %in% search_query,
                  stringr::str_detect(word, "[a-z]")) %>%
    dplyr::count(word, sort = TRUE) %>%
    dplyr::mutate(word = forcats::fct_reorder(word, n)) -> df
  
  return(df)
  
}
