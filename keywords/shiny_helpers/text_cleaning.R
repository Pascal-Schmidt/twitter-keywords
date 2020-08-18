clean_text <- function(df) {
  
  # remove links, handles, mentions and other unnecessary words
  remove_this <- c("\\n", "&amp;", "&lt;", "&gt;", "like", "can",
                   "https://t.co/[a-z,A-Z,0-9]*", "http://t.co/[a-z,A-Z,0-9]*",
                   "https", "http", "#[a-z,A-Z]*", "@[a-z,A-Z]*") %>%
    paste0(collapse = "|")
  
  # tweets cleaning
  df %>%
    
    # remove handles
    dplyr::mutate(text = stringr::str_remove_all(text,
                                                 pattern = na.omit(purrr::flatten_chr(mentions_screen_name)) %>%
                                                   paste0("@", ., collapse = "|")),
                  
                  # remove hashtags
                  text = stringr::str_remove_all(text,
                                                 pattern = na.omit(purrr::flatten_chr(hashtags)) %>%
                                                   paste0("#", ., collapse = "|")),
                  
                  # remove unnecessary text
                  text = stringr::str_remove_all(text, pattern = remove_this)) %>%
    
    # convert to lower
    dplyr::mutate(text = stringr::str_to_lower(text)) %>% 
    
    dplyr::select(text) -> df
  
  return(df)
  
}