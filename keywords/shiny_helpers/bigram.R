bigram <- function(df, stop_words_pck = stop_words_pck) {
  
  other <- stringr::str_replace_all(stop_words$word, pattern = "'", "’")
  
  df %>%
    tidytext::unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    tidyr::separate(bigram, c("word1", "word2"), sep = " ") %>%
    
    # remove stopwords
    dplyr::filter(!word1 %in% stop_words$word,
                  !word2 %in% stop_words$word,
                  !word1 %in% stop_words_pck$word,
                  !word2 %in% stop_words_pck$word,
                  !word1 %in% other,
                  !word2 %in% other,
                  !word1 %in% stringr::str_remove_all(tidytext::stop_words$word, "'"),
                  !word2 %in% stringr::str_remove_all(tidytext::stop_words$word, "'"),
                  stringr::str_detect(word1, "[a-z]"),
                  stringr::str_detect(word2, "[a-z]")) %>%
    dplyr::count(word1, word2, sort = TRUE) -> df
  
  return(df)

}
