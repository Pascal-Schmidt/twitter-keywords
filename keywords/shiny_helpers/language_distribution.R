c(
  "Undefined"             = "und",
  "English"               = "en",
  "Arabic"                = "ar",
  "Bengali"               = "bn",
  "Czech"                 = "cs",
  "Danish"                = "da",
  "German"                = "de",
  "Greek"                 = "el",
  "Spanish"               = "es",
  "Persian"               = "fa",
  "Finnish"               = "fi",
  "Filipino"              = "fil",
  "French"                = "fr",
  "Hebrew"                = "he",
  "Hindi"                 = "hi",
  "Hungarian"             = "hu",
  "Indonesian"            = "id",
  "Italian"               = "it",
  "Japanese"              = "ja",
  "Korean"                = "ko",
  "Malay"                 = "msa",
  "Dutch"                 = "nl",
  "Norwegian"             = "no",
  "Polish"                = "pl",
  "Portuguese"            = "pt",
  "Romanian"              = "ro",
  "Russian"               = "ru",
  "Swedish"               = "sv",
  "Thai"                  = "th",
  "Turkish"               = "tr",
  "Ukrainian"             = "uk",
  "Urdu"                  = "ur",
  "Vietnamese"            = "vi",
  "Chinese (Simplified)"  = "zh-cn",
  "Chinese (Traditional)" = "zh-tw"
) -> twitter_languages

twitter_languages %>%
  dplyr::bind_rows() %>%
  tidyr::pivot_longer(Undefined:`Chinese (Traditional)`, names_to = "lang_long") -> twitter_languages


language_distribution <- function(df) {
  
  df %>%
    dplyr::select(lang) %>%
    dplyr::count(lang, sort = TRUE) %>%
    dplyr::inner_join(twitter_languages, by = c("lang" = "value")) %>%
    dplyr::mutate(prop = round(n / sum(n), 4)) %>%
    dplyr::mutate(lang_long = forcats::fct_reorder(lang_long, n)) %>%
    dplyr::rename("Language" = lang_long) %>%
    ggplot(aes(x = Language, y = prop)) +
    geom_text(aes(label = paste0(n, " (", prop * 100, "%", ")"), y = prop + 0.2)) +
    geom_col() + 
    coord_flip() +
    expand_limits(y = 1.25) +
    xlab("") +
    ylab("") +
    theme_minimal() +
    theme(axis.title = element_blank(),
          axis.text.x = element_blank()) -> df
  
  return(df)
  
}