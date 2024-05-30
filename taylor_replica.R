# Install necessary packages
install.packages(c(
  "writexl", "tm", "topicmodels", "reshape2", "ggplot2", "wordcloud", "pals", 
  "SnowballC", "lda", "ldatuning", "kableExtra", "DT", "flextable", 
  "stopwords", "textstem", "purrr", "reticulate", "textTinyR", "tidytext"
))

# Load required libraries
library(readr)
library(stringr)
library(readxl)
library(dplyr)
library(writexl)
library(tm)
library(topicmodels)
library(reshape2)
library(ggplot2)
library(wordcloud)
library(pals)
library(SnowballC)
library(lda)
library(ldatuning)
library(knitr)
library(kableExtra)
library(DT)
library(flextable)
library(purrr)
library(tidytext)
library(stopwords)
library(textstem)


# Load data
df <- read_excel("C:/Users/aylin/Downloads/fixed.xlsx")
colnames(df)[2] <- "title"

# Clean the lyrics column
df <- df %>%
  mutate(
    Lyrics = str_replace_all(Lyrics, "Contributors.*Lyrics", ""),
    Lyrics = str_replace_all(Lyrics, "\\d*Embed", ""),
    Lyrics = str_replace_all(Lyrics, "\\[.*?\\]", ""),
    Lyrics = str_replace_all(Lyrics, "^\\d+\\s*", ""),
    Lyrics = str_remove(Lyrics, "^Contributor")
  )

# Remove rows with NA values
df <- na.omit(df)

# Further cleaning and filtering
df_clean <- df %>%
  mutate(word_count = str_count(Lyrics, "\\S+")) %>%
  filter(word_count <= 2000) %>%
  select(-word_count) %>%
  mutate(
    total_chars = nchar(Lyrics),
    latin_chars = str_count(Lyrics, "[A-Za-z]"),
    proportion_latin = latin_chars / total_chars
  ) %>%
  filter(proportion_latin > 0.5) %>%
  select(-total_chars, -latin_chars, -proportion_latin)

tidy_data <- df_clean %>%
  unnest_tokens(word, Lyrics)

tidy_data

# Get stopwords
stop_words <- get_stopwords()
stop_words <- stopwords::stopwords("en")
stop_words <- c(stop_words, "I'm", "I'll", "ain't","??'m", "??'ll")
new_stop_words <- c('ooh','yeah','hey','whoa','woah', 'ohh', 'was', 'mmm', 'oooh','yah','yeh','mmm', 'hmm',
                    'deh','doh','and','yes', 'can', 'And', 'the', 'The', 'but', 'ayy', 'doo', 'let', 'Let', 'huh',
                    'dit', 'dat', 'wit', 'ain', 'll', 've', 're', 'isn', 't', 's', 'd', 'm', 'woah', 'uh', 'na',
                    "I'm", "I'll", "ain't", "??'m", "??'ll", "mai", "get", "sai","wai", "like", "it'","cau", "\u0131'll","\u0131t's","choo",
                    "\u0131'm", "\u0131've")
stop_words <- c(stopwords::stopwords("en"), new_stop_words)
stop_words_df <- tibble(word = stop_words)

# Anti-join stopwords
tidy_data <- tidy_data %>%
  anti_join(stop_words_df, by = "word")

# Exclude words with less than 3 characters
tidy_data <- tidy_data %>%
  filter(nchar(word) >= 3)
# Lemmatize words
tidy_data <- tidy_data %>%
  mutate(word = lemmatize_words(word))

# Count word frequency per decade
word_freq_year <- tidy_data %>%
  count(Year, word, sort = TRUE)






##TOPIC Model
lyrics_t <-
  tidy_data |>
  count (Year, word) |>
  filter(n>120) |>   #the frequency threshold
  cast_sparse(Year, word, n)

dim(lyrics_t)

####Addition
top_mod <- LDA(lyrics_t, k = 4, method = "VEM")

# Get top words for each topic
top_words <- tidy(top_mod, matrix = "beta")

# Visualize top words
top_words %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  arrange(topic, -beta) %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  labs(x = NULL, y = "Beta") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


#install.packages("stm")
library(stm)
set.seed(123)
topic_model <- stm(lyrics_t, K = 4, verbose = FALSE)
summary(topic_model)

df_clean <- df_clean %>%
  mutate(decade = paste0(substr(Year, 1, 3), "0s"))

lyrics_gamma <- tidy(
  topic_model, 
  matrix = "gamma",
  document_names = rownames(lyrics_t)
) 

lyrics_gamma %>%
  left_join(
    df_clean %>%
      select(decade, document = Year) %>%
      mutate(decade = paste0(substr(document, 1, 3), "0s"))
  ) %>%
  mutate(topic = factor(topic)) %>%
  ggplot(aes(gamma, topic, fill = topic)) +
  geom_boxplot(alpha = 0.7, show.legend = FALSE) +
  facet_wrap(vars(decade)) +
  labs(x = expression(gamma))


###topic effects
set.seed(123)

effects <-
  estimateEffect(
    1:4 ~ decade,
    topic_model,
    df_clean |> distinct(Year, decade) |> arrange(Year)
  )
tidy(effects) |> 
  filter(term != "(Intercept)", p.value < 0.05)
##evidence that there is more topic 2 from 80s, 90s,2000s, 2010s, and more topic 4 from 2000s and 2010s 
tidy(topic_model, matrix = "lift") |> 
  filter(topic == 1)
tidy(topic_model, matrix = "lift") |> 
  filter(topic == 2)
tidy(topic_model, matrix = "lift") |> 
  filter(topic == 4)
tidy(topic_model, matrix = "lift") |> 
  filter(topic == 3)
