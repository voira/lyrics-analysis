# Load required libraries
library(readr)
library(stringr)
library(readxl)
library(dplyr)
library(writexl)
library(topicmodels)
library(reshape2)
library(ggplot2)
library(lda)
library(ldatuning)
library(tidytext)
library(stopwords)
library(textstem)
library(LDAvis)
library(tm)

# Set the locale to English
Sys.setlocale("LC_ALL", "en_US.UTF-8")

# Load data
df <- read_excel("fixed.xlsx")
colnames(df)[2] <- "title"

set.seed(123)

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
  unnest_tokens(word, Lyrics) %>%
  filter(word %in% c("??'m", "??s", "??'ll", "??", "??'ve") == FALSE) %>%
  mutate(word = tolower(word)) %>%
  mutate(word = lemmatize_words(word))

tidy_data

# Get the default stop words
stop_words <- stopwords::stopwords("en")

# Define additional stop words
new_stop_words <- c('ooh', 'yeah', 'hey', 'whoa', 'woah', 'ohh', 'was', 'mmm', 'oooh', 'yah', 'yeh', 'hmm',
                    'deh', 'doh', 'and', 'yes', 'can', 'And', 'the', 'the', 'but', 'ayy', 'doo', 'let', 'Let', 'huh',
                    'dit', 'dat', 'wit', 'ain', 'll', 've', 're', 'isn', 't', 's', 'd', 'm', 'woah', 'uh', 'na',
                    "I'm", "I'll", "ain't", "mai", "get", "sai", "wai", "like", "it'", "cau", 'one', 
                    'two', 'three', 'four', 'que', 'choo', 'chh', 'nah', 'boaw', 'ody', 'oop', 'ron', 'woo', 'aye', 
                    'wee', "get", "got", "have", "just", "wanna", "gotta", "y'all", "really", "even", "gon", "now")

# Combine and remove duplicates
combined_stop_words <- unique(c(stop_words, new_stop_words))

# Create a tibble
stop_words_df <- tibble(word = combined_stop_words)

# Anti-join stopwords
tidy_data <- tidy_data %>%
  anti_join(stop_words_df, by = "word")

# Exclude words with less than 3 characters
tidy_data <- tidy_data %>%
  filter(nchar(word) >= 3)

# Count word frequency per decade
word_freq_year <- tidy_data %>%
  count(Year, word, sort = TRUE)

# Define parameters
frequency_threshold <- 80

# Step 1: Count word frequencies by year
word_counts <- tidy_data %>%
  count(Year, word)

# Step 2: Filter out low-frequency words
filtered_counts <- word_counts %>%
  filter(n > frequency_threshold)

# Step 3: Cast filtered data into sparse matrix format
lyrics_t <- filtered_counts %>%
  cast_sparse(Year, word, n)

# Define parameters
num_topics <- 5
method <- "VEM"

# Fit LDA model
top_mod <- LDA(lyrics_t, k = num_topics, method = method)

# Get top words for each topic
top_words <- tidy(top_mod, matrix = "beta")

# Create the LDAvis visualization
vis <- createJSON(lda_model = top_mod, 
                  doc_topic_dists = top_mod@gamma,
                  vocab = top_mod@terms,
                  doc_lengths = doc.lengths)

# Save the visualization as an HTML file
serVis(vis, out.dir = "path/to/save")

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

topic_model <- stm(lyrics_t, K = 5, verbose = FALSE)
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
    1:5 ~ decade,
    topic_model,
    df_clean |> distinct(Year, decade) |> arrange(Year)
  )
tidy(effects) |> 
  filter(term != "(Intercept)", p.value < 0.05)
  
##evidence that there is more topic 2 from 80s, 90s,2000s, 2010s, and more topic 4 from 2000s and 2010s 
tidy(topic_model, matrix = "frex") |> 
  filter(topic == 1)
tidy(topic_model, matrix = "frex") |> 
  filter(topic == 4)
tidy(topic_model, matrix = "frex") |> 
  filter(topic == 5)
tidy(topic_model, matrix = "frex") |> 
  filter(topic == 3)
