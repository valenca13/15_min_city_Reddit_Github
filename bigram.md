Bigrams
================
GV
2024-10-31

### Import libraries

``` r
library(tm)
library(NLP)
library(stringr)
library(topicmodels)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(SnowballC)
library(textstem)
library(reshape2)
library(readxl)
library(stringr)
library(quanteda)
library(igraph)
library(ggraph)
library(tidyr)
library(broom)
```

### 1. Import Dataset

``` r
data <- read_xlsx("Reddit_15mincity_Treated_Pass1.xlsx")
```

#### Filter eligible comments from the database

``` r
df <- data.frame(data) |>
  filter(Filter == 1) |>   #filter by the comments that were classified as relevant
  select(comment)
```

### 2. Preprocessing

#### Normalize all the terms of the 15-minute city to “15mincity”

``` r
df$comment <- str_replace_all(df$comment, "15 minute city", "15mincity")
df$comment <- str_replace_all(df$comment, "15 minute city", "15mincity")
df$comment <- str_replace_all(df$comment, "15-minute city", "15mincity")
df$comment <- str_replace_all(df$comment, "15-minute city", "15mincity")
df$comment <- str_replace_all(df$comment, "15-minute cities", "15mincity")
df$comment <- str_replace_all(df$comment, "15 min city", "15mincity")
df$comment <- str_replace_all(df$comment, "15 min cities", "15mincity")
```

#### Standard preprocessing

``` r
#Remove punctuation
text2 <- gsub(pattern = "\\W", replace = " ", df)
#Take out urls
text3 <- str_replace_all(text2, "(http\\S+)", "")
#Lowercase words
text4 <- tolower(text3)
#remove single words 
text5 <- gsub(pattern = "\\b[A-z]\\b{1}", replace = " ", text4) 
#Remove whitespace
text6 <- stripWhitespace(text5)
#Lematize terms in its dictionary form
text7 <- lemmatize_strings(text6, dictionary = lexicon::hash_lemmas)
```

#### Remove specific stopwords for bigrams

``` r
adicional_stopwords <- c("don", stopwords("en"))
text_bigram <- removeWords(text7, adicional_stopwords)
```

### 3. Bigrams

#### Create dataframe

``` r
df_corpus <- data.frame(text_bigram)
```

#### Create bigram database by separating words in sequences of 2.

``` r
bigrams_df <- df_corpus %>%
  unnest_tokens(output = bigram,
                input = text_bigram,
                token = "ngrams",
                n = 2)
```

#### Count the number of bigrams

``` r
bigrams_df %>%
  count(bigram, sort = TRUE)

btm <- bigrams_df %>%
  count(bigram, sort = TRUE)
```

\#Remove stopwords in case it wasn’t in the beginning.
\#data(“stop_words”)

#### Separate words into two columns

``` r
bigrams_separated <- bigrams_df %>%
  separate(bigram, c("word1", "word2"), sep = " ")
```

#### Remove stopwords

``` r
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
```

#### Count the number of times two words are always together

``` r
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)
```

#### Plot the bigram graph network

``` r
bigram_network <- bigram_counts %>%
  filter(n > 11) %>% #filter for the most common combinations of bigrams that appear at least 15 times.
  graph_from_data_frame()

set.seed(2016)

a <- grid::arrow(type = "closed", length = unit(.06, "inches"))

ggraph(bigram_network, layout = "fr") +   
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.03, 'inches')) +
  geom_node_point(color = "orange", size = 1) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 0.3, size = 3) +
  theme_void()
```

![](Btm_bigram_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->
