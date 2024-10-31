BTM_Bigram
================
GV
2024-10-31

## R Markdown

### Import libraries

``` r
library(tm)
```

    ## Loading required package: NLP

``` r
library(NLP)
library(stringr)
library(topicmodels)
library(tidytext)
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ purrr     1.0.2
    ## ✔ forcats   1.0.0     ✔ readr     2.1.5
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ purrr::%||%()       masks base::%||%()
    ## ✖ ggplot2::annotate() masks NLP::annotate()
    ## ✖ dplyr::filter()     masks stats::filter()
    ## ✖ dplyr::lag()        masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(ggplot2)
library(dplyr)
library(SnowballC)
library(textstem)
```

    ## Loading required package: koRpus.lang.en
    ## Loading required package: koRpus
    ## Loading required package: sylly
    ## For information on available language packages for 'koRpus', run
    ## 
    ##   available.koRpus.lang()
    ## 
    ## and see ?install.koRpus.lang()
    ## 
    ## 
    ## Attaching package: 'koRpus'
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     tokenize
    ## 
    ## The following object is masked from 'package:tm':
    ## 
    ##     readTagged

``` r
library(reshape2)
```

    ## 
    ## Attaching package: 'reshape2'
    ## 
    ## The following object is masked from 'package:tidyr':
    ## 
    ##     smiths

``` r
library(readxl)
library(stringr)
```

### Import Dataset

``` r
data <- read_xlsx("Reddit_15mincity_Treated_Pass1.xlsx")
```

### Replace strange characters

# data$comment <- str_replace_all(df$comment, “\031”, “’”)

# data$comment <- str_replace_all(df$comment, “\034”, ” “)

# data$comment <- str_replace_all(df$comment, “\035”, ” “)

# data$comment <- str_replace_all(df$comment, “&gt”, ” “)

# data$comment <- str_replace_all(df$comment, “\*“,” “)

df \<- data.frame(data) \|\> filter(Filter == 1) \|\> \#filter by the
comments that were classified as relevant select(comment)

\#Normalize the term 15 minute city
df$comment <- str_replace_all(df$comment, “15 minute city”, “15mincity”)
df$comment <- str_replace_all(df$comment, “15 minute city”, “15mincity”)
df$comment <- str_replace_all(df$comment, “15-minute city”, “15mincity”)
df$comment <- str_replace_all(df$comment, “15-minute city”, “15mincity”)
df$comment <- str_replace_all(df$comment, “15-minute cities”,
“15mincity”) df$comment <- str_replace_all(df$comment, “15 min city”,
“15mincity”) df$comment <- str_replace_all(df$comment, “15 min cities”,
“15mincity”)

\#Remove punctuation text2 \<- gsub(pattern = “\W”, replace = ” “, df)
\#Take out urls text3 \<- str_replace_all(text2,”(http\S+)“,”“)
\#Lowercase words text4 \<- tolower(text3) \#remove single words text5
\<- gsub(pattern =”\b\[A-z\]\b{1}“, replace =” “, text4) \#Remove
whitespace text6 \<- stripWhitespace(text5) \#Lematize terms in its
dictionary form text7 \<- lemmatize_strings(text6, dictionary =
lexicon::hash_lemmas)

# Remove words for bigrams

adicional_stopwords \<- c(“don”, stopwords(“en”)) text_bigram \<-
removeWords(text7, adicional_stopwords)

head(text_bigram)

############################################################################# 

\#Bigrams

library(quanteda) library(igraph) library(ggraph) library(tidyverse)
library(tidyr) library(broom)

\#Create dataframe df_corpus \<- data.frame(text_bigram)

\#Create bigrams by separating words in sequences of 2. bigrams_df \<-
df_corpus %\>% unnest_tokens(output = bigram, input = text_bigram, token
= “ngrams”, n = 2)

\#Count bigrams bigrams_df %\>% count(bigram, sort = TRUE)

btm \<- bigrams_df %\>% count(bigram, sort = TRUE)

\#Remove stopwords in case it wasn’t in the beginning.
\#data(“stop_words”)

\#Separate words into two columns bigrams_separated \<- bigrams_df %\>%
separate(bigram, c(“word1”, “word2”), sep = ” “)

\#Remove stopwords bigrams_filtered \<- bigrams_separated %\>%
filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

\#Count the number of times two words are always together bigram_counts
\<- bigrams_filtered %\>% count(word1, word2, sort = TRUE)

\#Create network of bigrams

bigram_network \<- bigram_counts %\>% filter(n \> 11) %\>% \#filter for
the most common combinations of bigrams that appear at least 15 times.
graph_from_data_frame()

set.seed(2016)

a \<- grid::arrow(type = “closed”, length = unit(.06, “inches”))

ggraph(bigram_network, layout = “fr”) +  
geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a,
end_cap = circle(.03, ‘inches’)) + geom_node_point(color = “orange”,
size = 1) + geom_node_text(aes(label = name), vjust = 1, hjust = 0.3,
size = 3) + theme_void()

############################################################################## 

# Create biterm topic model

library(data.table) library(udpipe) library(dplyr) library(BTM)

\#Arrange document to format required for the udpipe

df_tm \<- mutate(df, doc_id =row_number())

colnames(df_tm) \<- c(“text”, “doc_id”)

df_tm \<- df_tm \|\> relocate(doc_id, text)

\#Data cleaning

\#Remove punctuation
df_tm$text <- gsub(pattern = "\\W", replace = " ", df_tm$text) \#Take
out urls df_tm$text <- str_replace_all(df_tm$text, “(http\S+)”, ““)
\#Lowercase words df_tm$text <- tolower(df_tm$text) \#remove single
words
df_tm$text <- gsub(pattern = "\\b[A-z]\\b{1}", replace = " ", df_tm$text)
\#Lematize terms in its dictionary form
df_tm$text <- lemmatize_strings(df_tm$text, dictionary =
lexicon::hash_lemmas)

adicional_stopwords \<- c(“don, wasn”, stopwords(“en”))

\#remove stopwords df_tm$text <- removeWords(df_tm$text,
adicional_stopwords)

df_tm$text <- str_replace_all(df_tm$text, “15 minute city”, “15mincity”)
df_tm$text <- str_replace_all(df_tm$text, “15 minute city”, “15mincity”)
df_tm$text <- str_replace_all(df_tm$text, “15-minute city”, “15mincity”)
df_tm$text <- str_replace_all(df_tm$text, “15-minute city”, “15mincity”)
df_tm$text <- str_replace_all(df_tm$text, “15-minute cities”,
“15mincity”) df_tm$text <- str_replace_all(df_tm$text, “15 min city”,
“15mincity”) df_tm$text <- str_replace_all(df_tm$text, “15 min cities”,
“15mincity”)

\#Remove whitespace df_tm$text <- stripWhitespace(df_tm$text)

biterm_data_tm \<- udpipe(df_tm, “english”, trace = 10) biterms \<-
as.data.table(biterm_data_tm) biterms \<- biterms\[, cooccurrence(x =
lemma, relevant = upos %in% c(“NOUN”, “ADJ”, “PROPN”) & !lemma %in%
stopwords(“en”), skipgram = 5), by = list(doc_id)\]

# Build BTM

set.seed(588) traindata \<- subset(biterm_data_tm, upos %in% c(“NOUN”,
“ADJ”, “PROPN”)) traindata \<- traindata\[, c(“doc_id”, “lemma”)\] model
\<- BTM(traindata, k = 5, beta = 0.01, iter = 1000, window = 15, biterms
= biterms, trace = 100)

## Inspect the model - topic frequency + conditional term probabilities

model\$theta

topicterms \<- terms(model, top_n = 25) topicterms \<-
data.frame(topicterms)

\#plot topicterms

library(textplot) library(ggraph) library(concaveman) plot(model, top_n
= 20, title =““, labels = paste(round(model\$theta \* 100, 2), “%”, sep
= ““))

plot(model, top_n = 25, title =““, labels = c(”Freedom of movement
(10.22 %)“,”Urban transition (21.41 %)“,”Choice and opportunity (38.29
%)“,”Explanation (13.43 %)“,”Apprehension and misinformation (16.65
%)“))

model\$theta

``` r
summary(cars)
```

    ##      speed           dist       
    ##  Min.   : 4.0   Min.   :  2.00  
    ##  1st Qu.:12.0   1st Qu.: 26.00  
    ##  Median :15.0   Median : 36.00  
    ##  Mean   :15.4   Mean   : 42.98  
    ##  3rd Qu.:19.0   3rd Qu.: 56.00  
    ##  Max.   :25.0   Max.   :120.00

## Including Plots

You can also embed plots, for example:

![](Btm_bigram_files/figure-gfm/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
