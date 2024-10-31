
# Webscrapping Reddit comments

library(RedditExtractoR)
library(tidyverse)

data <- find_thread_urls(subreddit = "NoStupidQuestions", sort_by = "top", keywords = "15 minute cities", period = "year") #

data <- data |> 
  filter(url %in% c("https://www.reddit.com/r/NoStupidQuestions/comments/1askcfh/why_do_some_americans_fear_15_minute_cities/")) 

#select the second thread

data_comments <- get_thread_content(data$url) 

view(data_comments$comments)


df <- data.frame(data_comments$comments) 

df <- df[,-c(1,4,6,7,8)]

library(writexl)
write_xlsx(df, "Reddit_15mincity.xlsx")


