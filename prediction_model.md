---
title: "Prediction Model"
author: "Aashay Sharma"
date: "21/07/2020"
output: 
        html_document:
          keep_md: true
---

## This markdown file consists of the model file as well as describes how exactly this model works.

## The Data cleaning and Exploratory analysis Tasks were done in a separate file (Milestone Report) and the code for the same is on the github page : https://github.com/aashay15/DS-Capstone

## After a bit of exploratory analysis and looking at the histograms and all, I figured out that for word prediction we can use the text or the phrase entered by the user to find out a matching bi-gram, trigram or quadgram and we will return the next word which will have the highest frequency in our data from all the ngrams, this way we have a decent prediction as well as the task would not be so computer intensive that is; we will have faster prediction.

## This Machine Learning Model was finalised by me, after looking at many other projects and their approaches which helped me out a lot in my project. 

## This model is very basic and just predicts based on the highest frequency but if we look at the predictions they are decently accurate and predicts many words correctly which will be depicted at the bottom section of this file.

## I have just worked on a very basic note on this model and thus there are many ways the model can be optimised further by fitting a larger data and a more randomised approach, but intially this also works and works just fine for a basic level predictions.

## The stored data was loaded from the RDS files :

```r
library(tidyverse)
library(stringr)
```


```r
bi_words <- readRDS("/Users/aashaysharma/Desktop/RStudio/DS-3/RDS/bi_words.rds")
tri_words <- readRDS("/Users/aashaysharma/Desktop/RStudio/DS-3/RDS/tri_words.rds")
quad_words <- readRDS("/Users/aashaysharma/Desktop/RStudio/DS-3/RDS/quad_words.rds")
```

## Functions to get the most frequent NGrams :


```r
#The function to get the most frequent bigrams
bigram <- function(input_words){
  num <- length(input_words)
  filter(bi_words, 
         word1==input_words[num]) %>% 
    top_n(1) %>%
    filter(row_number() == 1L) %>%
    select(num_range("word", 2)) %>%
    as.character() -> out
  ifelse(out =="character(0)", "?", return(out))
}

#The function to get the most frequent trigrams
trigram <- function(input_words){
  num <- length(input_words)
  filter(tri_words, 
         word1==input_words[num-1], 
         word2==input_words[num])  %>% 
    top_n(1) %>%
    filter(row_number() == 1L) %>%
    select(num_range("word", 3)) %>%
    as.character() -> out
  ifelse(out=="character(0)", bigram(input_words), return(out))
}

#The function to get the most frequent quadgrams
quadgram <- function(input_words){
  num <- length(input_words)
  filter(quad_words, 
         word1==input_words[num-2], 
         word2==input_words[num-1], 
         word3==input_words[num])  %>% 
    top_n(1) %>%
    filter(row_number() == 1L) %>%
    select(num_range("word", 4)) %>%
    as.character() -> out
  ifelse(out=="character(0)", trigram(input_words), return(out))
}
```

## Ngrams prediction function which takes the input and formats it out in usable format for the functions.

```r
predict <- function(input){
  # Create a dataframe
  input <- tibble(text = input)
  # Clean the Inpput
  replace_reg <- "[^[:alpha:][:space:]]*"
  input <- input %>%
    mutate(text = str_replace_all(text, replace_reg, ""))
  # Find word count, separate words, lower case
  input_count <- str_count(input, boundary("word"))
  input_words <- unlist(str_split(input, boundary("word")))
  input_words <- tolower(input_words)
  # Call the matching functions
  out <- ifelse(input_count == 1, bigram(input_words), 
                ifelse (input_count == 2, trigram(input_words), quadgram(input_words)))
  # Output
  return(out)
}
```

# Predictions made by this logic:


```r
predict("mothers")
```

```
## [1] "day"
```


```r
predict("happy")
```

```
## [1] "birthday"
```


```r
predict("barack")
```

```
## [1] "obama"
```


```r
predict("This is united")
```

```
## [1] "states"
```


```r
predict("This is united states of")
```

```
## [1] "america"
```


### I would further work on the model for more accuracy and would update the same.
