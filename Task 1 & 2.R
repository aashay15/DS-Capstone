
# Task 1 Getting and Cleaning Data 
# Loading the data set and cleaning it.

news <- readLines("/Users/aashaysharma/Desktop/RStudio/DS-Capstone/final/en_US/en_US.news.txt")

blogs <- readLines("/Users/aashaysharma/Desktop/RStudio/DS-Capstone/final/en_US/en_US.blogs.txt")

twitter2 <- readLines("/Users/aashaysharma/Desktop/RStudio/DS-Capstone/final/en_US/en_US.twitter.txt")


#Generating Random Sample
random_twitter2 <- ceiling(runif(n=6000, min=0, max=length(twitter2)))
random_news <- ceiling(runif(n=5000, min=0, max=length(news)))
random_blogs <- ceiling(runif(n=5000, min=0, max=length(blogs)))

twitter2 <- twitter2[random_twitter2]
blogs <- blogs[random_blogs]
news <- news[random_news]

#Converting the file encoding to better suit the tokenizers
twitter2 <- iconv(twitter2, "UTF-8", "ASCII", sub="")
news <- iconv(news, "UTF-8", "ASCII", sub="")
blogs <- iconv(blogs, "UTF-8", "ASCII", sub="")


#Creating a VCorpus by combining all the three random samples
myCorpus <- VCorpus(VectorSource(c(twitter2, blogs, news)))

#Cleaning the corpus using some built in funtions 
#Removing Numbers, Punctuation, White Spaces and converting to lowercase
myCorpus <- tm_map(myCorpus, removeNumbers)
myCorpus <- tm_map(myCorpus, removePunctuation)
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
myCorpus <- tm_map(myCorpus, stripWhitespace)

#Removing english StopWords (Eg: the, of, and etc..) and genrating a new corpus
myCorpus_nostop <- tm_map(myCorpus, removeWords, stopwords("en"))


#Using Google's Badwords file from code archive to make a data containing 
#badwords so that we can remove profanity (Badwords) from both the corpus
badwords <- readLines("/Users/aashaysharma/Desktop/badwords.txt")
badwords <- removePunctuation(badwords)
badwords <- removeNumbers(badwords)


#Removing Profanity (Badwords) fromt both the corpus
myCorpus <- tm_map(myCorpus, removeWords, badwords)
myCorpus_nostop <- tm_map(myCorpus_nostop, removeWords, badwords)

#Initiating the tokenizing functions
unigram <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 1))}
bigram <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 2, max = 2))}
trigram <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 3, max = 3))}
quadgram <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 4, max = 4))}


#Applying the tokenizers to the corpus with stopwords and the converting it into
#TermDocumentMatrix 
tdmUnigram <- TermDocumentMatrix(myCorpus, control = list(tokenize = unigram))
tdmBigram <- TermDocumentMatrix(myCorpus, control = list(tokenize = bigram))
tdmTrigram <- TermDocumentMatrix(myCorpus, control = list(tokenize = trigram))
tdmQuadgram <- TermDocumentMatrix(myCorpus, control = list(tokenize = quadgram))

#Applying the same to corpus containing no stopwords
tdmUnigram_no <- TermDocumentMatrix(myCorpus_nostop, control = list(tokenize = unigram))
tdmBigram_no <- TermDocumentMatrix(myCorpus_nostop, control = list(tokenize = bigram))
tdmTrigram_no <- TermDocumentMatrix(myCorpus_nostop, control = list(tokenize = trigram))

# Calculate Frequencies and sort descending
unigramFreq <- sort(rowSums(as.matrix(tdmUnigram)), decreasing = T)
bigramFreq2 <- sort(rowSums(as.matrix(tdmBigram2)), decreasing = T)
trigramFreq <- sort(rowSums(as.matrix(tdmTrigram)), decreasing = T)
quadgramFreq <- sort(rowSums(as.matrix(tdmQuadgram)), decreasing = T)

unigramFreq_no <- sort(rowSums(as.matrix(tdmUnigram_no)), decreasing = T)
bigramFreq2_no <- sort(rowSums(as.matrix(tdmBigram2_no)), decreasing = T)
trigramFreq_no <- sort(rowSums(as.matrix(tdmTrigram_no)), decreasing = T)


#Making Data frames out of the frequency data we got from tokenizing 
#This data sets will be further used to save in RDS format and then will be
#loaded into another file for model building purposes
uni_freq <- data.frame(word = names(unigramFreq), freq = unigramFreq)
bi_freq <- data.frame(word = names(bigramFreq2), freq = bigramFreq2)
tri_freq <- data.frame(word = names(trigramFreq), freq = trigramFreq)
quad_freq <- data.frame(word = names(quadgramFreq), freq = quadgramFreq)

uni_freq_no <- data.frame(word = names(unigramFreq_no), freq = unigramFreq_no)
bi_freq_no <- data.frame(word = names(bigramFreq2_no), freq = bigramFreq2_no)
tri_freq_no <- data.frame(word = names(trigramFreq_no), freq = trigramFreq_no)

#Getting the top 20 most frequent ngrams for plotting purposes
uni_freq_20 <- uni_freq[1:20,]
bi_freq_20 <- bi_freq[1:20,]
tri_freq_20 <- tri_freq[1:20,]

uni_freq_no_20 <- uni_freq_no[1:20,]
bi_freq_no_20 <- bi_freq_no[1:20,]
tri_freq_no_20 <- tri_freq_no[1:20,]


# Bar Plots
g_uni <- ggplot(data = uni_freq_20, aes(reorder(word,freq), freq)) + geom_bar(stat = "identity") + coord_flip() + xlab("Uni Grams")
  
g_bi <- ggplot(data = bi_freq_20, aes(reorder(word,freq), freq)) + geom_bar(stat = "identity") + coord_flip() + xlab("Bi Grams")

g_tri <- ggplot(data = tri_freq_20, aes(reorder(word,freq), freq)) + geom_bar(stat = "identity") + coord_flip() + xlab("Tri Grams")

grid.arrange(g_uni, g_bi, g_tri, nrow = 1)

# Bars Plots No Stop words

g_uni_no <- ggplot(data = uni_freq_no_20, aes(reorder(word,freq), freq)) + geom_bar(stat = "identity") + coord_flip() + xlab("Uni Grams")

g_bi_no <- ggplot(data = bi_freq_no_20, aes(reorder(word,freq), freq)) + geom_bar(stat = "identity") + coord_flip() + xlab("Bi Grams")

g_tri_no <- ggplot(data = tri_freq_no_20, aes(reorder(word,freq), freq)) + geom_bar(stat = "identity") + coord_flip() + xlab("Tri Grams")

grid.arrange(g_uni_no, g_bi_no, g_tri_no, nrow = 1)

sep <- tri_freq

tri_words <- sep %>% separate(word, c("word1", "word2", "word3"), " ")

bi_words <- bi_freq %>% separate(word, c("word1", "word2"), " ")

quad_words <- quad_freq %>% separate(word, c("word1", "word2", "word3", "word4"))


#Word Clouds 

#### UNIGRAM
wordcloud(words = uni_freq$word, freq = uni_freq$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
#### BIGRAM
wordcloud(words = bi_freq$word, freq = bi_freq$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
#### TRIGRAM
wordcloud(words = tri_freq$word, freq = tri_freq$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))

#### WordCloud for the corpus **NOT** containing stop words:

#### UNIGRAM
wordcloud(words = uni_freq_no$word, freq = uni_freq_no$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
#### BIGRAM
wordcloud(words = bi_freq_no$word, freq = bi_freq_no$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
#### TRIGRAM
wordcloud(words = tri_freq_no$word, freq = tri_freq_no$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))

#The code for model building will be in another file
            
                    
