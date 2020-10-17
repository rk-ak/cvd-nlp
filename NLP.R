# Import dataset
dataset_covid19_survey = read.csv('C:/Users/xxxxxx/Covid19 Survey/xxxxxx.csv')

# NLP Analysis
library(dplyr)
library(tidytext)
library(tm)
library(wordcloud2)
library(SentimentAnalysis)
library(syuzhet)
library(stringr)

# create row index
dataset_covid19_survey$Index <- as.numeric(rownames(dataset_covid19_survey))

# create text column
colnames(dataset_covid19_survey)[1] <- 'Raw'
#dataset_covid19_survey$Text <- ifelse((dataset_covid19_survey$Translation != ''), as.character(dataset_covid19_survey$Translation), 'NA')
#dataset_covid19_survey$Text <- str_replace(dataset_covid19_survey$Text,'Translation:  ', '')
#dataset_covid19_survey$Text <- ifelse(dataset_covid19_survey$Text == '', as.character(dataset_covid19_survey$Raw), 'NA')


# create a corpus - Vcorpus or simplecorpus
corpus <- VCorpus(VectorSource(dataset_covid19_survey$Text))

# preprocessing
# clean corpus
# 1. Replace or Removing numbers 
#corpus <- tm_map(corpus, replace_number)
corpus <- tm_map(corpus, removeNumbers)
# 2. Removing punctuation
corpus <- tm_map(corpus, removePunctuation)
# 3. Stripping any extra white space:
corpus <- tm_map(corpus, stripWhitespace)
# 4. Transforming everything to lowercase
corpus <- tm_map(corpus, content_transformer(tolower))
# 5. Removing stop words
# Add text subject words to new_stops
new_stops <- c("corona", "covid", "virus", "lockdown", stopwords("english"))
corpus <- tm_map(corpus, removeWords, new_stops)
# 6. Stemming
corpus <- tm_map(corpus, stemDocument)
# Cleaned corpus
dataset_covid19_survey$Clean_Text <- data.frame(text=unlist(sapply(corpus, '[','content')), 
                                                stringsasFactofs=F)$text


# create cleaned TDM
# creating tokenizers
Unigramtokenizer <- function(x)
  unlist(lapply(ngrams(words(x), 1), paste, collapse = " "), use.names = FALSE)
Bigramtokenizer <- function(x)
  unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
Trigramtokenizer <-function(x)
  unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)
# creating document matrix
unigram_clean_TDM <- TermDocumentMatrix(corpus, control=list(tokenize = Unigramtokenizer))
bigram_clean_TDM <- TermDocumentMatrix(corpus, control=list(tokenize = Bigramtokenizer))
trigram_clean_TDM <- TermDocumentMatrix(corpus, control=list(tokenize = Trigramtokenizer))
# set lowest frequencies
unigramf <- findFreqTerms(unigram_clean_TDM,lowfreq = 0)
bigramf <- findFreqTerms(bigram_clean_TDM,lowfreq = 0)
trigramf <- findFreqTerms(trigram_clean_TDM,lowfreq = 0)
# computing frequencies for n-grams
Unigramfreq <- rowSums(as.matrix(unigram_clean_TDM[unigramf,]))
Unigramfreq <- data.frame(word=names(Unigramfreq),frequency=Unigramfreq)
Bigramfreq <- rowSums(as.matrix(bigram_clean_TDM[bigramf,]))
Bigramfreq <- data.frame(word=names(Bigramfreq),frequency=Bigramfreq)
Trigramfreq <- rowSums(as.matrix(trigram_clean_TDM[trigramf,]))
Trigramfreq <- data.frame(word=names(Trigramfreq),frequency=Trigramfreq)
#convert TDM to DF
df_unigram_clean_TDM <- tidy(unigram_clean_TDM)
df_bigram_clean_TDM <- tidy(bigram_clean_TDM)
df_trigram_clean_TDM <- tidy(trigram_clean_TDM)

# sentiment analysis
#sntmnt_clean_DTM <- analyzeSentiment(clean_DTM, language = "english")
sntmnt_clean_DTM <- analyzeSentiment(as.character(dataset_covid19_survey$Clean_Text), language = "english", 
                                                  aggregate = NULL, removeStopwords = TRUE, stemming = TRUE)
sntmnt_clean_DTM <- as.data.frame(sntmnt_clean_DTM[,1:4])
dataset_covid19_survey$Wordcount_Clean_Text <- sntmnt_clean_DTM[,1]
dataset_covid19_survey$SentimentGI_Clean_Text <- sntmnt_clean_DTM[,2]
dataset_covid19_survey$NegativityGI_Clean_Text <- sntmnt_clean_DTM[,3]
dataset_covid19_survey$PositivityGI_Clean_Text <- sntmnt_clean_DTM[,4]

# summary sentiment analysis
summary(dataset_covid19_survey$SentimentGI_Clean_Text)

# get emotions
emtns_clean_text <- get_nrc_sentiment(as.character(dataset_covid19_survey$Clean_Text))
emtns_clean_text <- as.data.frame(emtns_clean_text[,1:10])
dataset_covid19_survey$Anger_Clean_Text <- emtns_clean_text[,1]
dataset_covid19_survey$Anticipation_Clean_Text <- emtns_clean_text[,2]
dataset_covid19_survey$Disgust_Clean_Text <- emtns_clean_text[,3]
dataset_covid19_survey$Fear_Clean_Text <- emtns_clean_text[,4]
dataset_covid19_survey$Joy_Clean_Text <- emtns_clean_text[,5]
dataset_covid19_survey$Sadness_Clean_Text <- emtns_clean_text[,6]
dataset_covid19_survey$Surprise_Clean_Text <- emtns_clean_text[,7]
dataset_covid19_survey$Trust_Clean_Text <- emtns_clean_text[,8]
dataset_covid19_survey$Negative_Clean_Text <- emtns_clean_text[,9]
dataset_covid19_survey$Positive_Clean_Text <- emtns_clean_text[,10]



# Export to csv
write.csv(dataset_covid19_survey, "dataset_covid19_survey_output.csv", row.names = F)
write.csv(df_unigram_clean_TDM, "df_unigram_clean_TDM.csv", row.names = F)
write.csv(df_bigram_clean_TDM, "df_bigram_clean_TDM.csv", row.names = F)
write.csv(df_trigram_clean_TDM, "df_trigram_clean_TDM.csv", row.names = F)
