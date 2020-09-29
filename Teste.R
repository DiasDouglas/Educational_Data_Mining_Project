#Instalar
install.packages("tm")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("twitteR")

#Carregar
library("tm")
library("wordcloud")
library("RColorBrewer")
library("twitteR")

config <- read.csv("config_twitter.csv", header = T, sep = ';')

consumer_key <- config$API_KEY
consumer_secret <- config$API_SECRET_KEY
access_token <- config$ACCESS_TOKEN
access_secret <- config$ACCESS_TOKEN_SECRET

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

tweets <- searchTwitter("homeschooling", n=1000, lang="en")
#Convertendo os twittes para o formato de DF.
tweets <- twListToDF(tweets)
#Colapsando todos os twittes em um vetor de uma posição.
tweets_t <- paste(tweets$text, collapse = " ")

tweets_S <- VectorSource(tweets_t)
corpus <- Corpus(tweets_S)

#Coloca tudo em minúsculo
corpus <- tm_map(corpus, tolower)
#Remove pontuação
corpus <- tm_map(corpus, removePunctuation)
#Remove espaços extras em branco
corpus <- tm_map(corpus, stripWhitespace)
#Remove palavras ruído
corpus <- tm_map(corpus, removeWords, stopwords("en"))
# remove URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
corpus <- tm_map(corpus, removeURL)
# remove qualquer coisa que não seja letras em português e espaço.
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
corpus <- tm_map(corpus, content_transformer(removeNumPunct))

dtm <- TermDocumentMatrix(corpus)
dtm <- as.matrix(dtm)

fre <- sort(rowSums(dtm),decreasing=TRUE)

wordcloud(corpus, min.freq = 1, max.words=50,
          random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))



