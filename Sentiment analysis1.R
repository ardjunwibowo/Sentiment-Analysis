library(tm)
library(wordcloud2)
library(twitteR)
library(rtweet)

# Ganti Sesuai dengan Key Milik Kita
consumer_key <- "aPfUiIcyJ557rmdUnHjH7g3nO"
consumer_secret <- "76PYEEDCt06trp7L3u9n5Q5xAZzQq5FNU3p9VWyPcBXCqytwx5"
access_token <- "99824379-FuI1pUdmzGSdE1vXuRm8kctBDl93HzQDXKzKudEj9"
access_secret <- "AFw1esD5Y9xJE3U56BvbSO9unNkkU1BKEXP4LNgpnH0Ji"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
tw = searchTwitter('jokowi + joko widodo', 
                   n = 10000,
                   retryOnRateLimit = 10e3)

View(tw)
##save dulu datanya
saveRDS(tw,file = 'tweet-mentah.rds')
##load datanya
tw <-readRDS('tweet-mentah.rds')
d = twListToDF(tw)
#remove(tw)

##visualisasi time series 
ts_plot(d, "1 hour") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of jokowi + joko widodo subianto Twitter statuses from past 1 Week",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

##lanjut ke asosiasi
## hanya ambil data tweet saja
komen <- d$text
komenc <- Corpus(VectorSource(komen))

##Cleaning data
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
twitclean <- tm_map(komenc, removeURL)

removeNL <- function(y) gsub("\n", " ", y)
twitclean <- tm_map(twitclean, removeNL)

replacecomma <- function(y) gsub(",", "", y)
twitclean <- tm_map(twitclean, replacecomma)

removeRT <- function(y) gsub("RT ", "", y)
twitclean <- tm_map(twitclean, removeRT)

removetitik2 <- function(y) gsub(":", "", y)
twitclean <- tm_map(twitclean, removetitik2)

removetitikkoma <- function(y) gsub(";", " ", y)
twitclean <- tm_map(twitclean, removetitikkoma)

removetitik3 <- function(y) gsub("p.", "", y)
twitclean <- tm_map(twitclean, removetitik3)

removeamp <- function(y) gsub("&amp;", "", y)
twitclean <- tm_map(twitclean, removeamp)

removeUN <- function(z) gsub("@\\w+", "", z)
twitclean <- tm_map(twitclean, removeUN)

remove.all <- function(xy) gsub("[^[:alpha:][:space:]]*", "", xy)
twitclean <- tm_map(twitclean,remove.all)


#Menghapus  titik koma, menjadi non kapital
twitclean <- tm_map(twitclean, removePunctuation)
twitclean <- tm_map(twitclean, tolower)
twitclean <- tm_map(twitclean , removeWords, 
                    c('indonesia','joko widodo','joko',
                      'widodo','pak','bpk'))



#Build a term-document matrix

{
  dtm <- TermDocumentMatrix(twitclean)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
}
head(d,n=10)


wordcloud2(d,shape = "star",
           backgroundColor = "white",
           color = 'random-light' ,size = 0.3)



## mencari asosiasi
v<-as.list(findAssocs(dtm,
                      terms= c('besan'),
                      corlimit= c(0.50,0.15,0.15,0.15,0.15,0.15,0.15)))
v


## save data
dataframe<-data.frame(text=unlist(sapply(twitclean, `[`)), stringsAsFactors=F)
View(dataframe)

write.csv(dataframe,file = 'twitclean-10kv8.csv')
dataframe[110,]

#PART 2

kalimat2<-read.csv("twitclean-10kv8.csv",header=TRUE)
#ambil kata kata untuk skoring
positif <- scan("s-pos.txt",what="character",comment.char=";")
negatif <- scan("s-neg.txt",what="character",comment.char=";")
kata.positif = c(positif)
kata.negatif = c(negatif)
score.sentiment = function(kalimat2, kata.positif, kata.negatif, .progress='none')
{
  require(plyr)
  require(stringr)
  scores = laply(kalimat2, function(kalimat, kata.positif, kata.negatif) {
    kalimat = gsub('[[:punct:]]', '', kalimat)
    kalimat = gsub('[[:cntrl:]]', '', kalimat)
    kalimat = gsub('\\d+', '', kalimat)
    kalimat = tolower(kalimat)
    
    list.kata = str_split(kalimat, '\\s+')
    kata2 = unlist(list.kata)
    positif.matches = match(kata2, kata.positif)
    negatif.matches = match(kata2, kata.negatif)
    positif.matches = !is.na(positif.matches)
    negatif.matches = !is.na(negatif.matches)
    score = sum(positif.matches) - (sum(negatif.matches))
    return(score)
  }, kata.positif, kata.negatif, .progress=.progress )
  scores.df = data.frame(score=scores, text=kalimat2)
  return(scores.df)
}

#melakukan skoring text
hasil = score.sentiment(kalimat2$text, kata.positif, kata.negatif)
head(hasil)

#CONVERT SCORE TO SENTIMENT
hasil$klasifikasi<- ifelse(hasil$score<0, "Negatif","Positif")
hasil$klasifikasi
View(hasil)


#Tukar Row
data <- hasil[c(3,1,2)]
View(data)
write.csv(data, file = "data ter labeli.csv")

#Memisahkan twit
data.pos <- hasil[hasil$score>0,]
View(data.pos)
write.csv(data.pos, file = "data-pos.csv")

data.neg <- hasil[hasil$score<0,]
View(data.neg)
write.csv(data.neg, file = "data-neg.csv")

