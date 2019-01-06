## install rtweet from CRAN
## Vignette: https://cran.r-project.org/web/packages/rtweet/rtweet.pdf
## instructional: https://www.springboard.com/blog/machine-learning-with-r/

## 2018-10-28

# install.packages("rtweet")
# install.packages("caret")
# install.packages("tm")
# install.packages("kernlab")
# install.packages("dplyr")
# install.packages("splitstackshape")
# install.packages("stringr")
# install.packages("e1071")
# install.packages("magrittr")

## load packages
library(rtweet)
library(caret)
library(tm)
library(kernlab)
library(dplyr)
library(splitstackshape)
library(stringr)
library(e1071)
library(magrittr)

# Getting Data from Twitter -----------------------------------------------

# skaber token
consumer_key <- readLines("Keychain/consumer_key.txt", warn=FALSE)
consumer_secret <- readLines("Keychain/secret_key.txt", warn=FALSE)

token <- create_token(
  app = "Helge",
  consumer_key = consumer_key,
  consumer_secret = consumer_secret)

# brugbare variable og regex
user <- "MHGottlieb"
reg <- c(emoji = "\\p{So}|\\p{Cn}", nl = "[\n]+", 
         link = " ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)")

#Henter likes fra twitter og frasorterer irrelevant info
favs <- get_favorites(user, n=3000)[c(1:5, 15,29:30,85)]
favs$emojis <- str_match_all(favs$text, reg["emoji"])
favs$text <- gsub(paste(reg, collapse="|"), "", favs$text, perl=TRUE)

tweets <- get_my_timeline(user=user, n=1000)[c(1:5, 15,29:30,85)]
tweets$emojis <- str_match_all(tweets$text, reg["emoji"])
tweets$text <- gsub(paste(reg, collapse="|"), "", tweets$text, perl=TRUE)

sum(tweets[tweets$lang=="da",]) # HER ER JEG KOMMET TIL I 

#Hacky-hurtigt loop til at downloade større mængde tweets fra min tidslijne
#... Så jeg har noget empiri at tage udgangspuntk i:

#data <- NULL

while(Sys.time()<1541360800){
  temp <- get_my_timeline(user=user, n=3200)[c(1:5, 15,29:30,85)]
  data <- unique(rbind(data, temp))
  message(paste0(nrow(data), " unikke tweets hentet"))
  save(data, file="tweets.RData")
  remove(temp)
  Sys.sleep(1800)
}


#Timeline for reference in traning (negative cases)
timeline <- get_my_timeline(user=user, n=1000)[c(1:5, 15,29:30,85)]
timeline <- timeline[which(timeline$lang %in% c("da", "en")),]
timeline$emojis <- str_match_all(timeline$text,"\\p{So}|\\p{Cn}") #udskiller emojis
timeline$text <- gsub("\\p{So}|\\p{Cn}| ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", timeline$text, perl=TRUE) #fjerner emojis og links fra tekst.

timeline_dk <- timeline[timeline$lang=="da",]
favs_dk <- favs[favs$lang=="da",]

remove(favs, timeline, consumer_key, consumer_secret, user)

# Opretter og traener modellen --------------------------------------------

#Training-set, bestaaende af...
ntrain <- nrow(favs_dk)
train_dk <- sample_n(favs_dk, ntrain-100) # et antal tilfældige tweets, jeg har liket
train_dk <- rbind(train_dk, sample_n(timeline_dk, nrow(timeline_dk)/2)) #...og et antal tilfældige (danske) tweets
train_dk$corpus <- c(rep("like", ntrain-100), rep("not like", nrow(timeline_dk)/2)) # Classifiers
train_dk$corpus <- as.factor(train_dk$corpus)

#Test-set, bestående af...
test_dk <- favs_dk[which(!favs_dk$status_id %in% train_dk$status_id),] #tweets, jeg ikke har liket
test_like <- nrow(test_dk)
test_dk <- rbind(test_dk, timeline_dk[which(!timeline_dk$status_id %in% train_dk$status_id),]) # tilfældige tweets
test_dk$corpus <- c(rep("like", test_like), rep("not like", nrow(test_dk)-test_like))
test_dk$corpus <- as.factor(test_dk$corpus)

#Rydder op (Turn down for what?)
remove(favs_dk, timeline_dk, test_like)

train <- VCorpus(VectorSource(train_dk$text), readerControl=list(language="Danish"))
train <- tm_map(train, content_transformer(stripWhitespace))
train <- tm_map(train, content_transformer(removeNumbers))
train <- tm_map(train, content_transformer(removePunctuation))

test <- VCorpus(VectorSource(test_dk$text), readerControl=list(language="Danish"))
test <- tm_map(test, content_transformer(stripWhitespace))
test <- tm_map(test, content_transformer(removeNumbers))
test <- tm_map(test, content_transformer(removePunctuation))

train.dtm <- as.matrix(DocumentTermMatrix(train, control=list(wordLengths=c(1,Inf))))
test.dtm <- as.matrix(DocumentTermMatrix(test, control=list(wordLengths=c(1,Inf))))

train.df <- data.frame(train.dtm[,intersect(colnames(train.dtm), colnames(test.dtm))])
test.df <- data.frame(test.dtm[,intersect(colnames(test.dtm), colnames(train.dtm))])

train.df$corpus <- train_dk$corpus
test.df$corpus <- test_dk$corpus

df.train <- train.df
df.test <- test.df

type <- c("C-svc", "nu-svc", "C-bsvc")
kernels <- c("rbfdot", "polydot", "vanilladot", "tanhdot", "laplacedot", 
             "besseldot", "anovadot")

cms <- NULL
acc <- NULL
h <- 1

for(i in 1:length(type)){
  for(y in 1:length(kernels)){
      df.model <- ksvm(corpus~., data= df.train, kernel=kernels[y], type=type[i])
      df.pred <- predict(df.model, df.test)
      con.matrix <- confusionMatrix(df.pred, df.test$corpus)
      message(paste(i, "/", y, type[i], "/ kernel:", kernels[y]))
      print(con.matrix[2])
      cms <- append(cms, paste(type[i], kernels[y]))
      acc <- append(acc, con.matrix[3][[1]][1])
  }
}

out <- cbind(cms, acc)
out <- out[order(out[,2]),] #C-svc polydot og C-svc vanilladot lader til at være lead contesters

df.model <- ksvm(corpus~., data= df.train, kernel="polydot", type="C-svc")
df.pred <- predict(df.model, df.test)
con.matrix <- confusionMatrix(df.pred, df.test$corpus)
print(con.matrix)

df.model <- ksvm(corpus~., data= df.train, kernel="vanilladot", type="C-svc")
df.pred <- predict(df.model, df.test)
con.matrix <- confusionMatrix(df.pred, df.test$corpus)
print(con.matrix)

#faktisk er de nøjagtigt ens!

