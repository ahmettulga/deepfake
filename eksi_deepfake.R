##############y??llara g??re post say??s??
eksi_deepfake$yil<-format(eksi_deepfake$tarih,format= "%Y")
table(eksi_deepfake$yil)
library(openxlsx)
post_yil<-data.frame(table(eksi_deepfake$yil))
write.xlsx(post_yil,"y??llar_post_eksi.xlsx")
#########aylara g??re post say??s??
eksi_deepfake$ay<-format(eksi_deepfake$tarih,format= "%m/%Y")
table(eksi_deepfake$ay)
post_ay_yil<-data.frame(table(eksi_deepfake$ay))
write.xlsx(post_ay_yil,"y??llar_aylar_post_eksi.xlsx")
#####URL
####URL analizi
library(stringr)

url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"

eksi_deepfake$ContentURL <- str_extract(eksi_deepfake$metin, url_pattern)

url_reddit<-data.frame(na.omit(eksi_deepfake$ContentURL))
library(openxlsx)
write.xlsx(url_reddit,"eksi_url_turkey.xlsx")
####temizleme
library(dplyr)
library(syuzhet)
library(plotly)
library(tm)
library(wordcloud)
table(eksi_deepfake$yil)
eksi_deepfake$yil<-format(eksi_deepfake$tarih,format= "%m/%Y")
nrc_vector <- get_sentiment(eksi_deepfake$metin, method = "nrc", lang = "turkish")
head(nrc_vector)
sum(nrc_vector)
sentiments <- data.frame(nrc_vector, eksi_deepfake$yil)
plot_ly(sentiments, x=~eksi_deepfake.yil, y=~nrc_vector, type="scatter", mode="jitter", name="syuzhet") %>%
  layout(title="Recent sentiments of HDB in Singapore",
         yaxis=list(title="score"), xaxis=list(title="date"))
######22 may??s 2023'de olumlu sonu??lar var
subat_2022<-eksi_deepfake%>%filter(yil=="02/2022")
write.table(eksi_deepfake$metin, file = "mtcars.txt", sep = "\t",
            row.names = FALSE)
########################sentiment analysis
isis<-gsub(pattern = "[[:digit:]]",replace=" ",eksi_deepfake$metin)
isis<-gsub(pattern = "[[:punct:]]",replace=" ",isis)
library(tm)
isis<-stripWhitespace(isis)
isis1<-tolower(isis)
library(syuzhet)
pkk_score<-get_nrc_sentiment(eksi_deepfake$metin,language = "turkish")
pkk_polarity<-pkk_score[,9:10]
pkk_sentiment<-pkk_score[,1:8]
pkk_polarity<-data.matrix(pkk_polarity,rownames.force = TRUE)
pkk_sentiment<-data.matrix(pkk_sentiment,rownames.force = TRUE)
barplot(pkk_polarity)
barplot(pkk_sentiment)
negative_items <- which(pkk_score$anger > 10)
s_v <- get_sentences(isis1)
eksi_deep_fear<-data.frame(s_v[negative_items])
####kelimeler aras?? korelasyon
TextDoc_dtm <- TermDocumentMatrix(isis1)
dtm_m <- as.matrix(TextDoc_dtm)
# Sort by descearing value of frequency
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
findAssocs(TextDoc_dtm, terms = c("deepfake"), corlimit = 0.50)
findAssocs(TextDoc_dtm, terms = findFreqTerms(TextDoc_dtm, lowfreq = 50), corlimit = 0.65)
######
terrorism<-readLines("terrorism_dic_tez.txt",warn=FALSE)
toks_1<-tokens(isis1)
stpo_w<-stopwords("turkish")
toks_3<-tokens_remove(toks_1,stpo_w)
###################
library(dplyr)
library(tidyverse)
library(tidytext)
library(lubridate)
tokenised_news_sample = eksi_deepfake %>% 
  unnest_tokens(output = word, input = metin) %>% 
  filter(!word %in% ref_StopWordListTR_1$a) %>% 
  filter(!str_detect(word, '[0-9]{1,}')) %>% 
  filter(nchar(word) > 2)
tokenised_news_sample %>% 
  group_by(word) %>% 
  tally() %>% 
  arrange(desc(n)) %>% head(20)  
tokenised_news_sample %>% 
  group_by(tarih, word) %>% 
  tally() %>% 
  arrange(tarih, desc(n)) %>% 
  group_by(tarih) %>% 
  top_n(5) %>% head(100)
tokenised_news_sample %>% 
  group_by(yil, word) %>% 
  tally() %>% 
  arrange(yil, desc(n)) %>% 
  group_by(yil) %>% 
  top_n(5)
tokenised_news_sample %>%
  filter(word == 'deepfake') %>% 
  group_by(yil, word) %>% 
  tally() %>% ggplot() + geom_col(aes(x = date, y = n))
tokenised_news_sample %>%
  filter(word %in% c('tehlike', 'tehdit','porno','cinsel','siyaset')) %>% 
  mutate(week = ymd(tarih))%>% 
  group_by(week, word) %>% 
  tally() %>% ggplot() + 
  geom_line(aes(x = week, y = n, color = word))
