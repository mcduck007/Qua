#http://chenangliu.info/en/high-frequency-words-in-toefl/
#

library(tm);
library(wordcloud);
#load a txt file
txt<-"E:\\TOEFL";
b<-Corpus(DirSource(txt),readerControl=list(language="eng"));
b<-tm_map(b,stripWhitespace);
b<-tm_map(b,removePunctuation);
b<-tm_map(b,tolower);
b<-tm_map(b,removeWords,c("and","the"));
b<-tm_map(b,removeWords,c("may","can"));
b<-tm_map(b,removeWords,c("also","often","one"));
b<-tm_map(b,removeWords,stopwords("english"));
tdm<-TermDocumentMatrix(b);
m1<-as.matrix(tdm);
v1<-sort(rowSums(m1),decreasing=TRUE);
d1<-data.frame(word =names(v1),freq=v1);
par(bg="lightyellow");
set.seed(10);
wordcloud(d1$word, d1$freq, scale=c(4,0.8),
          min.freq=6,max.words=100,
          col=rainbow(length(d1$freq)),font=2);