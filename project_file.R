#setwd is used for setting up our project directory
setwd <- setwd("C:\\Users\\manth\\OneDrive\\Desktop\\R projects\\Sample project")

#returns the project directory
getwd()

#reading csv file using read.csv
data<-read.csv("Ola_Datasets//Ola_500.csv")

#converting csv to dataframe for performing operations on it
data<-data.frame(data)
#getting head of dataframe
head(data)
#getting column names of the dataframe
colnames(data)

#text cleaning
data_text=data$Text
data_text=tolower(data_text)

#gsub() function is used to replace all the matches of a pattern from a string. 
#If the pattern is not found the string will be returned as it is.
data_text <- data$Text
data_text <- tolower(data_text)
data_text <- gsub("rt", "", data_text)
data_text <- gsub("@\\w+", "", data_text)
data_text <- gsub("[[:punct:]]", "", data_text)
data_text <- gsub("http\\w+", "", data_text)
data_text <- gsub("[ |\t]{2,}", "", data_text)
data_text <- gsub("^ ", "", data_text)
data_text <- gsub(" $", "", data_text)



#Calls the NRC sentiment dictionary to calculate the presence of eight different emotions
library("syuzhet")
mysentiment_data <-get_nrc_sentiment((data_text))
print(data_text[1])
print(mysentiment_data[1,])
print(data_text[13])
print(mysentiment_data[13,])
print(data_text[115])
print(mysentiment_data[115,])
print(data_text[7])
print(mysentiment_data[7,])
print(data_text[301])
print(mysentiment_data[301,])
print(data_text[55])
print(mysentiment_data[55,])

Sentimentscores_data<-data.frame(colSums(mysentiment_data[,]))

#names() function in R Language is used to get or set the name of an Object.
names(Sentimentscores_data)<-"Score"

#The cbind function is used to combine vectors, matrices and/or data frames by columns.
Sentimentscores_data<-cbind("sentiment"=rownames(Sentimentscores_data),Sentimentscores_data)

#rownames() function in R Language is used to set the names to rows of a matrix.
rownames(Sentimentscores_data)<-NULL

Sentimentscores_data
#plotting a ggplot of scores vs sentiments where we try to analyze users emotions in a tweet
library("ggplot2")
ggplot(data=Sentimentscores_data,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people behind the tweets on OLA")

#get most positive tweet from available tweets
sents.value <- get_sentiment(data_text)
most.positive <- data_text[sents.value==max(sents.value)]
most.positive

#get most negative tweet from available tweets
most.negative <- data_text[sents.value<=min(sents.value)]
most.negative

sents.value
df <- data.frame(Tweets=data_text,score=sents.value)
head(df)
str(df)
View(df)
install.packages("SentimentAnalysis")
library(SentimentAnalysis)
x=c()
y=c()
z=c()

for(i in 1:nrow(df)) {
  #nrow(df)
  row <- df[i,1];
  if(row!="" && is.na(row)==FALSE){
  sentiment <- analyzeSentiment(row)
  x=append(x,row)
  y=append(y,signif(sentiment$SentimentQDAP,digits=2))
  temp=as.numeric(convertToDirection(analyzeSentiment(row)$SentimentQDAP))
  if(temp==1){
    z=append(z,"Negative")  
  }
  else if(temp==2){
    z=append(z,"Neutral")
  }
  else if(temp==3){
    z=append(z,"Positive")
  }
  }else{
    x=append(x,row)
    y=append(y,"")
    z=append(z,"")
  }
  print(i)
}
sentimentdata=data.frame("Tweets"=x,'Score'=y,"Sentiment"=z)
#View(sentimentdata)
#here we save tweets along with their category i.e positive or negative in a csv file
write.table(sentimentdata, "Ola_Tweets_Category//Ola_Category_500.csv", sep = ",", col.names = !file.exists("Ola_Tweets_Category//Ola_Category_500.csv"), append = T)
#write.csv(sentimentdata,"Ola_Tweets_Category//Ola_Category_500.csv",append = TRUE)
data <- read.csv("Ola_Tweets_Category//Ola_Category_500.csv")
View(data[1:50,])
