################################################################# Preamble
library(cluster)
library(fpc)
library(tm)
library(koRpus)
library(RTextTools)
library(topicmodels)
library(e1071)
library(klaR)
library(randomForest)

#################################################################### Question 1
#Introducing the Data
DATA <- read.csv("~/AAAWarwick/AAData Mining/Assignments/Lab Assignments/Final Assignment/reutersCSV.csv")
text<-DATA[,139:140] #text columns
numeric<-DATA[,4:138] #label columns
NoZero<- DATA[which(rowSums(DATA[,4:138]) > 0),-(1:3)] #Remove all rows not assigned to a topic
NoZero1<-NoZero[,order(colSums(NoZero[1:135]), decreasing=TRUE)] #Order the topics from majority to minority

#Remove all duplicate classifications for each document.
for(i in 1:nrow(NoZero1)){
  for(j in 1:135) {
    if(NoZero1[i,j] == 1){
      NoZero1[i,-j] = 0
    }
  }
}
Data<-NoZero1 # All remaining documents allocated only to it's majority class # 9028 remaining rows
rowSums(Data) #Check to see all rowSums are 1
Data<-data.frame(Data, NoZero[,136:137]) #Building a dataframe with all the original columns

print(colnames(Data[,1:10]))#Print the ten largest colSums
PreprocessedTop10<-data.frame(Data[,(1:10)], NoZero[,136:137]) #Build a new dataframe containing only the majority ten classes and text columns
colnames(PreprocessedTop10)<-c("Earn","Acqisitions","MoneyFX","Crude","Grain","Trade","Interest","Wheat","Ship","Corn","Title","Text")
DataTop10<- PreprocessedTop10[which(rowSums(PreprocessedTop10[,1:10]) > 0),] #Removes all nonzero Values in the top ten

#Corpus Preprocessing in Data title
  corpus <- Corpus(VectorSource(Data$doc.title), readerControl=list(language="en")) 
  corpus <- tm_map(corpus, tolower) # converts all letters to lower case
  corpus <- tm_map(corpus, removePunctuation) #Removes Punctuation
  corpus <- tm_map(corpus, removeWords, stopwords("english")) # Removes all stop words
  corpus <- tm_map(corpus, stripWhitespace) # Removes all white space
  corpus <- tm_map(corpus, removeNumbers) # Removes 0-9
  corpus <- tm_map(corpus, stemDocument) # Remove Stem Words # 11340 documents remaining
  corpus <- tm_map(corpus, PlainTextDocument) # error with tm 0.6.0 tolower makes it a non document
  Title<-data.frame(text=unlist(sapply(corpus, `[`, "content")), stringsAsFactors=F)
DataCorpus<-data.frame(Data[1:135], Title)

#Corpus Preprocessing in Data text
  corpus <- Corpus(VectorSource(Data$doc.text), readerControl=list(language="en")) 
  corpus <- tm_map(corpus, tolower) # converts all letters to lower case
  corpus <- tm_map(corpus, removePunctuation) #Removes Punctuation
  corpus <- tm_map(corpus, removeWords, stopwords("english")) # Removes all stop words
  corpus <- tm_map(corpus, stripWhitespace) # Removes all white space
  corpus <- tm_map(corpus, removeNumbers) # Removes 0-9
  corpus <- tm_map(corpus, stemDocument) # Remove Stem Words # 11340 documents remaining
  corpus <- tm_map(corpus, PlainTextDocument) # error with tm 0.6.0 tolower makes it a non document
  Text<-data.frame(text=unlist(sapply(corpus, `[`, "content")), stringsAsFactors=F)
Data<-data.frame(DataCorpus[1:136], Text)
Data <- Data[!apply(Data, 1, function(x) any(x=="")),] #removes all documents with no text #11340 remaining instances

#Corpus Preprocessing in DataTop10 title
corpus <- Corpus(VectorSource(DataTop10$Title), readerControl=list(language="en")) 
corpus <- tm_map(corpus, tolower) # converts all letters to lower case
corpus <- tm_map(corpus, removePunctuation) #Removes Punctuation
corpus <- tm_map(corpus, removeWords, stopwords("english")) # Removes all stop words
corpus <- tm_map(corpus, stripWhitespace) # Removes all white space
corpus <- tm_map(corpus, removeNumbers) # Removes 0-9
corpus <- tm_map(corpus, stemDocument) # Remove Stem Words # 11340 documents remaining
corpusTop10 <- tm_map(corpus, PlainTextDocument) # error with tm 0.6.0 tolower makes it a non document
Title<-data.frame(text=unlist(sapply(corpusTop10, `[`, "content")), stringsAsFactors=F)
DataTop10Corpus<-data.frame(DataTop10[1:10], Title)

#Corpus Preprocessing in DataTop10 text
corpus <- Corpus(VectorSource(DataTop10$Text), readerControl=list(language="en")) 
corpus <- tm_map(corpus, tolower) # converts all letters to lower case
corpus <- tm_map(corpus, removePunctuation) #Removes Punctuation
corpus <- tm_map(corpus, removeWords, stopwords("english")) # Removes all stop words
corpus <- tm_map(corpus, stripWhitespace) # Removes all white space
corpus <- tm_map(corpus, removeNumbers) # Removes 0-9
corpus <- tm_map(corpus, stemDocument) # Remove Stem Words # 11340 documents remaining
corpusTop10 <- tm_map(corpus, PlainTextDocument) # error with tm 0.6.0 tolower makes it a non document
Text<-data.frame(text=unlist(sapply(corpusTop10, `[`, "content")), stringsAsFactors=F)
DataTop10<-data.frame(DataTop10[1:11], Text)
DataTop10 <- DataTop10[!apply(DataTop10, 1, function(x) any(x=="")),]  #removes all documents with no text #8599 remaining documents
colnames(DataTop10)<-c("Earn","Acqisitions","MoneyFX","Crude","Grain","Trade","Interest","Wheat","Ship","Corn","Title","Text")

array<-colnames(DataTop10)
for(i in 1:nrow(DataTop10)){
  for(j in 1:10) {
    if(DataTop10[i,j] == 1){
      DataTop10$Topic[i]<-(array[j])
    }
  }
}

DataTop10$Title<-gsub("said", "", DataTop10$Title) # Removing non Latin Alphabet words
DataTop10$Title<-gsub("reuter", "", DataTop10$Title) # Removing non Latin Alphabet words
DataTop10$Text<-gsub( "said", "", DataTop10$Text) # Removing non Latin Alphabet words
DataTop10$Text<-gsub("reuter", "", DataTop10$Text) # Removing non Latin Alphabet words

View(Data) # Preprocessed Data with all topics
View(DataTop10) # Preprocessed Data with only the top 10 manually allocated topics

##################################################################### Question 2

LDASETUP<-create_matrix(cbind((as.vector(DataTop10$Title)),as.vector(DataTop10$Text)),language="english", removeNumbers=TRUE, stemWords=TRUE)
lda<-LDA(LDASETUP, 10)
k <- length(unique(DataTop10$Topic))
terms(lda,10) #10 most likely terms per topic
Test$LDA<-topics(lda) # 3 most likely topics per document
Test$Actual<-DataTop10$Topic #For comparison # Test Accuracy

corpus<-DataTop10[,12]

#Training Data
corpus<-Corpus(VectorSource(corpus))
corpus<- DocumentTermMatrix(corpus,control=list(weighting=weightTfIdf, minWordLength=2, minDocFreq=5))
dim(corpus)
colnames(corpus)[10:100]
inspect(corpus[1:100,101:110])
findFreqTerms(corpus,10)
corpusdataframetrain <- as.data.frame(inspect(corpus)) #Moving into dataframe for classification in R
rownames(corpusdataframetrain)<- 1:nrow(corpus) # Naming each document
corpusdataframetrain$billion[180:200] #Testing the dataframe to see that it has worked with a sample
ClassTrain<-DataTop10$Topic
corpusdataframetrain <- cbind(corpusdataframetrain, ClassTrain)
A<-corpusdataframetrain


####################################################################### Question 3
corpusdataframetrain$folds <- sample(1:10, nrow(corpusdataframetrain), replace = TRUE)
for (i in 1:10){
  Train<-corpusdataframetrain[corpusdataframetrain$folds==i,]
  Test<-corpusdataframetrain[corpusdataframetrain$folds==-(i),]
  } 
NB<-naiveBayes(as.factor(ClassTrain) ~., data=Train,importance=TRUE,ntree=20)# Naive Bayes #Didn't work, too many variables.
NB<-naiveBayes(as.factor(ClassTrain) ~  billion+mln+dollar+bank+cts+company+share+oil+trade+year+net+rate+pct+dividend+dlrs+tonne+state+bank+reuter+sale+will+pct+loss+market+rate+april+offer+gas+price+japan+currency+will+official+japanese+profit+franc+quarter+unit+inc+export+reagan+import+qtr+central+year+div+will+mln+price+crude+surplus+yen+loan+pay+sell+group+year+opec+agriculture,data=Train,importance=TRUE,ntree=2000)# Naive Bayes 
RF <- randomForest(as.factor(ClassTrain)~ billion+mln+dollar+bank+cts+company+share+oil+trade+year+net+rate+pct+dividend+dlrs+tonne+state+bank+reuter+sale+will+pct+loss+market+rate+april+offer+gas+price+japan+currency+will+official+japanese+profit+franc+quarter+unit+inc+export+reagan+import+qtr+central+year+div+will+mln+price+crude+surplus+yen+loan+pay+sell+group+year+opec+agriculture, data=corpusdataframetrain,importance=TRUE, ntree=2000) # Random Forest 
SVM <- svm(as.factor(ClassTrain)~ billion+mln+dollar+bank+cts+company+share+oil+trade+year+net+rate+pct+dividend+dlrs+tonne+state+bank+reuter+sale+will+pct+loss+market+rate+april+offer+gas+price+japan+currency+will+official+japanese+profit+franc+quarter+unit+inc+export+reagan+import+qtr+central+year+div+will+mln+price+crude+surplus+yen+loan+pay+sell+group+year+opec+agriculture, data=corpusdataframetrain,importance=TRUE, ntree=2000) # SVM
prediction<-predict(RF, Train)
predictedclasscount<-table(prediction)
Classcount <- table(Train$ClassTrain)
confusion<-table(Train$ClassTrain,prediction)
confusion

  for (i in 1:9){
    TP<-confusion[i,i]
    TPFP<-sum(confusion[,i])
    FNTP<-sum(confusion[i,])
    Precision<-TP/TPFP
    Recall<- TP/FNTP
    fMeasure<- (2* Precision*Recall)/(Precision+Recall)
    print(TP)
    print(TPFP)
    print(FNTP)
       }
print(Precision)
print(Recall)
print(fMeasure)
#Accuracy <- (tp + tn) / (tp + tn + fp + fn)
#Recall = tp / (tp + fn)
#Precision = tp / (tp + fp)
#FMeasure = 2 * precision * recall / (precision + recall)

################################################################### Question 4

corpusdataframetrain1<-corpusdataframetrain[,order(colSums(corpusdataframetrain[1:27357]), decreasing=TRUE)] #Order the topics from majority to minority
CorpusDF<-corpusdataframetrain1[1:1000]
CorpusDF<-cbind(CorpusDF, corpusdataframetrain[27359])
#Kmeans Clustering
results<-kmeans(CorpusDF[1:1001], 9)
results$cluster
table(CorpusDF$ClassTrain,results$cluster)
plot(CorpusDF[1:10], col=results$cluster)
CorpusDF<-CorpusDF[1:1000,]
Plot<-clusplot(CorpusDF, results$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

#silhouette
CorpusDF.pam = pam(CorpusDF[1:500],9)
plot(CorpusDF.pam)

# Hierarchical Clustering
d <- dist(CorpusDF, method = "euclidean") # distance matrix
Cluster <- hclust(d, method="ward.D2") # display dendogram
groups <- cutree(Cluster, k=9) # cut dendogram into 9 clusters
plot(Cluster, xlab="Cluster using Ward Method", ylab="Feature")
rect.hclust(Cluster, k=9, border="red")

#silhouette for HAC
Silhouette <- silhouette(cutree(CorpusDF, k = 9), daisy(CorpusDF))
plot(sillhouette)

#DBScan
d <- dbscan(CorpusDF[1:10000,],9,MinPts=3,showplot = 1); 

#silhouette for DBScan
CorpusDF.pam = pam(CorpusDF[1:500],9)
plot(CorpusDF.pam)