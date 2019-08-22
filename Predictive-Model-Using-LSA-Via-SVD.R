library(quanteda)
library(stopwords)
library(topicmodels)
library(tidyverse)
library(tidytext)
library(caTools)
library(ROCR)
library(rpart)
library(rpart.plot)
library(car)
library(Hmisc)

#importing dataset to the R environment and work on
Dataset <- read.csv("gastext.csv", stringsAsFactors = FALSE)

#Data Preprocessing

#Check the number of NAs in each column to aid direction of preprocessing
colSums(is.na(Dataset))

#Check the data types of columns to see if there will be the need to rectify some of them to the right data type
str(Dataset)

#=================================================================================================================
#Converting all the variables having the data type of integer to factors 
Variables <- c(1, 3:15)
Dataset[, Variables] <- lapply(Dataset[, Variables] , factor)
str(Dataset)

#=================================================================================================================
#Cleaning the text version (Variable--Comment) of the Dataset
#Create a corpus
Corpus4Comments <- corpus(Dataset$Comment)
summary(Corpus4Comments)

#Construct a sparse document-feature matrix, from the just created corpus (Corpus4Comments)
#while convert all features to lowercase, steming, removing stopword, in english and other
#common unaided characters 
DFM4Comments <- dfm(Corpus4Comments, tolower = T ,stem = T, remove_punc = T, remove = c(stopwords("english"), " ", ".", ","))
summary(DFM4Comments)

#=================================================================================================================
#1.	Provide the word cloud after pre-processing
WordCloud4Comments <- textplot_wordcloud(DFM4Comments, min_size = 0.5, max_size = 4, min_count = 2,
                   max_words = 500, color = "darkblue",rotation = 0.1, random_order = FALSE, 
                   random_color = FALSE,ordered_color = FALSE, labelcolor = "gray20", labelsize = 1.5,
                   labeloffset = 0, fixed_aspect = TRUE, comparison = FALSE)

#=================================================================================================================
#2.	What are the top 5 terms that are most related to "price"? Specify your similarity measurement method and detailed results.
Top5termsRel2Price <- textstat_simil(DFM4Comments, selection = "price", margin = "feature", 
                                     method = "cosine", upper = FALSE, diag = FALSE)

#Print the to 5 terms that are most related to "price"
as.list(Top5termsRel2Price,n = 5)

#=================================================================================================================
#3.	What are the top 5 terms that are most related to "service"? Specify your similarity measurement method and detailed results.
Top5termsRel2Service <- textstat_simil(DFM4Comments, selection = "servic", margin = "feature", 
                                       method = "jaccard", upper = FALSE, diag = FALSE)

#Print the to 5 terms that are most related to "service"
as.list(Top5termsRel2Service,n = 5)

#=================================================================================================================
#Topic Modeling
#4.	Perform topic modeling with 4 topics
FarClDFM4Comments <- dfm_remove(DFM4Comments, c("shower","Point", "productx", " ", ".", ",", "?"))

#Checkin if those selected words are dropped with plot
FarClWordCloud4Comments <- textplot_wordcloud(FarClDFM4Comments, min_size = 0.5, max_size = 4, min_count = 2,
                                         max_words = 500, color = "darkblue",rotation = 0.1, random_order = FALSE, 
                                         random_color = FALSE,ordered_color = FALSE, labelcolor = "gray20", labelsize = 1.5,
                                         labeloffset = 0, fixed_aspect = TRUE, comparison = FALSE)

#Eliminating all zeros rows from Data
FarClDFM4Comments <- as.matrix(FarClDFM4Comments)
FarClDFM4Comments <- FarClDFM4Comments[which(rowSums(FarClDFM4Comments)>0),]
FarClDFM4Comments <- as.dfm(FarClDFM4Comments)

LDA4Comments <- LDA(FarClDFM4Comments,k = 4, control = list(seed = 123))
LDA4Comments

#Making the LDA_VEM tidy for further analysis using Tidy package
TidyLDA4Comments <- tidy(LDA4Comments)
summary(TidyLDA4Comments)

#o	Provide the term/beat plots for four topics
TopTerms4LDA4Comments <- TidyLDA4Comments %>%
  group_by(topic) %>%
  top_n(4, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

#Visulization
TopTerms4LDA4Comments %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) + 
  labs(title = "Visualization of Term vs Bete for the Top Four Topics",
       x = "Terms", y = "Beta") + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") + coord_flip()

#o	Try your best to summarize those four topics
# View topic 4 terms in each topic
LDA4CommentsTerm <- as.matrix(terms(LDA4Comments,4))
View(LDA4CommentsTerm)

# Document-topic probabilities
DocTopicProb <- tidy(LDA4Comments, matrix = "gamma")
DocTopicProb

# View document-topic probabilities in a table
DocTopicProbView  <- as.data.frame(LDA4Comments@gamma)
View(DocTopicProbView)

#=================================================================================================================
#Preprocessing Data for Deision Tree Model
# Creating the repeatability seed 

set.seed(123)

Data4Analysis <- Dataset %>% select(3:15)

# Data partition

DataSplit <- sample.split(Data4Analysis$Target, SplitRatio = 0.7)

# Creating Training Dataset
TrainSet = subset(Data4Analysis, DataSplit == TRUE)

# Create Testing Dataseet
TestSet = subset(Data4Analysis, DataSplit == FALSE)

#o	Model 1 only uses non-text information (i.e., does not use the Comment column)
# Building a Decision Tree model 1

DTreeModel1 <- rpart(Target ~., method="class", data = TrainSet)

# Print out the result
printcp(DTreeModel1)

#To Void error message of margins been too large
par(mar = rep(2, 4))

# Display decision tree plot
prp(DTreeModel1, type = 2, extra = "auto", cex = 0.8,
    main = "Decision Tree Diagram for gas stations'Target customers")

#Evaluating model performance using the test dataset
#Predicting the default probabilities based on the test dataset
PredProba <- predict(DTreeModel1,TestSet)
PredProba <- as.data.frame(PredProba)

#Turn the default probabilities to binary of threshold 0.5
ConvertProb2Binary <- function(DTreeProb){
  if (DTreeProb >= 0.5){
    return('Yes')
  }else{
    return("No")
  }
}

PredProba$Result <- sapply(PredProba$`1`,ConvertProb2Binary)

# Creating the confusion matrix
print("The confusion matrix is:")
print(table(PredProba$Result,TestSet$Target))

# Creating the ROC curve
pred <- prediction(PredProba$`1`,TestSet$Target)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
abline(a=0,b=1)

# Calculating and printing AUC value
auc <- performance(pred, measure="auc")
auc <- auc@y.values[[1]]
print(paste("AUC for the Decision tree model is:", auc))

#=================================================================================================================
#Further Cleaning for the corpus
# Further remove very infrequent words by keep only words occurring 5 times in 1000, and in 2 of 5 of documents 
MoClDFM4Comments <- dfm_trim(FarClDFM4Comments, min_docfreq = 0.4, 
                             min_termfreq = 0.005, termfreq_type = "prop") 
dim(MoClDFM4Comments)

#Weight a dfm by term frequency-inverse document frequency (tf-idf)
MoClDFM4Comment <- dfm_tfidf(MoClDFM4Comments, scheme_tf = "count", scheme_df = "inverse", 
                             base = 10, force = FALSE)
dim(MoClDFM4Comment)

# Fitting the Latent Semantic Analysis scaling model to data for dimension reduction
# 10 dimensions to be included in the output and smooth the margin "both", "documents", "features" 
SVDMuseLSA <- textmodel_lsa(MoClDFM4Comment, nd = 5, margin = c("both", "documents", "features"))
head(SVDMuseLSA$docs)

#combine both Dataset for the model2 Decision tree model
#dropping the last row of the categorical dataset to match the SVD
Data4Analysis <- Data4Analysis[-287,]
DataAnalysis <- cbind(Data4Analysis, as.data.frame(SVDMuseLSA$docs))

#=================================================================================================================
# Data partition

DataSplit <- sample.split(DataAnalysis$Target, SplitRatio = 0.7)

# Creating Training Dataset
TrainSet = subset(DataAnalysis, DataSplit == TRUE)

# Create Testing Dataseet
TestSet = subset(DataAnalysis, DataSplit == FALSE)

#o	Model 1 only uses non-text information (i.e., does not use the Comment column)
# Building a Decision Tree model 1

DTreeModel2 <- rpart(Target ~., method="class", data = TrainSet)

# Print out the result
printcp(DTreeModel2)

#To Void error message of margins been too large
par(mar = rep(2, 4))

# Display decision tree plot
prp(DTreeModel2, type = 2, extra = "auto", cex = 0.8,
    main = "Decision Tree Diagram for gas stations'Target customers with SVD")

#Evaluating model performance using the test dataset
#Predicting the default probabilities based on the test dataset
PredProba2 <- predict(DTreeModel2,TestSet)
PredProba2 <- as.data.frame(PredProba2)

#Turn the default probabilities to binary of threshold 0.5
ConvertProb2Binary <- function(DTreeProb){
  if (DTreeProb >= 0.5){
    return('Yes')
  }else{
    return("No")
  }
}

PredProba2$Result <- sapply(PredProba2$`1`,ConvertProb2Binary)

# Creating the confusion matrix
print("The confusion matrix is:")
print(table(PredProba2$Result,TestSet$Target))

# Creating the ROC curve
pred2 <- prediction(PredProba2$`1`,TestSet$Target)
perf2 <- performance(pred2, "tpr", "fpr")
plot(perf2)
abline(a=0,b=1)

# Calculating and printing AUC value
auc <- performance(pred2, measure="auc")
auc <- auc@y.values[[1]]
print(paste("AUC for the Decision tree model is:", auc))

#=================================================================================================================
#base on the confusion matrix of the validation dataset the first model (Model1, without Comment column, yeilding Accuracy of 0.61) 
#is better model compare to the second model (Model2, with the comment column, yeilding accuracy of 052)