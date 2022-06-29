

### Course: Text Analysis
### Lecturer: Dr. Jochen Hartmann
### Assignment: "Predicting journalist success on twitter from text data"


### Data Source:  https://www.pnas.org/doi/10.1073/pnas.2026045118 
### Reference: Markowitz, D. M., & Shulman, H. C. (2021). The predictive utility of word familiarity for online engagements and funding. Proceedings of the National Academy of Sciences, 118(18).
### Please see the ReadMe file for variable descriptions: https://osf.io/rym7z/


# check and set working directory
getwd()
rm(list=ls())



# set random seed to make results replicable
set.seed(42)
# Install libraries
install.packages('tidytext')
install.packages('wordcloud')
install.packages('tidyverse')
install.packages('ggpubr')

# load libraries
library(dplyr)
library(stargazer)
library(car)
library(anytime)
library(tm)
library(tidytext)
library(wordcloud)
library(tidyverse)
library("syuzhet")
library(rpart)
library(rpart.plot)
library(caret)
library(ggpubr)
library(corrplot)


# load data
d <- read.csv("journalists_data_OSF.csv", stringsAsFactors = F)

# explore data
head(d, 3)
View(d[1:10,])
length(unique(d$source))  # check number of tweets sources
unique(d$source) # list of sources
length(unique(d$screen_name))  # check number of tweets sources
unique(d$screen_name) # list of sources
summary(d)
sort(table(d$source), decreasing = T)  # check distribution of sources
d %>% 
  group_by(source) %>% 
  summarise(number_of_journalist_per_source= length(unique(screen_name)))# number of journalists per source

tapply(d$screen_name, INDEX = d$source, FUN = unique) # names of journalists

d1<- d %>% 
  group_by(source, screen_name) %>% 
  summarise(Number_of_tweets= n(),
            Likes=mean(favorite_count),
            Retweets= mean(retweet_count)) # summary of descriptive statistics
d1

write.csv(d1, file="summary.csv", row.names = FALSE)

d %>% 
  group_by(screen_name) %>% 
  arrange(-favorite_count) %>% 
  select(c("screen_name","text","favorite_count")) # Top favorite tweets

d %>% 
  group_by(screen_name) %>% 
  arrange(-ln_favorite_count_prop) %>% 
  select(c("screen_name","text","ln_favorite_count_prop"))# Top favorite tweets corrected for time difference

d %>% 
  group_by(screen_name) %>% 
  arrange(-retweet_count) %>% 
  select(c("screen_name","text","retweet_count"))# Top retweets 
d %>% 
  group_by(screen_name) %>% 
  arrange(-ln_retweet_count_prop) %>% 
  select(c("screen_name","text","ln_retweet_count_prop"))# Top retweets corrected for time difference

# Correlation bewteen engagement variables

cor(d$ln_favorite_count_prop,d$ln_retweet_count_prop)


ggscatter(d, x = "ln_favorite_count_prop", y = "ln_retweet_count_prop", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Natural log transformed likes", ylab = "Natural log transformed retweet")
 # Natural log transformed likes and retweets were also positively linked (r = 0.78, P < 0.001) 

res <- cor.test(d$ln_favorite_count_prop, d$ln_retweet_count_prop, 
                method = "pearson")
res
# # Natural log transformed likes and retweets were also positively linked (r = 0.78, P < 0.001) 

# correlation between engagement metrics and LIWC categories

d2 <-subset(d, select = -c(user_id,status_id,created_at,created_at_date,pull_date,date_difference,screen_name,text,source,
                           favorite_count,ln_favorite_count,favorite_count_prop,retweet_count,ln_retweet_count,retweet_count_prop))

d2 <-subset(d, select = -c(user_id,status_id,created_at,created_at_date,pull_date,date_difference,screen_name,text,source,
                           ln_favorite_count,favorite_count_prop,ln_retweet_count,retweet_count_prop))

corr_d2=cor(d2,method="s")
corr_d2[1:5,1:5]

corrplotfile <- "corrplot_result.png"
png(corrplotfile, height = 2400, width = 2400)
corrplot(corr_d2, tl.cex = 2, cl.cex = 2, number.cex = 2)
dev.off()

# Testing the correlation between the Dict category and the online engagement
res <- cor.test(d$ln_favorite_count_prop, d$Dic,
                method = "pearson")

res # cor = -.109 with p-value < 2.2e-16
res <- cor.test(d$ln_retweet_count_prop, d$Dic,
                method = "pearson")
res # # cor = -.075 with p-value < 2.2e-16


# transform variables
summary(d$WC)
d$WC_mc <- as.numeric(scale(d$WC, scale = F))  # mean-center word count (WC)
summary(d$WC_mc)  # check mean-centering
d$social_engagement_index <- (d$ln_favorite_count_prop+d$ln_retweet_count_prop)/2
head(d[ ,c("ln_favorite_count_prop","ln_retweet_count_prop","social_engagement_index")],5)


# pre-process data
corpus<- iconv(d$text, to="utf-8" ) 
corpus <- Corpus(VectorSource(d$text))
inspect(corpus[1:5])
corpus <- tm_map(corpus, tolower)
inspect(corpus[1:5])
corpus <- tm_map(corpus, removePunctuation)  # remove punctuation
inspect(corpus[1:5])
corpus <- tm_map(corpus, removeNumbers)
inspect(corpus[1:5])
corpus<- tm_map(corpus, removeWords, stopwords('english')) # remove stopwords like 'and'
inspect(corpus[1:5])
removeURL <- function(x)gsub('http[[:alnum:]]*','',x)
cleanset <- tm_map(corpus, content_transformer(removeURL))
inspect(cleanset[1:5])
cleanset <- tm_map(cleanset, gsub, pattern='â???T', replacement='')
cleanset <- tm_map(cleanset, gsub, pattern='â???', replacement='')
cleanset <- tm_map(cleanset, gsub, pattern='â???o', replacement='')
cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:5])

# Term document Matrix
tdm <- TermDocumentMatrix(cleanset)
tdm
tdm <- as.matrix(tdm)
tdm[1:10, 1:20]

# Bar plot
w <- rowSums(tdm)
w <- subset(w, w>=200)
barplot(w,
        las=2,
        col= rainbow(50)
  
)

# Word cloud
w <- sort(rowSums(tdm), decreasing= T)
set.seed(222)
wordcloud(words=names(w), freq= w,
          max.words=98,
          random.order = F,
          min.freq = 5,
          colors=brewer.pal(8, 'Dark2'),
          scale= c(5,0.3),
          rot.per = 0.3)

# Obtain sentiments Score
s <- get_nrc_sentiment(d$text)
head(s)
d$text[2]

barplot(colSums(s),
        las = 2,
        col= rainbow(10),
        ylab= 'Count',
        main= 'nrc_Sentiment Score')




# create additional text-based variables
d$buzz <- grepl("\\bbreaking\\b|\\bnews\\b|\\bstate\\b|\\bcountry\\b|\\bwatch\\b|\\bpresident\\b|\\bread\\b|\\btrump\\b", tolower(d$text))  # helpful resource for regex: https://cran.r-project.org/web/packages/stringr/vignettes/regular-expressions.html
round(prop.table(table(d$buzz)), 3)  # explore new variable

# Statitiscal test on the new variable
d$buzz<-ifelse(d$buzz=="FALSE",0,1)

t.test(d$favorite_count ~ d$buzz)
# mean in group 0 mean in group 1 
# 508.6921       1177.9522 
# p-value = 2.903e-11
d%>%
  filter(buzz == T) %>% select(text) %>% sample_n(5)

t.test(d$retweet_count ~ d$buzz)
# mean in group 0 mean in group 1 
# 1016.794       1001.768 
# p-value = 0.9457
d%>%
  filter(buzz == T) %>% select(text) %>% sample_n(5)


## The use of word in the lexicon "buzz" tend to lead to higher favorites( likes) and this observation is statiscally signifiant
## However this does not automatically translate to retweet

# variance analysis
results.aov <- aov(d$favorite_count ~ d$buzz)
summary(results.aov)

# to compute effect size d for mean differences:  
if(!require(effectsize)) {    # installing this package allows you to read data from Excel
  install.packages("effectsize"); require(effectsize)}
cohens_d(d$favorite_count ~ d$buzz)
# cohen d=-.21

ggplot(d, aes(x=as.factor(buzz), y=favorite_count)) + 
geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("buzz lexicon")




# extract Affin sentiment
selected_examples <- d$text  # select examples
sentiment_score <- get_sentiment(selected_examples, 
                                 method = "afinn")
affin <- data.frame(example_sentence = selected_examples, sentiment_score, 
                          polarity = ifelse(sentiment_score > 0, 1, 0))  # assign sentiment to sentences

View(affin)

d <- inner_join(d,affin[,-2], by=c('text'='example_sentence'))
View(d[110:114])

t.test(favorite_count ~ polarity, data =d)
# mean in group 0 mean in group 1 
# 763.8478        441.9050
# p-value = 2.202e-09

t.test(retweet_count ~ polarity, data =d)

# mean in group 0 mean in group 1 
# 1139.2268        997.8148
# p-value = 0.6253

## The use of word in the affin dictionary tends to lead to higher favorites( likes) and this observation is statiscally signifiant
## However this does not automatically translate to retweet

# effect of size

cohens_d(favorite_count ~ polarity, data=d)

# cohen d=.1

ggplot(d, aes(x=as.factor(polarity), y=favorite_count)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("affin lexicon")


# visualize data
# plot(table(d$created_at_date), main = "Number of Projects over Time")  # plot distribution of years
#par(mfrow = c(1,2))
hist(d$ln_favorite_count_prop, main = "Time corrected Favorites engagement (in likes, ln)")
hist(d$ln_retweet_count_prop, main = "Time corrected Retweets engagement(in retweets, ln)")
hist(d$WC, main = "Word Count")
hist(d$WC_mc, main = "Word Count (mean-centered)")


# sample data (optional)

set.seed(42)
sample<- sample.int(n = nrow(d), 500, replace = F)
sample_500 <- d[sample, ]
d_test <- d[-sample, ]


write.csv(sample_500, file='sample.csv', row.names = FALSE)
write.csv(d_test, file = 'd_test.csv', row.names = FALSE)

# explore data

sample_labelled <- read.csv("sample_labelled.csv",stringsAsFactors = F)
head(sample_labelled)
table(sample_labelled$politically_oriented)

# pre-process data
corpus <- VCorpus(VectorSource(sample_labelled$text))
corpus_clean <- tm_map(corpus, content_transformer(tolower))
corpus_clean <- tm_map(corpus_clean, removePunctuation)  # remove punctuation
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords('english')) # remove stopwords like 'and'
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords('english')[-which(stopwords('english') == "not")])  # remove stopwords like 'and', except for 'not'

# print stopwords
stopwords('english')  
"and" %in% stopwords('english')  # check if a word occurs in the stopwords

inspect(corpus_clean[1:5])

# specify tokenization limits
min_uni <- 3
min_chars <- 2
max_chars <- 30

# tokenize, create document-term-matrix (dtm)
UnigramTokenizer <- function(x) unlist(lapply(ngrams(words(x), 1), paste, collapse = " "), use.names = FALSE)
dtm_unigram <- DocumentTermMatrix(corpus_clean, control = list(tokenize = UnigramTokenizer, 
                                                               wordLengths=c(min_chars,max_chars), 
                                                               bounds = list(global = c(min_uni,Inf))))
dtm_unigram <- weightBin(dtm_unigram)
colnames(dtm_unigram)

# explore dtm
dtm_unigram
head(as.matrix(dtm_unigram)[,1:15])


# add labels to dtm
labeled_dtm <- as.data.frame(cbind(sample_labelled$politically_oriented, as.data.frame(as.matrix(dtm_unigram))))
str(labeled_dtm)
labeled_dtm <- labeled_dtm[,-c(2,3,4,5,6,7,8,9)] # deleting inconsistent attributes
str(labeled_dtm)
labeled_dtm[,-1] <- apply(labeled_dtm[,-1], 2, function(x) as.numeric(as.character(x)))
colnames(labeled_dtm)[1] <- "y"

# split data
partition <- .8
set.seed(0); 
trainIndex <- createDataPartition(labeled_dtm$y, p = partition, list = FALSE)
#set.seed(100)
#trainIndex<- sample.int(n = nrow(labeled_dtm), size = floor(partition*nrow(labeled_dtm)), replace = F)
train <- labeled_dtm[trainIndex,]
test <- labeled_dtm[-trainIndex,]
nrow(train) + nrow(test) == nrow(labeled_dtm)


# train decision tree (DT)
colnames(train) <- make.names(colnames(train))
colnames(test) <- make.names(colnames(test))
model_dt <- rpart(y ~ ., data = train, cp = 0.00, method = "class")  # optional: Let's vary the complexity parameter (cp)
model_dt

# plot decision tree
rpart.plot(model_dt, 
           box.palette="Blues",
           tweak = 1,
           fallen.leaves = TRUE,
           round = 0,
           type=1)

# predict on hold-out test data
preds_dt <- predict(model_dt, newdata = test, type = "class")
head(preds_dt)
length(preds_dt)
length(colnames(test))

# evaluate accuracy
round(mean(preds_dt == test$y), 2)

# Accuracy DT evaluated a 80%

# Applying Code sentiment ML Advanced
pre_process_data <- function(dataset){
  
  processed_dataset <- VCorpus(VectorSource(sample_labelled$text))
  processed_dataset <- tm_map(processed_dataset, content_transformer(tolower))
  processed_dataset <- tm_map(processed_dataset, removeNumbers)
  processed_dataset <- tm_map(processed_dataset, removePunctuation)
  processed_dataset <- tm_map(processed_dataset, stripWhitespace)
  
  min_uni <- 3; min_bi <- 7; min_chars <- 3
  my_tokenizer <- function(x) unlist(lapply(NLP::ngrams(words(x), 1:2), paste, collapse = " "), use.names = FALSE)
  dtm_uni_bigram <- DocumentTermMatrix(processed_dataset, control = list(tokenize = my_tokenizer, wordLengths=c(min_chars,20), bounds = list(global = c(min_uni,Inf))))
  return(dtm_uni_bigram)
  
  
}

corpus_clean_dtm <- pre_process_data(sample_labelled)
corpus_clean<- as.data.frame(as.matrix(corpus_clean_dtm))


str(corpus_clean)
object.size(corpus_clean) #3041656 bytes
head(corpus_clean[,700:730])  # explore bigrams (at the end of the dtm)
View(head(corpus_clean, 20))
which.max(colSums(corpus_clean)) # 576

# Split labeled DTM into training set (80% of data) and hold-out test set (20% of data)

partition <- .8

labeled_dtm <- as.data.frame(cbind(sample_labelled$politically_oriented, corpus_clean))
labeled_dtm[,-1] <- apply(labeled_dtm[,-1], 2, function(x) as.numeric(as.character(x)))
colnames(labeled_dtm)[1] <- "class_dv"

set.seed(128); trainIndex <- createDataPartition(sample_labelled$politically_oriented, p = partition, list = FALSE)
train_labeled <- labeled_dtm[trainIndex,]
test_labeled <- labeled_dtm[-trainIndex,]

# Define trainControl functions

cv_tune <- 5; rep_tune <- 1
cv_final <- 10; rep_final <- 5

ctrl_tune <- trainControl(method = "repeatedcv", number = cv_tune, repeats = rep_tune, selectionFunction = "best", 
                          verboseIter = TRUE, savePredictions = "final", classProbs = FALSE)
ctrl_final <- trainControl(method = "repeatedcv", number = cv_final, repeats = rep_final, selectionFunction = "best", 
                           verboseIter = TRUE, savePredictions = "final", classProbs = FALSE)

# Set parameter grids

grid_knn <- expand.grid(k = c(1,15,30,45,65))
grid_rf <- expand.grid(mtry = c(round(sqrt(ncol(train_labeled))/2),round(sqrt(ncol(train_labeled)))), 
                       splitrule = "gini", min.node.size = 1)
grid_svm <- expand.grid(C = c(0.01,0.1,1,10,100))

# Create set of models and combine grids

set_of_models <- c("knn", "ranger", "svmLinear")
model_parameter_grids <- as.data.frame(matrix(nrow = length(set_of_models), ncol = 2))

colnames(model_parameter_grids) <- c("model", "parameter_grid")
model_parameter_grids$model = set_of_models
model_parameter_grids$parameter_grid = list(grid_knn, grid_rf, grid_svm)
model_parameter_grids

df_train_results <- as.data.frame(matrix(nrow = length(set_of_models), ncol = 5))
colnames(df_train_results) <- c("final_model", "model", "train_acc", "tuned_parameters", "runtime")

# Initialize lists

models = list()
final_model_list = list()
tuned_parameters = list()
models_final = list()
final_model_list_final = list()

# Train models

set.seed(128); system.time(
  for(i in 1:length(set_of_models)) {
    
    method_train <- model_parameter_grids$model[i]
    grid <- model_parameter_grids$parameter_grid[i]
    grid <- grid[[1]]
    
    fitted <- caret::train(y = factor(train_labeled[,1]), x = train_labeled[,-1], method = method_train, metric = "Accuracy",
                           tuneGrid = grid, trControl = ctrl_tune)
    
    final_model <- fitted
    train_acc <- caret::confusionMatrix(fitted$pred$pred, fitted$pred$obs)
    
    final_model_list[[i]] <- final_model
    models[[i]] <- fitted
    tuned_parameters[[i]] <- fitted$bestTune
    
    df_train_results$train_acc[i] <- round(train_acc$overall[1],4)
    
    # Fit tuned model on full dataset
    
    fitted_final <- caret::train(y = factor(labeled_dtm[,1]), x = labeled_dtm[,-1], method = method_train, metric = "Accuracy",
                                 tuneGrid = fitted$bestTune, trControl = ctrl_final)
    
    final_model_final <- fitted_final
    repeated_acc <- caret::confusionMatrix(fitted_final$pred$pred, fitted_final$pred$obs)
    
    final_model_list_final[[i]] <- final_model_final
    models_final[[i]] <- fitted_final
    
    df_train_results$repeated_acc[i] <- round(repeated_acc$overall[1],4)
    
    
  }
)

# Save models and tuned parameters

df_train_results$final_model <- final_model_list
df_train_results$model <- models
df_train_results$tuned_parameters <- tuned_parameters

parameters <- data.frame(df_train_results$tuned_parameters[[1]]$k,
                         df_train_results$tuned_parameters[[2]]$mtry,
                         df_train_results$tuned_parameters[[3]]$C)
colnames(parameters) <- c("kNN_k", "RF_mtry", "SVM_C")

# Compute standard deviations and standard errors

std <- function(x) sd(x)/sqrt(length(x))
std_dev <- vector(mode="numeric", length=0)
std_err <- vector(mode="numeric", length=0)

for(l in 1:length(set_of_models)) {
  
  std_dev[l] <- sd(final_model_list_final[[l]]$resample$Accuracy)
  std_err[l] <- std(final_model_list_final[[l]]$resample$Accuracy)
  
}

# explore variance of accuracy for rf
final_model_list_final[[2]]$resample$Accuracy

df_train_results$std_dev <- round(std_dev,4); df_train_results$std_err <- round(std_err,4)

# Predict on hold-out test set

df_train_results$test_acc = NA
predictions <- as.data.frame(matrix(nrow = nrow(test_labeled), ncol = length(final_model_list)))
colnames(predictions) <- c("kNN", "RF", "SVM")
head(predictions)

for(j in 1:length(final_model_list)) {
  
  method_train <- model_parameter_grids$model[j]
  
  pred_i <- predict(final_model_list[[j]], test_labeled[, -1], type = "raw")
  
  test_acc <- caret::confusionMatrix(pred_i, as.factor(test_labeled[,1]))
  df_train_results$test_acc[j] <- round(test_acc$overall[1],4)
  predictions[, j] = pred_i 
  
}

# Consolidate results

results_cols <- c("test_acc", "std_dev", "std_err")
results <- df_train_results[,results_cols]
rownames(results) <- c("kNN", "RF", "SVM")

# Print results

results

# Plot results

ggplot(results, aes(x=rownames(results), y=test_acc)) + 
  geom_bar(position=position_dodge(), stat="identity", fill = "#4285f4", size=.3) +
  geom_errorbar(aes(ymin=test_acc-2*std_err, ymax=test_acc+2*std_err), size=.3,
                width=.2, position=position_dodge(.9)) +
  geom_text(aes(label = sprintf("%.2f", test_acc), y= test_acc),  vjust = -2)+
  xlab("Method") +
  ylab("Accuracy (%)") +
  scale_y_continuous(limits = c(0,1)) +
  ggtitle(paste0("Accuracy per Method, ", "N=", nrow(data))) +
  theme_classic()
#colnames(kickstarter_test)
#if(FALSE){d <- d %>% sample_n(10000)}

corpus2 <- VCorpus(VectorSource(d_test$text))
dtm2 <- DocumentTermMatrix(corpus2, control = list(dictionary = Terms(corpus_clean_dtm), 
                                                   weighting = function(x) weightBin(x)))  




dtm2  # inspect document-term-matrix (number of columns should be identical to the document-term-matrix based on which the classifier is trained)
#colnames(d)<- make.names(colnames(d))
#d_test <- read.csv("d_test.csv",stringsAsFactors = F)
d_test$politically_oriented<- predict(final_model_list[[2]], as.data.frame(as.matrix(dtm2)), type = "raw")  # create new column based on predictions from classifier
table(d_test$politically_oriented)  # important: this is a weak sentiment measure and just for illustration purposes
d3<-rbind(sample_labelled,d_test)

# Save the file 
write.csv(d3, file='d3.csv', row.names = FALSE)

#Pretrained models Roberta
dr<-read.csv("dr.csv", stringsAsFactors= F)
df<-cbind(d,dr$rob_label)

# merging the tables
dfinal <- inner_join(d3,dr[,-c(1,3,5)], by= 'text')
View(dfinal[110:116])
dfinal$rob_label <- ifelse(dfinal$rob_label=='POSITIVE',1,0)


# plot the data to observe random effects

ggplot(dfinal, aes(Dic,ln_favorite_count_prop))+
  geom_point()+
  stat_smooth(method='lm')+
  facet_wrap(~screen_name)+
  labs(title='Favorite engagement',
       subtitle='Natural log transformed favorite count by screen_name',
       x='LIWC Dictionary',
       y='Natural log transformed favorite count ')+
       theme(panel.background = element_rect(fill='NA'),
             axis.line = element_line(color='black'))


ggplot(dfinal, aes(Dic,ln_retweet_count_prop))+
  geom_point()+
  stat_smooth(method='lm')+
  facet_wrap(~screen_name)+
  labs(title='Retweet engagement',
       subtitle='Natural log transformed retweet count by screen_name',
       x='LIWC Dictionary',
       y='Natural log transformed retweet count ')+
  theme(panel.background = element_rect(fill='NA'),
        axis.line = element_line(color='black'))
  

### correlation among the 5 textual features
dfinal$politically_oriented <- as.numeric(dfinal$politically_oriented)
round(cor(cbind(dfinal$buzz, dfinal$polarity, dfinal$politically_oriented, dfinal$rob_label, dfinal$Dic)), digits = 2)

#analyze data
m <- list()
#m[[1]] <- lm(ln_favorite_count_prop ~ WC_mc+ buzz + polarity + politically_oriented + rob_label+ Dic , data = dfinal )  # linear regression (for continuous data)
#m[[1]] <- lm(ln_favorite_count_prop ~ Dic, data = dfinal)
#m[[2]] <- update(m[[1]], "ln_retweet_count_prop ~ .")  # change to ln_retweet_count_prop to compare model fit

library(lme4)# for mixed effects models
install.packages('lmerTest')
library(lmerTest)
#-------------------------------------------------------------------------------------------------------------
# Linear mixed models: ln_favorite_count_prop Vs Textual features
# fit a mixed model with screen_name nested with the source as a random effect on the intercept

m[[1]] <- lmer(ln_favorite_count_prop ~ Dic +(1 | source/screen_name), data = dfinal )
m[[2]] <- lmer(ln_favorite_count_prop ~ buzz +(1 | source/screen_name), data = dfinal )
m[[3]] <- lmer(ln_favorite_count_prop ~ politically_oriented +(1 | source/screen_name), data = dfinal )
m[[4]] <- lmer(ln_favorite_count_prop ~ polarity +(1 | source/screen_name), data = dfinal )
m[[5]] <- lmer(ln_favorite_count_prop ~ rob_label +(1 | source/screen_name), data = dfinal )


# summary
summary(m[[1]])
summary(m[[2]])
summary(m[[3]])
summary(m[[4]])
summary(m[[5]])


# view random effects
ranef(m[[1]])
ranef(m[[2]])
ranef(m[[3]])
ranef(m[[4]])
ranef(m[[5]])

# R2  for mixed model
install.packages('MuMIn')
library(MuMIn)

r.squaredGLMM(m[[1]])
r.squaredGLMM(m[[2]])
r.squaredGLMM(m[[3]])
r.squaredGLMM(m[[4]])
r.squaredGLMM(m[[5]])

# Confidence intervals for mixed model
confint(m[[1]], oldNames = FALSE)
confint(m[[2]], oldNames = FALSE)
confint(m[[3]], oldNames = FALSE)
confint(m[[4]], oldNames = FALSE)
confint(m[[5]], oldNames = FALSE)

#----------------------------------------------------------------------------------------------------------
# Linear mixed models: ln_retweet_count_prop Vs Textual features
# fit a mixed model with screen_name nested with the source as a random effect on the intercept
re <- list()
re[[1]] <- update(m[[1]], "ln_retweet_count_prop ~ .")  # change to non-ln transformed ln_retweet_count_prop to compare model fit
re[[2]] <- update(m[[2]], "ln_retweet_count_prop ~ .")  # change to non-ln transformed ln_retweet_count_prop to compare model fit
re[[3]] <- update(m[[3]], "ln_retweet_count_prop ~ .")  # change to non-ln transformed ln_retweet_count_prop to compare model fit
re[[4]] <- update(m[[4]], "ln_retweet_count_prop ~ .")  # change to non-ln transformed ln_retweet_count_prop to compare model fit
re[[5]] <- update(m[[5]], "ln_retweet_count_prop ~ .")  # change to non-ln transformed ln_retweet_count_prop to compare model fit

# summary
summary(re[[1]])
summary(re[[2]])
summary(re[[3]])
summary(re[[4]])
summary(re[[5]])


# view random effects
ranef(re[[1]])
ranef(re[[2]])
ranef(re[[3]])
ranef(re[[4]])
ranef(re[[5]])

# R2  for mixed model

r.squaredGLMM(re[[1]])
r.squaredGLMM(re[[2]])
r.squaredGLMM(re[[3]])
r.squaredGLMM(re[[4]])
r.squaredGLMM(re[[5]])

# Confidence intervals for mixed model
confint(re[[1]], oldNames = FALSE)
confint(re[[2]], oldNames = FALSE)
confint(re[[3]], oldNames = FALSE)
confint(re[[4]], oldNames = FALSE)
confint(re[[5]], oldNames = FALSE)


# THE END

