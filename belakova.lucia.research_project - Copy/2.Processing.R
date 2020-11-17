# load the data set into RStudio
df.credit_card <- read.csv("creditcard.csv", sep = ",", header = TRUE)

# a peek at what our data set looks like in RStudio
# both the first few values and the last few
head(df.credit_card)
tail(df.credit_card)

# As the dataset poster claimed, the individuals categories have been hidden
# due to security issues. However, we do have access to a couple of features
# with a header.
#
# We will now take a closer look at the column headers in this data set.
# I am looking for a specific column that has the Boolean 'fraud = T/F' value
names(df.credit_card)

# The column I was looking for is called 'df.credit_card$Class.
# let's see how much supposed fraud we have here
table(df.credit_card$Class) # 0         1 
                            # 284315    492 

# What percentage of the dataset is fraud really?
paste("Fraud accounts for", 
      round((sum(df.credit_card$Class == 1)/nrow(df.credit_card))*100,
      digits = 3), "% of the data.")
# [1] "Fraud accounts for 0.173 % of the data."

# Fraud is a very small proportion of our data, which is a problem,  
# A finely tuned model is necessary, as we do not want the model to treat the
# few frauds as background noise. In addition, credit card fraud accounts for
# billions of dollars in revenue losses worldwide, so you really don't want to
# mess around when it comes to the precision, sensitivity, specificity, and
# accuracy


# now let's see how much cleaning we have to do here
str(df.credit_card)

# right off the bat, I want the $is_fraud column to be a factor,
# not an integer, so we shall make the conversion
df.credit_card$Class <- as.factor(df.credit_card$Class)
is.factor(df.credit_card$Class) # [1]  TRUE

# i also really don't like that sex is referred to as gender here when it 
# isn't referring to that, so we shall change that and also make it into a 
# factor
names(df.credit_card)[names(df.credit_card) == 'gender'] <- "sex"
df.credit_card$sex <- as.factor(df.credit_card$sex)
is.factor(df.credit_card$sex) # [1] TRUE

# ----------------------------- SPLITTING DATA ---------------------------------

# We will split the data into training data and testing data.
# 80% of the data will be training and then we will test the remaining
# 20% of the data on the models.
set.seed(42069) # set seed so the results are the same across the board
df.credit_card.split <- sample.split(df.credit_card, SplitRatio = 0.8)

train_data <- subset(df.credit_card, df.credit_card.split == TRUE)
test_data <- subset(df.credit_card, df.credit_card.split == FALSE)

print(dim(train_data)) # [1] 220497     31
print(dim(test_data))  # [1] 64310      31
# Both levels of the factor $Class are represented in both datasets!


# Machine learning algorithms prefer scaled data rather than unscaled data.
# scaling can also be called min-max normalization and it attempts to rescale
# the ranges of certain features. 
# If we don't rescale the data, the feature with a higher value
# range starts dominating our desired model for the data at hand.
# In addition, since the range of values of raw data varies widely, in some 
# machine learning algorithms, objective functions do not work correctly
# without normalization/scaling. Therefore, we will now scale our data.
#
# First, we will normalize the training data
train_data$Amount <- scale(train_data$Amount, center = TRUE, scale = TRUE)
#
# and then the testing data.
test_data$Amount <- scale(test_data$Amount, center = TRUE, scale = TRUE)

# Let's see if the normalization went through
print(head(train_data$Amount)) # Check
print(head(test_data$Amount)) # Double check - no more extreme values!

# Since our data is extremely unbalanced, which means that if we simply 
# classified every single transaction as non-fraudulent, we would be right
# more than 98% of the time, which sounds pretty good. But as we have
# previously mentioned, when billions of dollars are at stake,
# that is not good enough. Now, most supervised learning algorithms expect a 
# relatively equal proportion of 0s and 1s, or Not Frauds and Yes Frauds in the 
# training dataset. 
#
# This is best illustrated in a Confusion Matrix
# First, we will make a logistic regression model for the training data
# which is the fundamental aspect of building a Confusion Matrix.
logistic.model <- glm(Class~.,family = binomial(link = "logit"), data = train_data)
summary(logistic.model) # The glm() function is able to perform many
                        # generalized linear models.
                        # 'Binomial' in this case means that we want to perform 
                        # a Logistic Regression, as opposed to some other type 
                        # of glm.

# Since a confusion matrix is a summary of prediction results in a
# classification model, we have to first make some predictions.
prediction_1 <- predict(logistic.model, test_data, "response")
prediction_2 <- ifelse(prediction_1 < 0.5,0,1) 
                # 0 and 1 are just the fraud binary
table(prediction_2)

# And now we can actually make a Confusion Matrix! Wee!
confusionMatrix(table(prediction_2, reference = test_data$Class))
#
# We can see in the Confusion Matrix that the accuracy is 0.9992, 
# but when we look at the 'Specificity',
# we can see that Specificity : 0.6186
# Specificity is also called the true negative rate and is 
# calculated as the number of correct negative predictions divided by the total
# number of negatives. So since this value is quite low, this would mean that 
# we have a high number of false positives and a low number of true negatives.
# Not horrible, but not great, especially if we introduce any differences to 
# the test set.

# We can also plot an ROC curve which shows the performance of a classification
# model at all classification thresholds (aka where the decision is made about
# which transaction is fraudulent and which one is not).
pdf("2.visualizations/ROC-1.pdf", width = 8, height = 8, paper = 'special')

roc(response = test_data$Class, predictor = prediction_2, plot = TRUE,
    col = "#800020", legacy.axes = TRUE, xlab = "False Positive Percentage",
    ylab = "True Positive Percentage", percent = TRUE, print.auc = TRUE, 
    lwd = 4)
      # Area under the curve: 80.92%

legend("bottomright", legend = c("ROC", "TPR = FPR"), fill = c("#800020",
      "grey"), cex = 0.8, title = "Legend", text.font = 3, bg = "white")

dev.off()

# The area under the ROC curve (AUC under ROC) measures the usefulness of an
# algorithm, with a greater area indicating a very useful test.
# 0.8092 is not particularly high. In the case of credit card fraud, we are 
# looking for an AUC under ROC of 0.95 and more.


# So since our data is quite unbalanced but we are hard-pressed to find more 
# out there, we can use a technique called 'ROSE' or Randomly Over Sampling 
# Examples, which balances data by generating synthetic balanced samples.
# The idea is that we want a similar amount of frauds and non-frauds in both
# the training and testing datasets
df.credit_card.rose <- ROSE(Class~.,data = df.credit_card, N = 1000, p = 0.5)
table(df.credit_card_rose$data$Class)
df.credit_card.rose <- df.credit_card.rose$data
##   0   1 
## 497 503 
#
# That is more on par. Now we can split this into testing and training 
# datasets.
set.seed(69)
df.credit_card.rose.split <- sample.split(df.credit_card.rose, SplitRatio = 0.8)

train_data_rose <- subset(df.credit_card.rose,
                          df.credit_card.rose.split == TRUE)
test_data_rose <- subset(df.credit_card.rose,
                         df.credit_card.rose.split == FALSE)

print(dim(train_data_rose)) 
print(dim(test_data_rose))  

# Now we can do the same logistic regression model on our new training data
logistic.model.rose <- glm(Class~.,family = binomial(link = "logit"),
                           data = train_data_rose)
summary(logistic.model.rose)

prediction_3 <- predict(logistic.model.rose, test_data_rose, "response")
prediction_4 <- ifelse(prediction_3 < 0.5,0,1)
table(prediction_4)

confusionMatrix(table(prediction_4, reference = test_data_rose$Class))
                # Now, with the new data, there is a good tradeoff between 
                # sensitivity and specificity (0.9658 and 0.8649, respectively)

# We can plot another ROC curve for this model
pdf("2.visualizations/ROC-2.pdf", width = 8, height = 8, paper = 'special')

roc(response = test_data_rose$Class, predictor = prediction_4, plot = TRUE,
    col = "#000080", legacy.axes = TRUE, xlab = "False Positive Percentage",
    ylab = "True Positive Percentage", percent = TRUE, print.auc = TRUE,
    lwd = 4)
# Area under the curve: 91.53% - much better than before, when it was 80.92%

legend("bottomright", legend = c("ROC", "TPR = FPR"), fill = c("#000080", "grey"),
       cex = 0.8, title = "Legend", text.font = 3, bg = "white")

dev.off()

# We can now overlay the two graphs to really see the difference
pdf("2.visualizations/ROC-3.pdf", width = 8, height = 8, paper = 'special')

roc(response = test_data$Class, predictor = prediction_2, plot = TRUE,
    col = "#800020", legacy.axes = TRUE, xlab = "False Positive Percentage",
    ylab = "True Positive Percentage", percent = TRUE, print.auc = TRUE, 
    lwd = 4, print.auc.y = 30)

plot.roc(test_data_rose$Class, predictor = prediction_4, percent = TRUE, col = "#000080", print.auc = TRUE,
    add = TRUE, print.auc.y = 35, lwd = 4)

legend("bottomright", legend = c("ROC Before ROSE", "ROC After ROSE",
                                 "TPR = FPR"), fill = c("#800020", "#000080",
                                  "grey"), cex = 0.8, title = "Legend",
                                  text.font = 3, bg = "white")

dev.off()

# We can now follow similar steps to figure out whether a Random Forest model 
# will give us a more accurate model for detecting credit card fraud.
# For the sake of completion, we can also build a model on the unbalanced data
# as well as the data balanced by ROSE, and comparing the two.
#
# Let's start with building a Random Forest for the unbalanced data first
set.seed(420)

pdf
train_data_rf <- randomForest(Class~.,data = train_data, proximity = TRUE, ntree = 100, importance = T)


