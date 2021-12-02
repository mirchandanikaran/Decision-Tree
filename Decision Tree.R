# This exercise will discuss the decision tree method in R
# Can be applicable for classification and regression
# used for machine learning 
# Read data file
# Define the default folder

setwd("D:/Datasets")
getwd()  # confirm the default folder 


# Import the data from the default folder

data = read.csv("Cardiotocographic.csv", header =T) 
names(data)
head(data)
tail(data)
length(data)
View(data)

# In the dataset the NSP is the target variable 
# and consists of 3 categories 
# 1--> Normal, 2--> Suspect, # 3--> Critical

str(data)

# Since it is a numeric variable, we need to define 
# it as a factor 
data$NSP = as.factor(data$NSP)
# After above command the NSPF will be a factor variable
# Confirm with using str function

str(data)

########################################################

# Step number 2: Partition of Data as per machine learning
# rule. Divide the data into training data and test data

set.seed(1234)
pd= sample(2, nrow(data), 
           replace=TRUE, 
           prob=c(0.8, 0.2))
pd
# pd means partition data. The data is divided into 2 samples
# Training data and test data 
# in the ratio of 80:20

train = data [pd ==1,]
head(train)
length(train$NSP)
tail(train)

# Now let us understand the test data

test = data [pd ==2,]
head(test)
tail(test)
length(test$NSP)

########################################################

# Step 3:  Model development
# Decision tree with party

library(party)
tree1 = ctree(NSPF ~ LB+AC+FM, data=train, 
               controls=ctree_control(mincriterion=0.90, 
                                      minsplit=100))
tree1
plot(tree1)

tree2 = ctree(NSPF ~ LB+AC+FM, data=train, 
              controls=ctree_control(mincriterion=0.99, 
                                     minsplit=400))
plot(tree2)

names(data)

tree3 = ctree(NSPF ~ ASTV+ ALTV+ FM, data=train, 
              controls=ctree_control(mincriterion=0.99, 
                                     minsplit=400))
plot(tree3)

train_subset = train[,c(-22,-23)]
names(train_subset)
dim(train_subset)
tree4 = ctree(NSP ~ ., data=train, 
              controls=ctree_control(mincriterion=0.90, 
                                     minsplit=100))
tree4
plot(tree4)
# Develop the decision tree model with NSPF as the dependent variable
# the LB, AC and FM are independent variables
# We are Developing the model on training data
# Minimum Criteria for significance is 90 %
# Classification takes place with minimun sample size of 200
# if sample size increases the nodes in decision tree decreases.
# There are 17 nodes found in the results
# Decision node and leaf node

print(tree1)

plot(tree1)
plot(tree2)
plot(tree3)
# The root is on the top and the leaves are in bottom
# The most influential varaible comes in the top of the tree

# Step no 4: Predict for validation data
predict(tree1, test, type="prob")
predict(tree1, test)


####################################################################
#Misclassification error
length(predict(tree1))
length(train$NSPF)

tab = table(predict(tree1), train$NSPF)
print(tab)
1-sum(diag(tab))/sum(tab)
sum(diag(tab))/sum(tab)

accuracy = (1304 + 95 + 11)/1718
accuracy

tab = table(predict(tree3), train$NSPF)
print(tab)
1-sum(diag(tab))/sum(tab)
sum(diag(tab))/sum(tab)
accuracy = (1304+ 149 + 48)/1718
accuracy

tab<-table(predict(tree3), test$NSPF)
print(tab)
sum(diag(tab))/sum(tab)



####################################################

# Decision tree with rpart package
library(rpart)
library(rpart.plot)

treeX = rpart(NSPF~LB+AC+FM, train)
rpart.plot(treeX)
rpart.plot(treeX, extra=1)
rpart.plot(treeX, extra=2)
rpart.plot(treeX, extra=3)
rpart.plot(treeX, extra=4)

predict(treeX, test)
tab<-table(predict(tree3), test$NSPF)
print(tab)
sum(diag(tab))/sum(tab)