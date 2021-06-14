library(tidyverse)
library(ggplot2)

train <- read.csv("C:/Users/Ajeng Damara Erfatia/Downloads/train (1).csv")
View(train)
test <- read.csv("C:/Users/Ajeng Damara Erfatia/Downloads/test (1).csv")
View(test)

train <- read.csv("../input/titanic/train.csv", stringsAsFactors = FALSE)
test <- read.csv("../input/titanic/test.csv", stringsAsFactors = FALSE)
summary(train)
train$Pclass <- as.factor(train$Pclass)
#showing death based on ticket class
ggplot(train, aes(x = Pclass, fill = factor(Survived))) + geom_bar() + xlab("Ticket Class") + ylab("Total Count") + labs(fill = "Survived")


#create dummy column
test$Survived <- rep("None", length(test))
test <- test[c(1,12,2,3,4,5,6,7,8,9,10,11)]

#Pclass is a ticket class so use it as factor
train$Pclass <- as.factor(train$Pclass)

#plot which class has the most survived passengers
ggplot(train, aes(x = Pclass, fill = factor(Survived)))+ geom_bar() + xlab("Pclass") + ylab("Total Count")

#rowbind both train and test dataset
kombinasi <- rbind(train, test)

#check unique names #as.character because name is in factor
length(unique(as.character(kombinasi$Name)))

#since we got 1307 out 0f 1309, means we have possibly 2 dup names
#find the duplicated names
kombinasi[which(duplicated(as.character(kombinasi$Name))), "Name"]

#extract the title, [2] (check the row content first)
kombinasi$Title <- sapply(kombinasi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
kombinasi$Title

#delete the space before name
kombinasi$Title <- sub(' ', '', kombinasi$Title)

#check the titles first
table(kombinasi$Title)

#so there's quite a lot of titles i'm not familiar with.
#to categorize it i might find out from the connection between age and title, though this is just an assumption. might do further research.
kombinasi$Age[which(kombinasi$Title == 'Master')]
kombinasi$Age[which(kombinasi$Title == 'Miss')]
kombinasi$Age[which(kombinasi$Title == 'Ms')]
kombinasi$Age[which(kombinasi$Title == 'Mme')]
kombinasi$Age[which(kombinasi$Title == 'Mlle')]

#since some titles have different abbreviations, combine them and factor them
kombinasi$Title[kombinasi$Title %in% c('Don', 'Sir', 'Mr' )] <- 'Mr'
kombinasi$Title[kombinasi$Title %in% c('Mme', 'Mlle', 'Dona', 'Lady', 'the Countess', 'Mrs')] <- 'Mrs'
kombinasi$Title[kombinasi$Title %in% c('Miss', 'Ms')] <- 'Miss'
kombinasi$Title[kombinasi$Title %in% c('Col', 'Dr', 'Rev', 'Master', 'Jonkheer', 'Major', 'Capt')] <- 'Others'
kombinasi$Title <- as.factor(kombinasi$Title)

#intro to data science tutorial uses growing matrix and i'll just add it here because imo it's quite useful
#titles <- NULL
#for(i in 1:nrow(kombinasi)){
#titles <- c(titles, extractTitle(kombinasi[i, "None"]))
#}

ggplot(kombinasi[1:891,], aes(x=Title, fill = Survived)) +geom_bar() + facet_wrap(~Pclass) + ggtitle("Ticket class") + xlab("Title") + ylab("Total count") + labs(fill = "Survived")