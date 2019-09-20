

train <-read.csv("train.csv", stringsAsFactors = FALSE)
test <- read.csv("test.csv", stringsAsFactors =  FALSE)



sapply(train, function(x){sum(is.na(x))})

sapply(test, function(x){sum(is.na(x))})


train$Survived <- as.factor(train$Survived)
train$Pclass<- as.factor(train$Pclass)

str(train)

table(is.na(train))
rowSums(is.na(train))

sapply(train, function(x){sum(is.na(x)==TRUE)/length(x)})

sapply(test, function(x){sum(is.na(x)==TRUE)/length(x)})


missmap(train, main = "Missing Map")


train$Age[is.na(train$Age)]<-mean(train$Age, na.rm = TRUE)







