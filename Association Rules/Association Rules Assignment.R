#To find the different set of association rules for Groceries Dataset using apriori algorithm.
#And also Observe the change in number of rules for different support,confidence and lift values

Groceries <- read.csv(file.choose())
install.packages("arules")
library(arules)
install.packages("arulesViz")
library(arulesViz)
install.packages("dplyr")
library(dplyr)
install.packages("tm")
library(tm)
View(Groceries)
summary(Groceries)
glimpse(Groceries)
str(Groceries)

paste_fun <- function(i){
  return (paste(as.character(i),collapse=" "))
}
Groceries["new_col"] <- apply(Groceries,1,paste_fun)
View(Groceries)
x <- Corpus(VectorSource(Groceries$new_col)) 
x <- tm_map(x,stripWhitespace)
dtm0 <- t(TermDocumentMatrix(x))
dtm0_df <- data.frame(as.matrix(dtm0))


rules <- apriori(Groceries,parameter=list(support=0.01, confidence = 0.3,minlen=5))
rules # For given support, confidence and minlen values, we get 25 rules
plot(rules)
head(quality(rules))
plot(rules, method = "grouped")
plot(rules,method = "scatterplot")
plot(rules,method = "graph") # Plots Graph of 25 rules which has highest lift ratio

rules <- apriori(Groceries,parameter=list(support=0.05, confidence = 0.5,minlen=2,maxlen=4))
rules # For given support, confidence and minlen values, we get 8 rules
plot(rules)
head(quality(rules))
plot(rules, method = "grouped")
plot(rules,method = "scatterplot")
plot(rules,method = "graph") # Plots Graph of 8 rules which has highest lift ratio

rules <- apriori(Groceries,parameter=list(support=0.01, confidence = 0.45,minlen=4))
rules # For given support, confidence and minlen values, we get 115 rules
plot(rules)
head(quality(rules))
plot(rules, method = "grouped")
plot(rules,method = "scatterplot")
plot(rules,method = "graph") # Plots Graph of best 100 rules which has highest lift ratio


#To find the different set of association rules for Movies Dataset using apriori algorithm.
#And also Observe the change in number of rules for different support,confidence and lift values

Movies <- read.csv(file.choose())
View(Movies)
summary(Movies)
glimpse(Movies)
str(Movies)
Movies[] <- lapply(Movies,as.character)
View(Movies)
paste_fun <- function(i){
  return (paste(as.character(i),collapse=" "))
}
Movies["new_col"] <- apply(Movies,1,paste_fun)
View(Movies)
x <- Corpus(VectorSource(Movies$new_col)) 
x <- tm_map(x,stripWhitespace)
dtm1 <- t(TermDocumentMatrix(x))
dtm1_df <- data.frame(as.matrix(dtm1))

rules <- apriori(Movies,parameter=list(support=0.05, confidence = 0.9,minlen=2,maxlen=3))
rules # For given support, confidence and minlen values, we get 6326 rules
plot(rules)
head(quality(rules))
plot(rules, method = "grouped")
plot(rules,method = "scatterplot")
plot(rules,method = "graph")

rules <- apriori(Movies,parameter=list(support=0.5, confidence = 0.95,minlen=2, maxlen=3))
rules # For given support, confidence and minlen values, we get 564 rules
plot(rules)
head(quality(rules))
plot(rules, method = "grouped")
plot(rules,method = "scatterplot")
plot(rules,method = "graph") # Plots Graph of 564 rules which has highest lift ratio
# Lesser the data, more wil be the rules formed.

#To find the different set of association rules for Books Dataset using apriori algorithm.
#And also Observe the change in number of rules for different support,confidence and lift values

Books <- read.csv(file.choose())
View(Books)
summary(Books)
glimpse(Books)
str(Books)

rules <- apriori(as.matrix(Books),parameter=list(support=0.02, confidence = 0.5,minlen=5,maxlen=6))
rules # For the given support, confidence and minimum length, we get 186 rules
head(quality(rules))
plot(rules,method = "scatterplot")
plot(rules,method = "grouped")
plot(rules,method = "graph")

rules <- apriori(as.matrix(Books),parameter=list(support=0.07, confidence = 0.7,minlen=4, maxlen=5))
rules
plot(rules)
head(quality(rules))
plot(rules, method = "grouped")
plot(rules,method = "scatterplot")
plot(rules,method = "graph")

