setwd("/Users/saulgarcia/Desktop/Github/R/Apriori")

###### INTRODUCTION TO APRIORI ######
install.packages("arules")
library(arules)

#ReadingData
#Class Sex Age Survived
train<- read.csv("train.csv")
test<- read.csv("test.csv")
load("titanic.raw.rdata")

str(titanic.raw)

#Apriori
rules <- apriori(titanic.raw)
inspect(rules)

#Rules containing Survived only
rules = apriori(titanic.raw,
                control= list(verbose = F), 
                parameter= list(minlen=2, supp=0.005, conf = 0.8),
                appearance = list( rhs=c("Survived=No",
                                         "Survived=Yes"),
                                       default="lhs"))
#keep decimal places
quality(rules) <- round(quality(rules),3)
rules.sorted = sort(rules, by="lift")

inspect(rules)

#There are Redundant rules, which could be removed.

#find redundant rules
subset.matrix <-  is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag = T)] = NA
redundant <-  colSums(subset.matrix, na.rm = T) >= 1

#Which are redundant
which(redundant)

#Remove redundant rules
rules.pruned <-  rules.sorted[!redundant]

inspect(rules.pruned)

#Rules about Children
rules = apriori(titanic.raw,
                control= list(verbose = F), 
                parameter= list(minlen=3, supp=0.002, conf = 0.2),
                appearance = list( default="none", rhs=c("Survived=Yes"),
                                    lhs = c("Class=1st","Class=2nd","Class=3rd",
                                            "Age=Child","Age=Adult")))
rules.sorted = sort(rules, by="confidence")
inspect(rules.sorted)

#Visualize:
install.packages("arulesViz")
library(arulesViz)
set.seed(111)
plot(rules)
plot(rules, method="graph", control=list(type="items"))
plot(rules, method="grouped")
plot(rules, method="paracoord", control=list(reorder= T))

#Reference:
#http://www.r-bloggers.com/association-rules-and-market-basket-analysis-with-r/