
install.packages("arules") # install the package
library("arules") # invoke the package

install.packages("readxl")
library(readxl)

mydata <- read_xlsx(file.choose(),1)
View(mydata)# Read xlsx file

rules <-  apriori(as.matrix(mydata[,2:7]),parameter=list(support=0.2,confidence=0.7)) 
?apriori

inspect(rules)

inspect(sort(rules, by="lift"))
inspect(head(sort(rules, by="lift")))








