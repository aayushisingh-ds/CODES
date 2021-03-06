
input <- read.csv("Universities_Clustering.csv")
View(input)
mydata <- input[,(2:7)]
# Load data as mydata
View(mydata)

normalized_data <- scale(mydata) #excluding the university name column before normalizing
View(normalized_data)
d <- dist(normalized_data, method = "euclidean") # distance matrix
fit <- hclust(d, method="complete") 
fit
?hclust
plot(fit) # display dendrogram
plot(fit, hang=-1)
groups <- cutree(fit, k=3)# cut tree into 3 clusters
groups <- cutree(fit, k=4)

?cutree
rect.hclust(fit, k=3, border="red")
?rect.hclust

membership<-as.matrix(groups)
View(membership)

final <- data.frame(input, membership)
View(final)

final1 <- final[,c(ncol(final),1:(ncol(final)-1))]
ncol(final)
View(final1)
final1 <- final[,c(8,1,2,3,4,5,6,7)]
final1

?write.xlsx

write.csv(final1, file="final.csv")

getwd()
setwd()

