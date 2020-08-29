# Multinomial Logit Model
# packages required
require('mlogit')
require('nnet')

#In built dataset
data()
data(Mode)
?Mode
View(Mode)
head(Mode)
tail(Mode)


table(Mode$choice) # tabular representation of the Y categories

?Mode # learn more about the dataset

Mode.choice <- multinom(choice ~ cost.car + cost.carpool + cost.bus + cost.rail + time.car+ time.carpool + time.bus + time.rail, data=Mode)
summary(Mode.choice)

summary(Mode.choice)$coefficients
?pnorm
Mode$choice  <- relevel(Mode$choice, ref= "car")

##### Significance of Regression Coefficients###
z <- summary(Mode.choice)$coefficients / summary(Mode.choice)$standard.errors
z
?pnorm
p_value <- (1-pnorm(abs(z),0,1))*2
?pnorm
p_value
# odds ratio 
exp(coef(Mode.choice))

# predict probabilities
prob <- fitted(Mode.choice)
prob

# Find the accuracy of the model

class(prob)
prob <- data.frame(prob)
View(prob)
prob["pred"] <- NULL
View(prob)

# Custom function that returns the predicted value based on probability
get_names <- function(i){
  return (names(which.max(i)))


pred_name <- apply(prob,1,get_names)
pred_name
?apply
prob$pred <- pred_name
View(prob)
View(prob$pred)

# Confusion matrix
table(pred_name,Mode$choice)
# Accuracy 
mean(pred_name==Mode$choice)

cbind(prob,Mode$choice)
# confusion matrix visualization
barplot(table(pred_name,Mode$choice),beside = T,col=c("red","lightgreen","blue","orange"),legend=c("bus","car","carpool","rail"),main = "Predicted(X-axis) - Legends(Actual)",ylab ="count")

 # 69.31 %
data(iris)
View(iris)
sample_n(iris, 3)
# Split the data into training and test set
set.seed(123)
training.samples <- iris$Species %>% 
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- iris[training.samples, ]
test.data <- iris[-training.samples, ]


# Fit the model
model <- nnet::multinom(Species ~., data = train.data)
# Summarize the model
summary(model)
# Make predictions
predicted.classes <- model %>% predict(test.data)
head(predicted.classes)
# Model accuracy
mean(predicted.classes == test.data$Species)
