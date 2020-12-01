# Simple Linear Regression 
student_data<-read.csv("http://bit.ly/w-data")
View(student_data)

# data analysis
summary(student_data) 

x <- student_data$Hours
y <- student_data$Scores

plot(x = x, y = y, xlab = "Hours", ylab = "Scores")

barplot(height = y, width = x, xlab = "Hours", ylab = "Scores", main = "bar_plot")

cor(x,y) # to Find Correlation Coefficient (r)

#visualization
ggplot(student_data, aes(x = x, y = y)) + geom_point() + stat_smooth()

model <- lm(Scores ~ Hours, data = student_data)
model

#Regression line

ggplot(student_data, aes(x=x, y=y)) + geom_point() + stat_smooth(method = lm)

#model assesment

summary(model)

confint(model)

sigma(model)*100/mean(student_data$Hours)

Hours <- 9.25

pred <- predict(model,newdata = data.frame(Hours=9.25))
pred
