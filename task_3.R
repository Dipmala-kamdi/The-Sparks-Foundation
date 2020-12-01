library(ggplot2)
library(tidyverse)
library(corrplot)
library("Hmisc")
library(DataExplorer)
library(funModeling)

# E.D.A.
retail_data<-read.csv(choose.files())
View(retail_data)

# data analysis
dim(retail_data)
summary(retail_data) 

boxplot(retail_data)
plot(retail_data)

any(is.na(retail_data))
plot_str(retail_data)

df_status(retail_data)

# Plot Sales in each state

ggplot(data=retail_data,aes(x=reorder(State,Sales),y=Sales)) + 
  geom_bar(stat ='identity',aes(fill=Sales))+
  coord_flip() + 
  theme_grey() + 
  scale_fill_gradient(name="Sales Level")+
  labs(title = 'Ranking of States by Sales',
       y='Sales',x='State')+ 
  geom_hline(yintercept = mean(retail_data$Sales),size = 1, color = 'blue')

# Plot profit in each state

ggplot(data=retail_data,aes(x=reorder(State,Profit),y=Profit)) + 
  geom_bar(stat ='identity',aes(fill=Profit))+
  coord_flip() + 
  theme_grey() + 
  scale_fill_gradient(name="Profit Level")+
  labs(title = 'Ranking of States by Profit',
       y='Profit',x='State')+ 
  geom_hline(yintercept = mean(retail_data$Profit),size = 1, color = 'blue')

# Plot profit in each Region

ggplot(data=retail_data,aes(x=reorder(Region,Profit),y=Profit)) + 
  geom_bar(stat ='identity',aes(fill=Profit))+
  coord_flip() + 
  theme_grey() + 
  scale_fill_gradient(name="Profit Level")+
  labs(title = 'Ranking of Regions by Profit',
       y='Profit',x='Region')+ 
  geom_hline(yintercept = mean(retail_data$Profit),size = 1, color = 'red')

# Plot Quantity and Region

ggplot(data=retail_data,aes(x=reorder(Region,Quantity),y=Quantity)) + 
  geom_bar(stat ='identity',aes(fill=Quantity))+
  coord_flip() + 
  theme_grey() + 
  scale_fill_gradient(name="Quantity Level")+
  labs(title = ' Regions and Quantity',
       y='Quantity',x='Region')+ 
  geom_hline(yintercept = mean(retail_data$Quantity),size = 1, color = 'red')


# Choose variables

mydata <- retail_data[,c(1,2,7,8,10,11,12,13)]

mydata$Ship.Mode <- as.numeric(mydata$Ship.Mode)
mydata$Segment <- as.numeric(mydata$Segment)
mydata$Region <- as.numeric(mydata$Region)
mydata$Category <- as.numeric(mydata$Category)
mydata$Sales <- as.numeric(mydata$Sales)
mydata$Quantity <- as.numeric(mydata$Quantity)

res <- cor(mydata)
res

res2 <- rcorr(as.matrix(mydata))
res2

# The smaller the p-value, the more significant the correlation.

corrplot(res, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

# create report

create_report(retail_data)
create_report(mydata)
