library(ggplot2)
library(tidyverse)
library(corrplot)
library("Hmisc")
library(DataExplorer)
library(funModeling)

# E.D.A.
terror_data<-read.csv(choose.files())
View(terror_data)

# data analysis
dim(terror_data)
summary(terror_data) 

boxplot(terror_data)

any(is.na(terror_data))
plot_missing(terror_data)
plot_missing(terror_data, missing_only = TRUE)

df <- terror_data[colSums(is.na(terror_data))/nrow(terror_data)<0.3]
plot_missing(df, missing_only = TRUE)


plot_str(df)

df_1 <-df_status(df)
var_remove <- subset(df_1,df_1$p_zeros > 30)

mydata <- df[,!(names(df) %in% var_remove[,"variable"])]
plot_str(mydata)

mydata <- mydata[,c(2,7,9,13,14,15,16,18,19,20,22,23,27,29,31,32,33,37,38,42,43,45,46,47,48,49,50,51,55,57,65,67,68,69,70,72,73,74,75,76.77)]
# Plot 1

ggplot(data=mydata,aes(x=reorder(iyear,country),y=country)) + 
  geom_bar(stat ='identity',aes(fill=country))+
  coord_flip() + 
  theme_grey() + 
  scale_fill_gradient(name="basic understanding of terror attack")+
  labs(title = 'terror attack',
       y='country',x='iyear')+ 
  geom_hline(yintercept = mean(mydata$country),size = 1, color = 'blue')

# Plot 2

ggplot(data=mydata,aes(x=reorder(region,targtype1),y=targtype1)) + 
  geom_bar(stat ='identity',aes(fill=targtype1))+
  coord_flip() + 
  theme_grey() + 
  scale_fill_gradient(name="region")+
  labs(title = 'plot regions by target type 1',
       y='targtype1',x='region')+ 
  geom_hline(yintercept = mean(mydata$targtype1),size = 1, color = 'blue')

# Plot 3

ggplot(data=mydata,aes(x=reorder(targtype1,weaptype1),y=weaptype1)) + 
  geom_bar(stat ='identity',aes(fill=weaptype1))+
  coord_flip() + 
  theme_grey() + 
  scale_fill_gradient(name="weaptype1 and targtype1")+
  labs(title = 'Ranking of targtype1 by weaptype1',
       y='weaptype1',x='targtype1')+ 
  geom_hline(yintercept = mean(mydata$weaptype1),size = 1, color = 'red')

# Plot 4

ggplot(data=mydata,aes(x=reorder(iyear,attacktype1),y=attacktype1)) + 
  geom_bar(stat ='identity',aes(fill=iyear))+
  coord_flip() + 
  theme_grey() + 
  scale_fill_gradient(name="attacktype1 acoording to year")+
  labs(title = ' attacks type1 and year',
       y='attacktype1',x='iyear')+ 
  geom_hline(yintercept = mean(mydata$attacktype1),size = 1, color = 'red')


# create report

create_report(terror_data)
create_report(mydata)
