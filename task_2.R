library(funModeling)
library(ggplot2)
library(tidyverse)
library(DataExplorer)

# clusters
mydata<-read.csv(choose.files())
View(mydata)

my_data<- mydata[,-1]
View(my_data)

summary(my_data) 
dim(my_data)
str(my_data)

is.na(my_data)
any(is.na(my_data))
df_status(my_data)

# distance matrix
di_mat <- dist(my_data, method = "euclidean") 
di_mat

#### cluster dendrogrm with complete linkage ####
?hclust
fit <- hclust(di_mat, method="complete", members = NULL)
fit
View(fit)

#dendrogram
plot(fit)
#draw clusters
rect.hclust(fit, k=3, border="red")
# cut tree into 3 clusters
groups <- cutree(fit, k=3) 
View(groups)

data <- cbind(groups,mydata)
View(data)


install.packages('pvclust')
library(pvclust)
# Model Based Clustering
library(mclust)
fit_1 <- Mclust(my_data)
plot(fit_1) # plot results
summary(fit_1) # display the best model 
