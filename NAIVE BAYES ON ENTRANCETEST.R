install.packages("psych")
install.packages("naivebayes")
install.packages("dplyr")
library(naivebayes)
library(dplyr)
library (ggplot2)
library(psych)

Entrance=read.csv(file.choose(),sep=",",header=TRUE)
View(Entrance)
str(Entrance)
summary(Entrance)
head(Entrance)

Entrance$admit=factor(Entrance$admit,levels=c(0,1),labels=c("No","YES"))
Entrance$rank=as.factor(Entrance$rank)
levels(Entrance$admit)


pairs.panels(Entrance[,-1])

Entrance %>% 
  ggplot(aes(x=gre,fill=admit))+geom_density(alpha=0.8,color="black")


set.seed(1234)
ind=sample(2,nrow(Entrance),replace=T,prob=c(0.8,0.2))
View(ind)
Entrance_train=Entrance[ind==1,]
Entrance_test=Entrance[ind==2,]



Entrance_Naive_model=naive_bayes(admit~ gre+gpa,data=Entrance_train)
Entrance_Naive_model


"Entrance_test %>%
#  filter(admit == "YES") %>%
  summarise(mean(gre), sd(gre))"

plot(Entrance_Naive_model)


P=predict(Entrance_Naive_model,Entrance_test,type = 'prob')
P1=predict(Entrance_Naive_model,Entrance_test)

head(cbind(P1,P,Entrance_test$admit))


(tab1=table(P1,Entrance_test$admit))

sum(diag(tab1))/sum(tab1)*100

library(gmodels)
CrossTable(Entrance_test$admit,P1)
