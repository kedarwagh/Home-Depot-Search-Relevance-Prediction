#install.packages("ggplot2")
#install.packages("labeling")
#install.packages("pander")
#install.packages("dplyr")
#install.packages("lapply")
#install.packages("Greg")

library("ggplot2")
library("labeling")
library("pander")
library("dplyr")
library("lapply")
library("Greg")
rm(list=ls())

df_train = read.csv("C:/Users/Kedar/Home Depot_IndepStudy/Code/train.csv", header = T, encoding = 'ISO-8859-1')

ggplot(df_train, aes(df_train$relevance)) + geom_bar() + labs(title="Frequency of 13 individual relevance scores", x="relevance score" ,y="row count")



mytable <- data.frame(table(df_train$relevance))
names(mytable) <- c("Rel. Score","Frequency")


pander(mytable,caption=attr(mytable, "relFreq"),split.table=60,justify=c("left"),table.alignment="center")



df_train <- df_train[!df_train$relevance %in% c(1.25,1.5,1.75,2.25,2.5,2.75),]

df_train %>% 
  group_by(df_train$product_uid) %>% 
  summarize(count.PUID=n(),avg.Relevance=mean(df_train$relevance,na.rm=TRUE)) %>% 
  ggplot(aes(x=count.PUID,y=avg.Relevance,group=1)) + geom_jitter(alpha=0.2) + scale_y_continuous(breaks = round(seq(1,3,by = 0.333),1)) + labs(title="Average Relevance by count of product_uid")





df_train = cbind(df_train,data.frame(sapply(gregexpr("\\W+", df_train$search_term), length) + 1))
colnames(df_train)[6] = c("count_search_term")

#Create averages for annotating the plot
avgST = round(mean(df_train$count_search_term),2)
avgSTGroup = df_train %>% group_by(as.factor(df_train$relevance)) %>% summarise(groupMean=round(mean(df_train$count_search_term),1))
names(avgSTGroup)=c("relevance","grpMean")

#Create plot, limit y-axis,add overlay boxplot, annotate with meanby group and text. 
plotText<-paste("The mean relevance is ",avgST,"\ny-axis has been reduced.\n max number of  search_terms =",max(df_train$count_search_term))

df_train %>% 
  ggplot(aes(x=factor(df_train$relevance),y=df_train$count_search_term)) +
  geom_jitter(alpha=0.05) +
  coord_cartesian(ylim=c(1,12)) +
  geom_boxplot(color = "red", outlier.colour = NA, fill = NA)+
  geom_text(data = avgSTGroup, aes(x = relevance, y = grpMean, label = grpMean), size = 5, vjust = -0.25,colour="green") +
  annotate("text",label=plotText,x=2,y=11,size=3,colour="black") + 
  labs(title="Average relevance by number search_terms",x="relevance",y="Count of search_terms")
