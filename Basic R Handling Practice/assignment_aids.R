setwd("C:\\Users\\user\\Desktop\\data analytics")
#Usjudgedata analysis

library(dplyr)
library(data.table)

Data_USJUdgeRatings <- data.frame(USJudgeRatings)
# read row name
setName<- setDT(Data_USJUdgeRatings,keep.rownames = TRUE)
class(Data_USJUdgeRatings)

#the average rating and standard deviation in the normal data set layout
local_USJUdgeRatings<-Data_USJUdgeRatings %>%  mutate(Average_Rating=apply(Data_USJUdgeRatings[,-1],1,mean,na.rm=TRUE),Std_Deviation=apply(Data_USJUdgeRatings[,-1],1,sd,na.rm=TRUE))



#Q2a)
aids_data <- read.csv("Aids2.csv",header = T)
# typeof(data1)
# class(data1)
# q <- str(data1)
# data1$sex
# summary(data1)
# View(data1)
# 
# n1 <- nrow(data1)
# n2 <- ncol(data1)
# colnames(data1)
# row.names(data1)
#subset
#Q2b
aids_data_subset <- subset(aids_data,state != "Other")
aids_data_subset <- aids_data_subset %>% filter(!is.na(aids_data_subset$age))

aids_data_subset$agebracket<- ifelse(aids_data_subset$age<20,"0-20", 
                                     ifelse(aids_data_subset$age %in% c(20:40), 
                                        "20-40", ifelse(aids_data_subset$age %in% c(40:60),"40-60",">60")))

##Q2d)
#sorting
aids_data_sort <- aids_data_subset[order(-aids_data_subset$diag, aids_data_subset$death),]
View(aids_data_sort)


#q5e

aids_data_sort <- aids_data_sort %>% filter(!is.na(aids_data_sort$death))%>% 
  mutate(diag= ifelse(is.na(diag),0,diag))
aids_data_sort$dd <- aids_data_sort$diag^2/aids_data_sort$death
View(aids_data_sort)

# View(aids_data_subset)data2 <- data1
# data2$AgeFlag <- ifelse(data2$age>60,1,0)
# View(data2)
# 
# data2$Age_Corrected <- ifelse(data2$age==530,37,data2$age)
# View(data2)
# 


#row and column operation in data fame using index
