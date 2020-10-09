#The most important library for graphics
library(ggplot2)
library(MASS)
library(plotrix)

#read data
s10 <- read.csv(file = "sachin.csv")
tail(s10, 3)

#quick plots for data exploration
with(s10,plot(Dismissal))

#scatter plot
plot(s10$BF,s10$Runs)
qplot(BF, Runs, data=s10)
#ggplot equivalent
ggplot(s10,aes(x=BF,y=Runs))+geom_point()

#line graph
s10a=s10[order(s10$BF),]
plot(s10a$BF,s10a$Runs, type="l")
qplot(BF, Runs, data=s10a,geom=c("line","point"))
ggplot(s10a,aes(x=BF,y=Runs))+geom_point()+geom_line()

#bar plot
#barplot(factor(s10$result))
qplot(result,data=s10,geom="bar")
ggplot(s10,aes(x=result))+geom_bar()
qplot(factor(result),Runs,data=s10,geom="bar",stat="identity")

#histogram
hist(s10$Runs)
qplot(Runs,data=s10,binwidth=4)
ggplot(s10,aes(x=BF))+geom_histogram(binwidth=4)

#boxplot
plot(s10$Opposition,s10$Runs)#if x is factor
qplot(Opposition,Runs,data=s10,geom="boxplot")
ggplot(s10,aes(x=result, y=Runs))+geom_boxplot()
ggplot(s10,aes(x=result, y=Runs))+geom_violin()

#curve plot
curve(x^2,from=0,to=20)
curve(x^3,add=TRUE,col="red")

#pie chart
dismissal.freq=with(s10,table(Dismissal))
pie(dismissal.freq)
pie(dismissal.freq,col=c("red","yellow","blue",
                         "green","violet","brown","purple","pink"))
pie(dismissal.freq,col=rainbow(8),radius=1,labels=names(dismissal.freq))

#pie chart with annonated values
lbls=paste("\n",names(dismissal.freq),"\n",dismissal.freq,sep="")
pie(dismissal.freq,col=rainbow(8),radius=1,labels=lbls)

#3d pie chart
pie3D(dismissal.freq,col=rainbow(8),explode=0.3,labels=lbls,labelcex=0.8)

#radial pie
radial.pie(dismissal.freq,col=rainbow(8),labels=lbls,show.grid.labels=0)

par(mfrow=c(2,2))
hist(s10$BF,main="Histogram of Balls faced",xlab="Balls faced",ylab="Count")
plot(s10$Opposition,s10$Runs,xlab="opponents",ylab="Runs",main="Boxplot of runs")#if x is factor
hist(s10$Runs,main="Histogram of Runs",xlab="Runs",ylab="Count")
plot(s10$BF,s10$Runs,xlab="balls faced",ylab="Runs",main="Balls vs Runs")


#more graphing options

levels(s10$Dismissal)
#remove rare cases
levels(s10$Dismissal)=c("bowled", "caught", "others","lbw", "not out","others","others","others")
with(s10,plot(Dismissal))
#barchart with color
qplot(Dismissal,geom="bar",data=s10,fill=I("red"))+ylab("number of cases")
#Boxplot
qplot(result,Runs,data=s10,geom="boxplot",fill = I("blue"))+xlab("Types of results")



# a=integer(0)
# for(i in 1:452){
# a[i]=ifelse(s10$result[i]=="won",1,0)
# }
# cor(a,s10$Runs)
# 
# fit=aov(Runs ~ result, data=s10)
# summary(fit)



s10a <- subset(s10, Dismissal %in% c("bowled", "caught", "lbw", "not out"))
s10a$Inns <- factor(s10a$Inns)
qplot(data = s10a, Dismissal, geom = "bar",fill=I("green"))

s10c <- subset(s10, Opposition %in% c("v Australia", "v South Africa", "v New Zealand"))
ggplot(data = s10c,aes(x = BF, y = SR, colour = Opposition)) + geom_point() +
  xlab("Ball Faced") + ylab("Runs Scored") + labs(title = "Ball faced vs Runs Scored")

## Examples of geoms: line,point,box,bar,...
s10b <- subset(s10, result %in% c("won", "lost") & Opposition %in% c("v Australia",
                                                                     "v South Africa", "v New Zealand"))

s10b$result <- factor(s10b$result)
s10b$opp <- factor(s10b$Opposition)

ggplot(s10b, aes(opp, Runs,fill=opp)) + geom_violin()

require(chron)
qplot(x = factor(years(StartDate)), y = Runs, geom = "boxplot", fill = I("green"),
      data = s10b) + xlab("Year")+ylab("Runs scored by sachin")

## Scales

ggplot(data = s10b) + geom_point(aes(x = BF, y = Runs, colour = result)) + scale_colour_manual(values = c(lost = "red",
                                                                                                          won = "green"))

ggplot(data = s10b) + geom_point(aes(x = BF, y = Runs, size = result, shape = result)) +
  scale_size_manual(values = c(lost = 5, won = 3)) + scale_shape_manual(values = c(lost = 15,
                                                                                   won = 25))

ggplot(data = s10b) + geom_point(aes(x = BF, y = Runs, colour = result)) + scale_colour_discrete(name = "Parinaam",
                                                                                                 labels = c(lost = "HAAR", won = "JEET"))


## Facets

s10c = subset(s10, (Opposition %in% c("v Australia", "v England", "v South Africa",
                                      "v Sri Lanka")) & (result %in% c("lost", "won")))

s10c$yr = years(s10c$StartDate)
qplot(data = s10c, factor(Inns), geom = "bar")
qplot(data = s10c, factor(Inns), geom = "bar",fill="red")
qplot(data = s10c, factor(Inns), fill=result, geom = "bar", position="fill")
qplot(data = s10c, factor(Inns), fill = result, geom = "bar", position = "fill",
      facets = Opposition ~ yr)


## Facets Themes

s10a <- subset(s10, Dismissal %in% c("bowled", "caught", "lbw", "not out"))
s10a$Dismissal <- factor(s10a$Dismissal)
s10a$Inns <- factor(s10a$Inns)
qplot(data = s10a, Dismissal, geom = "bar", fill = Inns)
qplot(data = s10a, Dismissal, geom = "bar", fill = Inns)+scale_fill_brewer()
qplot(data = s10a, Dismissal, geom = "bar")+facet_wrap(~ Inns)
qplot(data = s10a, Dismissal, geom = "bar", fill = Inns) + theme_bw() + coord_flip()
qplot(data = s10a, Dismissal, geom = "bar", fill = Inns) + theme(panel.background = element_rect(fill = "lightblue"))
qplot(data = s10a, Dismissal, geom = "bar", fill = Inns) + theme(plot.background = element_rect(fill = "yellow"),
                                                                 panel.background = element_rect(fill = "purple"))


