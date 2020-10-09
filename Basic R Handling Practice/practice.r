a=1:10;
a=c(2,5,7,8,9)
b=rep(2,5)
c=rep(a,length.out=21)

a[4]
a[c(3,4)]
length(c)

d="ram"
d=c("ram","shyam")
d=rep(d,c(3,3))

x=1:10
y=rep("Swagato",10)

xy=cbind(x,y)
xyr=rbind(x,y)

xy=data.frame(x,y)

######Hotel Data Analysis########

df=read.csv("Hotel_Review.csv")

str(df)
summary(df[,c(1:4)])
names(df)
summary(df[,c(10:15)])

is.na(df$Rating_Value)
which(is.na(df$Rating_Value))
df$Rating_Value[which(is.na(df$Rating_Value))]=mean(df$Rating_Value,na.rm=TRUE)

for(i in 10:15){
df[,i][which(is.na(df[,i]))]=mean(df[,i],na.rm=TRUE)}

for(i in 10:15){
m=mean(df[,i])
s=sd(df[,i])
max=m+2*s
min=m-2*s

df=df[-which(df[,i]>max | df[,i]<min),]}