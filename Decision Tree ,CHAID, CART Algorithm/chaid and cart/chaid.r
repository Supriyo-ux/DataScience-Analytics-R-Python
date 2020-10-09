install.packages("partykit")
install.packages("CHAID", repos="http://R-Forge.R-project.org",type="source")
library(CHAID)

ch <- read.csv("binary.csv")
names(ch)

gcat <- ifelse(ch$gre <= 400, 1, ifelse((ch$gre >= 600) , 3, 2))
table(gcat)

gpacat<-ifelse(ch$gpa>3.39,1,0)

target <- as.factor(ch$admit)
grecat <- as.factor(gcat)
rankcat <-as.factor(ch$rank)
gpacat<-as.factor(gpacat)

df.chaid <-data.frame(target,grecat,rankcat,gpacat)

chaid.tree <-chaid(target~grecat+gpacat+rankcat,data=df.chaid)

plot(chaid.tree)

