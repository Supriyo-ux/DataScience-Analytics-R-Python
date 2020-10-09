setwd("C:\\Users\\user\\Desktop\\data analytics")

data1 <- read.csv("Aids2.csv",header = T)
data2 <- data1

data2$age_Flg_col <- 0
for(i in c(1:nrow(data2))){
  
          if(data2[i,7]>46){
            data2[i,8]=1
          }
        else
        {
    data2[i,8]=0
      }
}

