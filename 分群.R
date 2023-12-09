library(dplyr)

table<-read.csv("C:\\Users\\88698\\OneDrive\\桌面\\機器學習\\學習單(共).csv")

is.na(table)
sum(is.na(table))
#填補遺失值
mean.國文學科能力測驗<-mean(table[,1],na.rm=T)
na.rows<-is.na(table[,1])
table[na.rows,1]<-mean.國文學科能力測驗

mean.過去模擬考中各科通常得到幾級分<-mean(table[,2],na.rm=T)
na.rows<-is.na(table[,2])
table[na.rows,2]<-mean.過去模擬考中各科通常得到幾級分

table(table$擔心國文考不好)
na.rows<-is.na(table[,3])
table[na.rows,3]<-"3"
sum(is.na(table))

table(table$國文科考試時緊張的程度)
na.rows<-is.na(table[,4])
table[na.rows,4]<-"3"
sum(is.na(table))

table(table$我擔心考不好會被父母責怪)
na.rows<-is.na(table[,5])
table[na.rows,5]<-"2"


table(table$我覺得自己準備得很充分)
na.rows<-is.na(table[,6])
table[na.rows,6]<-"從沒有或很少有"
sum(is.na(table))

table(table$我覺得考不好會比別人矮一截)
na.rows<-is.na(table[,7])
table[na.rows,7]<-"常有"
sum(is.na(table))

table <- table %>%
  mutate(
    國文科考試時緊張的程度 = recode(國文科考試時緊張的程度,
                         "非常緊張" = 5,
                         "完全不緊張" = 1,
                         "2" = 2,
                         "3" = 3,
                         "4" = 4
    ),
    
    我覺得自己準備得很充分 = recode(我覺得自己準備得很充分,
                         "從沒有或很少有" = 1,
                         "偶爾" = 2,
                         "常有" = 3,
                         "大半時間或經常如此" = 4
    ),
    
    我覺得考不好會比別人矮一截 = recode(我覺得考不好會比別人矮一截,
                           "從沒有或很少有" = 1,
                           "偶爾" = 2,
                           "常有" = 3,
                           "大半時間或經常如此" = 4
    ),
  )


#轉類別變數 / 連續型變數


table$擔心國文考不好 <- as.factor(table$擔心國文考不好)
table$我擔心考不好會被父母責怪 <- as.factor(table$我擔心考不好會被父母責怪)
table$國文科考試時緊張的程度 <- as.factor(table$國文科考試時緊張的程度)
table$我覺得自己準備得很充分 <- as.factor(table$我覺得自己準備得很充分)
table$我覺得考不好會比別人矮一截 <- as.factor(table$我覺得考不好會比別人矮一截)


#AGNES 演算法
dist<-dist(table, method="manhattan") #other method= manhattan 
dog.hcluster<-hclust(dist, method="average") #other method= single, complete, average, ward.D2
plot(dog.hcluster) #繪製樹狀圖
rect.hclust(dog.hcluster, k=3, border="red")    #分3群標示
cut.h.cluster<-cutree(dog.hcluster, k=3)
cbind(dist,cut.h.cluster) #分群總表
count.cluster<-table(cut.h.cluster) #每群個數表
plot(count.cluster) # 每群個數長條圖

#DIANA 演算法
install.packages("cluster")
library(cluster)
dog.diana<-diana(table, metric="euclidean",stand=TRUE)
dog.diana
plot(dog.diana) #繪製樹狀
rect.hclust(dog.diana, k=3, border="red")    #分3群標示
cut.dog.diana<-cutree(dog.diana, k=3)
cbind(table,cut.dog.diana) #分群總表
count.diana<-table(cut.dog.diana) #每群個數表
plot(count.diana) # 每群個數長條圖




table$擔心國文考不好 <- as.numeric(table$擔心國文考不好)
table$我擔心考不好會被父母責怪 <- as.numeric(table$我擔心考不好會被父母責怪)
table$國文科考試時緊張的程度 <- as.numeric(table$國文科考試時緊張的程度)
table$我覺得自己準備得很充分 <- as.numeric(table$我覺得自己準備得很充分)
table$我覺得考不好會比別人矮一截 <- as.numeric(table$我覺得考不好會比別人矮一截)
#K-Means 演算法
install.packages("factoextra")
library(factoextra)
km.house<-kmeans(table,3,nstart=10)
km.house
cbind(table$district,km.house$cluster) #分群總表
count.km.house<-table(km.house$cluster)#每群個數表
plot(count.km.house) # 每群個數長條圖
fviz_cluster(km.house, data=table, geom=c("point","test"), ellipse.type="norm")

#K-Medoid 演算法
library(cluster)
library(factoextra)
kmedoid.house<-pam(table,k=3)
kmedoid.house
cbind(table$district,kmedoid.house$clustering) #分群總表
count.kmedoid.house<-table(kmedoid.house$clustering)#每群個數表
plot(count.kmedoid.house) # 每群個數長條圖
fviz_cluster(kmedoid.house, data=table, geom=c("point","test"), ellipse.type="norm")



