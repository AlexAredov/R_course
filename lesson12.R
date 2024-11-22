#task1
library(data.table)
install.packages("coin")
library("coin")
library("boot")
data_arthritis <- read.table("/Users/alex_aredov/Downloads/Arthritis.txt", header = TRUE)
data_arthritis$Treatment = as.factor(data_arthritis$Treatment)
data_arthritis$Sex = as.factor(data_arthritis$Sex)
data_arthritis$Improved = as.factor(data_arthritis$Improved)
partial_tables <- margin.table(table(data_arthritis), c(5,2,3))
mantelhaen.test(partial_tables) #без перес
cmh_test(data_arthritis$Treatment~data_arthritis$Improved | data_arthritis$Sex)#с перес

#task2
data_anorexia <- read.table("/Users/alex_aredov/Downloads/Anorexia.txt", header = TRUE)
data_anorexia
wilcox.test(data_anorexia$Prewt, data_anorexia$Postwt, paired = TRUE) 
wilcoxsign_test(data_anorexia$Prewt~data_anorexia$Postwt) 

#task3
data_thalidomide <- read.table("/Users/alex_aredov/Downloads/thalidomide.txt", header = TRUE)
data_thalidomide$trt = as.factor(data_thalidomide$trt)
data_thalidomide$tb = as.factor(data_thalidomide$tb)
oneway_test(data_thalidomide$wt ~ data_thalidomide$trt | data_thalidomide$tb)

#task4
data_birthwt <- read.table("/Users/alex_aredov/Downloads/birthwt.txt", header = TRUE)
data_birthwt
samplemean<-function(d, i) {mean(d[i])}
mn_1<-boot(data_birthwt$smoke, samplemean, R=1000)
boot.ci(mn_1,type="perc")
mn_1$t0
mn_2<-boot(data_birthwt$ht, samplemean, R=1000)
boot.ci(mn_2,type="perc")
mn_2$t0

