
setwd("D:/R/work/ARGs subtypes/")
install.packages("vegan")
library(vegan)
install.packages("picante")
library(picante)
library(dplyr)


input="ARGs subtypes.txt"
df<-read.delim(input,header = T,row.names = 1,check.names = F)

Shannon<-diversity(df,index = "shannon",MARGIN = 2,base = exp(1))
Simpson<-diversity(df,index = "simpson",MARGIN = 2,base = exp(1))
Richness<-specnumber(df,MARGIN = 2)

index<-as.data.frame(cbind(Shannon,Simpson,Richness)) 
tdf<-ceiling(as.data.frame(t(df)))
bs_chao_ace<-t(estimateR(t(df)))
obs_chao_ace<-obs_chao_ace[rownames(index),]
index$Chao<-obs_chao_ace[,2]
index$ACE<-obs_chao_ace[,4]
index$Sobs<-obs_chao_ace[,1]
index$Pielou<-Shannon/log(Richness,2)
index$Goods_coverage<-1-colSums(df==1)/colSums(df)
write.table(cbind(simple=c(rownames(index)),index), "diversity_index.txt", row.names = F, sep="\t", quote=F)
index$samples<-rowmames(index)
groups<-read.csv("group")
colnames<-(groups)[1:2]<-c("samples","group")
f2<-merge(index,groups,by="sample")
write.csv(df2,file = "diversity_index.csv")
write.table(index,"diversity_index.txt",row.names = F,sep="\t",quote=F)
