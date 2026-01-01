
setwd("D:/R/work/")
library(openxlsx)
library(plspm)

df1<- read.xlsx("plspm6.xlsx",sep = "\t")

df1<-scale(df1)

df1_blocks <- list(
  pH="pH",
  Soil = c( "AP", "AK","SON","SAK"),
  BC= "BC",
  MGEs = "MGEs",
  ARGs = "ARGs",
  Risk = "Risk"
)
df1_blocks

pH=c(0,0,0,0,0,0)
Soil=c(1,0,0,0,0,0)
BC=c(1,1,0,0,0,0)
MGEs=c(0,1,1,0,0,0)
ARGs=c(0,1,1,1,0,0)
Risk=c(0,0,1,1,1,0)

df1_path<-rbind(pH,
                Soil,
                BC,
                MGEs,
                ARGs,
                Risk)
colnames(df1_path)<-rownames(df1_path)
df1_path

df1_modes<-rep("A",6)
df1_modes

df1_pls<-plspm(df1,df1_path,df1_blocks,modes=df1_modes)
df1_pls
summary(df1_pls)


df1_pls$path_coefs

df1_pls$inner_model

innerplot(df1_pls,colpos = "red",colneg = "blue",show.values = TRUE,lcol = "gray",box.lwd = 0)

df1_pls$gof

df1_pls$inner_summary

df1_pls$effects

df1_pls$outer_model
outerplot(df1_pls,what = "loadings",arr.width = 0.1,colpos = "red",colneg = "blue",show.values=TRUE,lcol = "gray")
outerplot(df1_pls,what = "weights",arr.width = 0.1,colpos = "red",colneg = "blue",show.values=TRUE,lcol = "gray")

df1_pls$scores


