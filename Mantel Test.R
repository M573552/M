
setwd("D:/R/work/Mantel Test/")
library(openxlsx)

species <- read.xlsx("genus.xlsx", sheet = 1, colNames = TRUE, rowNames = TRUE)

env <- read.xlsx("env.xlsx", sheet = 1, colNames = TRUE, rowNames = TRUE)

library(dplyr)
library(linkET)
library(ggplot2)
mantel01<-mantel_test(species,env,
                      spec_select = list(
                        Acinetobacter ="Acinetobacter",
                        Enterobacter="Enterobacter",
                        Enterococcus="Enterococcus",
                        Klebsiella ="Klebsiella",
                        Pseudomonas= "Pseudomonas"),
                      method="spearman",
                      permutation=999)%>%
  mutate(rd = cut(r, breaks = c(-Inf,0.2,0.4,Inf),
                  labels = c("< 0.2","0.2 - 0.4",">= 0.4")),
         pd = cut(p, breaks = c(-Inf,0.01,0.05,Inf),
                  labels = c("< 0.01","0.01 - 0.05",">= 0.05")))
mantel_plot<-qcorrplot(correlate(env),
          type ="lower", 
          diag =TRUE,
) +
  geom_square() +
  geom_couple(aes(colour = pd, size = rd),data = mantel01, curvature =0.1,
              node.colour = c("blue","blue"),
              node.fill = c("grey","grey"),
              node.size = c(3.5,2.5),
  ) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11,"RdBu"),
                       limits = c(-1,1),
                       breaks = seq(-1,1,0.5)) +
  geom_mark(size=2.5,
            only_mark=T,
            sig_level=c(0.05,0.01,0.001),
            sig_thres=0.05)+
  scale_size_manual(values = c(0.5,1,1.5,2)) +
  scale_colour_manual(values = color_pal(3)) +
  
 
  guides(size = guide_legend(title ="Mantel's r",
                             override.aes = list(colour ="grey35"),
                             order =2),
         colour = guide_legend(title ="Mantel's P",
                               override.aes = list(size =1.5),
                               order =1),
         fill = guide_colorbar(title ="Spearman's r", order =3))

write.table(mantel01,"mantel_results.tsv", sep ="\t", row.names =FALSE)

ggsave("mantel_test.pdf", width =8, height =6, dpi =200)
ggsave("mantel_test.png", width =8, height =6, dpi =300)

library(eoffice)
topptx(mantel_plot, "mantel_test.pptx")
