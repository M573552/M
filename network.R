
setwd("D:/R/work")


library(Hmisc)
library(dplyr)
library(igraph)


NR_data   <- read.csv("NR.csv",   row.names = 1, check.names = FALSE)
ARGs_data <- read.csv("ARGs.csv", row.names = 1, check.names = FALSE)


all_mat <- cbind(NR_data, ARGs_data)  


corr_out <- rcorr(as.matrix(all_mat), type = "spearman")
r <- corr_out$r  
p <- corr_out$P  


r[abs(r) < 0.8] <- 0                
p <- p.adjust(p, method = "BH")
p[p >= 0.01] <- 0                   
p[p < 0.01]  <- 1
z <- r * p                          
View(z)

adj_matrix <- z
diag(adj_matrix) <- 0            


g <- graph_from_adjacency_matrix(as.matrix(adj_matrix),
                                 weighted = TRUE,
                                 mode     = "undirected")
g <- simplify(g)                    
g <- delete_vertices(g, V(g)[degree(g) == 0]) 


E(g)$correlation <- E(g)$weight
E(g)$weight <- abs(E(g)$weight)


edge_list <- as_data_frame(g, what = "edges") %>% 
  rename(source = from, target = to) %>% 
  mutate(cor = sign(correlation))  

write.csv(edge_list, "edge_list_full2.csv", row.names = FALSE)
write.csv(adj_matrix, "adj_matrix_full2.csv", row.names = FALSE)


V(g)$type <- ifelse(V(g)$name %in% colnames(NR_data), "NR", "ARGs")
plot(g,
     vertex.color = factor(V(g)$type),
     vertex.label.cex = 0.6,
     edge.width     = E(g)$weight * 2,
     edge.color     = ifelse(E(g)$cor > 0, "skyblue", "tomato"))

tax <- read.csv('m.csv', row.names = 1, check.names = FALSE, stringsAsFactors = FALSE)
tax <- as.data.frame(tax) 
tax <- tax[as.character(V(g)$name), , drop = FALSE]

V(g)$Phylum <- tax$Phylum  

tax <- tax[rowSums(!is.na(tax)) > 0, ]
D<- tax
print(D)

write.table(D, 'D.txt', 
            sep = '\t', row.names = TRUE, quote = FALSE)

