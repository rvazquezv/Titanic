library(dslabs)
library(tidyverse)
library(caret)
data("tissue_gene_expression")
dim(tissue_gene_expression$x)


##########################Q1

X <- tissue_gene_expression$x[,1:500] %>% as.matrix()
d<-dist(X)
image(as.matrix(d), col = rev(RColorBrewer::brewer.pal(9, "RdBu")))
cor(X)
pca <- prcomp(X)
summary(pca)


data.frame(pca$x[,1:2], tissue=tissue_gene_expression$y) %>% 
  ggplot(aes(PC1,PC2, fill = tissue))+
  geom_point(cex=3, pch=21) +
  coord_fixed(ratio = 1)

##########################Q2

pca$sdev
pc <- 1:length(tissue_gene_expression$y)
qplot(pc, pca$sdev)

summary(pca)$importance[,1:10] 

avgs <- rowMeans(tissue_gene_expression$x)
data.frame(pc_1 = pca$x[,1], avg = avgs, 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(avgs, pc_1, color = tissue)) +
  geom_point()
corr <- cor(avgs, pca$x[,1])


##########################Q3



x <- with(tissue_gene_expression, sweep(x, 1, rowMeans(x)))
pca <- prcomp(x)
data.frame(pc_1 = pca$x[,1], pc_2 = pca$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()



##########################Q4

df<-data.frame(pc_1 = pca$x[,1], pc_2 = pca$x[,2],pc_3 = pca$x[,3],pc_4 = pca$x[,4],pc_5 = pca$x[,5],pc_6 = pca$x[,6],pc_7 = pca$x[,7], 
           pc_8 = pca$x[,8],pc_9 = pca$x[,9],pc_10= pca$x[,10],tissue = tissue_gene_expression$y) 
  

df %>% ggplot(aes(pc_7,color = tissue)) +
  geom_boxplot()

for(i in 1:10){
  boxplot(pca$x[,i] ~ tissue_gene_expression$y, main = paste("PC", i))
}



##########################Q5



summary(pca)$importance[,1:5]

plot(summary(pca)$importance[3,])


##########################Q-C1

d <- dist(tissue_gene_expression$x - rowMeans(tissue_gene_expression$x))



##########################Q-C2


h <- hclust(d)

plot(h, cex = 0.65, main = "", xlab = "")


##########################Q-C3


library(RColorBrewer)
sds <- matrixStats::colSds(tissue_gene_expression$x)
ind <- order(sds, decreasing = TRUE)[1:50]
colors <- brewer.pal(7, "Dark2")[as.numeric(tissue_gene_expression$y)]
heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = colors)





heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = colors)
heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = rev(colors))
heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = sample(colors))
heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = sample(colors))



