library(ggplot2)
K <- 3
I <- 50
a <-  matrix(c(1,0,0,
               0,1,0,
               0,0,1,
               1,1,0,
               1,0,1,
               0,1,1,
               1,1,1,
               1,0,0,
               0,1,0,
               0,0,1,
               1,1,0,
               1,0,1,
               0,1,1,
               1,1,1,
               1,0,0,
               0,1,0,
               0,0,1,
               1,1,0,
               1,0,1,
               0,1,1,
               1,1,1,
               1,0,0,
               0,1,0,
               0,0,1,
               1,1,0,
               1,0,1,
               0,1,1,
               1,1,1,
               1,0,0,
               0,1,0,
               0,0,1,
               1,1,0,
               1,0,1,
               0,1,1,
               1,1,1), 
             ncol = K, byrow = TRUE)

colnames(a) <- c("K1","K2","K3")
rownames(a) <- paste(rep("I",35),1:35,sep = "")
library(reshape2)
meltA <- melt(a)
meltA$value <- ifelse(meltA$value==1,0,1)
meltA$value <- as.factor(meltA$value)


ggplot(meltA, aes(x = Var1, y = Var2)) + 
  geom_raster(aes(fill=value)) + 
  scale_fill_grey()
