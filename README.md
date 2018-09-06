# CLARA
library(cluster)
library(ggplot2)
library(purrr)
library(factoextra)
library(plotly)
avg_sil <- function(k){
  km.res <- clara(Y, k)
 ss <- silhouette(km.res$cluster, dist(Y))
 mean(ss[, 3])
}
# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15

# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")
#plot average silhouette width to select the optimuim number of clusters
fviz_nbclust(Y, clara, method = "silhouette")
#clara result
clarax = clara(Y,3)
fviz_cluster(clarax)
# add color
Y1<-data.frame(V1=clarax$data[,1], V2=clarax$data[,2])
Y1$label<-factor(clarax$clustering)
ggplot(data=Y1,aes(x=V1, y=V2, color=label))+
  geom_point()+
  scale_color_manual(breaks = c("1", "2", "3"),values=c("purple", "green", "yellow"))+
  stat_ellipse(geom = "polygon", alpha = 1/4, aes(fill = label))
# reconstruct to an image
P1=matrix(clarax$clustering,nrow = 33,ncol = 193)
plot_ly(z=P1,colors = colorRamp(c("purple","green","yellow")),type="heatmap")



  
  


  

