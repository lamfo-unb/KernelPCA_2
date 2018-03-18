library(mlRFinance)
data("circle")
plot(circle[,1:2],pch=16)
kpca<-KPCA(circle,"Gaussian",0.5)
plot(kpca,type="l",pch=16)




comp<-as.data.frame(kpca$rotation[,1:2])
plot(comp,pch=16)


comp$Type <- ifelse(comp$PC1< -0.02,1,2)
plot(comp$PC1,comp$PC2,col=comp$Type,pch=16)




circle <- as.data.frame(circle)
circle <- cbind(circle,ifelse(comp$PC1< -0.02,1,2))
plot(circle[,1] ,circle[,2],col=circle[,3],pch=16)
