library(FactoMineR)
library(cluster)
X=ques[,39:49]
d=dist(scale(X),method = "euclidian")
hc1=hclust(d,method = "ward.D") # or complete or average
plot(hc1)
plot(hc1, hang = -1)

# Output hclust
hc1$dist.method
hc1$method
hc1$height
hc1$order## Order des indivuds sur le dendogramme
hc1$merge## Déroulement du processus d'aggrégation.

#2. Déterminer la meilleure partition

library(NbClust)
set.seed(1234)
nc_km <- NbClust(X, min.nc = 2, max.nc = 15, method = "kmeans" )
# On a utilis? 26 crit?res pour calculer la qualit? des classifications
sum(table(nc_km$Best.n[1, ]))

# Afficher le graphique des nombres de classes
barplot(table(nc_km$Best.n[1, ]), xlab = "Number of Clusters", ylab = "Number of Criteria",  main = "Number of Clusters Chosen by 26 Criteria")

cl3 = kmeans(X, centers = 2)  
cl3$cluster
cl3$centers

classif2<-as.hclust(classif)
plot(rev(classif2$height), type="h", ylab="hauteurs")
rect.hclust(hc1,k=2)
classes<-cutree(classif2,k=2)
classes

#Visualiser l'effet coude.Combien de classes peut-on consid?rer ? 

plot(1:40,hc1$height[40:1],type="b") 
## Un presque effet de coude existe en 5 classes.

inertie = sort(hc1$height,decreasing=TRUE)
plot(inertie[1:20],type="s",xlab="Nombre de classes", ylab="Inertie")


#3. Rajouter la classe d'affectation de chaque individu en tant que variable

X.comp<-cbind.data.frame(X, as.factor(classes))
colnames(X.comp)[11]<-"Classe"
X.comp


#4. Description des classes
#   La fonction catdes permet de trier les variables quanti de la plus caractérisante à la moins caractérisante en positif

res.cat=catdes(X.comp, num.var=12)
res.cat
# #proba: the significance threshold considered to characterize the category (by default 0.05)


# description d'une var quali par une var quanti ou quali 

# p-value correspond à la significativité de la différence entre les proportions Mod/Cla et Global 



#5. Représentation des classes sur un plan factoriel


res.pca<-PCA(X.comp)
plot(res.pca,choix="ind",habillage=13)


#6. Représentation des classes sur un plan factoriel : procédure HCPC

res.pca<-PCA(X, ncp=Inf, graph=F)
#res.hcpc<-HCPC(res.pca,consol=FALSE)
res.hcpc<-HCPC(res.pca)

