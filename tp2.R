operateur<-factor(c(rep("TT",7), rep("OO",4), rep("OR",4)))
region<-factor(c("N", "C", "N", "S", "S", "C", "N",
                 "S", "C", "C", "C", "S", "S", "C", "S"))
don<-cbind.data.frame(operateur,region)

# Tableau crois´e
tab<-table(don$operateur,don$region)
# Tableaux des fr´equences, profils-lignes et profils-colonnes
f<-round(100*tab/sum(tab),1)
pl<-round(100*prop.table(tab,margin=1),1)
pc<-round(100*prop.table(tab,margin=2),1)

# Test du chi-2
test<-chisq.test(tab)
test

library(FactoMineR)

tab<-matrix(c(908,869,901,619,1307,1008,1035,612,73,107,80,177,642,408,140,
              209,360,336,311,298,435,494,504,281), ncol=6)

rownames(tab)<-c("Prim","Second","Tech","Sup")
colnames(tab)<-c("Radio","T´el´e","QuotNat", "QuotReg", "PrMag","PrTV")
res.ca<-CA(tab)
barplot(res.ca$eig[,2], names=paste("Dim",1:nrow(res.ca$eig)))
round(res.ca$eig,3)
round(cbind(res.ca$row$coord[,1:2],res.ca$row$cos2[,1:2]),2)
plot(res.ca, invisible="col")
round(cbind(res.ca$col$coord[,1:2],res.ca$col$cos2[,1:2]),2)
plot(res.ca, invisible="row")

