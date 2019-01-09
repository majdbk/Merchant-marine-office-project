getwd()
setwd("C:/Users/dell/testing")

library(xlsx)
port <- read.xlsx(file=file.choose(),sheetIndex=1,row.names="importateur")
port

#**********************************ACP*****************************************#
#pas de varible illustratives qui resulte des autres variables
#pas d'individus supplementaires

#on selectione les varibles quntitative
port.active=port[,1:14]

nrow(port.active)

#origin est une variable Qualitative, le nuage de point n’est pas vraiment un nuage mais une répartition de l’effectif de l'origin en 3 droites.
#type est une variable Qualitative, le nuage de point n’est pas vraiment un nuage mais une répartition de l’effectif du type en 2 droites.
#on a une forte corelation positive entre les varibles des mois
#pour les deux varibles nbmarque et nbagenceoff on peut pas etudier la corelation par rapport au varible de mois qui presente le nombre de voiture importe

pairs(port)
cor(port.active)

#application de l'acp
#on commence tout d'abord par centre et reduire les donne parce qu'on a une difference importante entre les moyenne et les variance des varibles
port.active.cent<- princomp(port.active, cor = T, scores = T)

#recuperation des valeur propre des donne centre et reduite
val.propres<- port.active.cent$sdev^2

#on represente graphiquement la variation des valeurs propres pour choisr k la plus optimale qui maximise la representation des donnes
plot(1:14,val.propres,type="b",ylab="Valeurs propres",xlab="Composante",main="Scree plot")

#interpretation pour la la valeur de k la plus optimale:
Il y a une forte variation entre les valeurs propres des variables 1 et 2.
d'apres le critre de coude et on observant bien l'evolution des valeurs propres on observe un decrochment suivi
d'une  decroisance reguliere a partir de k=2
el le critre de kaiser vient pour confirmer le reslutat vu que les valeur propre qui sont >= 1 sont pour les 
2premiers axes
=>donc par la suite on prendra k=2

# 
library(FactoMineR)
res.pca = PCA(port, scale.unit=TRUE, ncp=2,quali.sup=c(15:16), graph=T)

res.pca$eig
Les deux premières dimensions contiennent 85% de l'inertie totale 
#interpretation sur l'axe1:
pour les varible:
pour les varible des mois qui represnte la quantite des voitures importe l sont tous corole positivement a l'axe1
donc le permier axe oppose les concesio qui ont u taux d'impor important tout au long de 'anne par rapport
au autre concesio qui ont un taux faible d'import par rappor au autre

#interpretation sur l'axe2:
l'axe 2 est represente par les deux varible nbmarque et nombre agence
les deux varible sont assez mal represente sur l'axe2, de plus il sont tout deux orthogonaux a la varible
janvier donc elles sont non significative
La deuxième composante principale n’explique donc aucune information


x11()
plot(res.pca,cex=0.4,shadow=TRUE,choix="ind",axes=c(1,2),habillage=15)

#interpratation des individus:
on s'appercoit que les voiture les plus importe sont majoriteraement d'origin europene
et la classe d'origine americaine est classe entre les deux origine europene et asitique

#une autre representation nous confirme ce qui precede:
plotellipses(res.pca)


#**********************************CAH*****************************************#

#tout d'abord on va centre et reduire les donnes
port.cr <- scale(port.active[,1:12],center=T,scale=T)

#faire la matrice de distance
d <- dist(port.cr,method = "euclidean") 

#on applique l'algrorithle de clasification ascandante
cah <- hclust(d,method="ward.D2") 

#on affiche le d'endograme
plot(cah,hang=-1,cex=0.75) 
rect.hclust(cah,k=3) # choisir nombre de class direct sur le plot
class <- cutree(cah,k=3) # affichage des classe sur la comande

#table de contingence avec lorigin:
table(class,port$origin) #linge*colonne

#interpretation:
taux de clasification pour amerique : 0%
%individu mal classe: 100%

taux de clasification pour asie :37.5%
%individu mal classe:62.5%

taux de clasification pour europe : 55.5%
%individu mal classe:44.5%

=> on a 69% d'individu mal classe sur l'echantillon 
donc on va proceder avec la methode kmeans pour voir si on a une meilleur segmentation qu celle avec cah
vue que le taux d'erreur est trop elevée



#**********************************kmeans*****************************************#

#fonction pour centrer et reduire les colonne
centrage_reduction <- function(x)
{
return((x-mean(x))/sqrt(var(x)))
}

portk.cr <- apply(port[,1:12],2,centrage_reduction)

#application de la methode kmeans
portk.kmeans <- kmeans(portk.cr,centers=3,iter.max=40)
groupekm<-portk.kmeans$cluster

#1ere methode: choix de k par l'evalution de linertie
inertie.expl <- rep(0,times=10)
for (k in 2:10){
clus <- kmeans(portk.cr,centers=k,nstart=5)
inertie.expl[k] <- clus$betweenss/clus$totss
}

plot(1:10,inertie.expl,type="b",xlab="Nb. de groupes",ylab="% inertie expliquée")

#1ere methode: indice de calinski 
library(fpc)
sol.kmeans <- kmeansruns(portk.cr ,krange=2:10,criterion="ch")
plot(1:10,sol.kmeans$crit,type="b",xlab="Nb. de groupes",ylab="Silhouette")

=> d'apres le critre de coude on prendra k=3


#table de contingence avec lorigin:
table(groupekm,port$origin) #linge*colonne

#interpretation:
taux de clasification pour amerique : 100%
%individu mal classe: 0%

taux de clasification pour asie :0%
%individu mal classe:100%

taux de clasification pour europe : 44.5%
%individu mal classe:55.5%



=> on a 77.75% d'individu mal classe sur l'echantillon 

c'est pour cela qu'on gardera la clasifiation par cah et pour confirmer le resulta du cah
on prdedera a la Classification Hiérarchique sur Composantes Principales qui nous donnera les classes
automatiqument






#**********************************classification arbre*****************************************#


dato =port[,c(3,13,14,15,16)]

tree = rpart(origine~., data=dato , minsplit=10)
summary(tree)

plot(tree , uniform = TRUE, branch = 0.5, margin = 0.1)
text(tree , all = FALSE, use.n = TRUE)




#****************************************text mining********************************************#

install.packages("tm")  # pour le text mining
install.packages("SnowballC") # pour le text stemming
install.packages("wordcloud") # générateur de word-cloud 
install.packages("RColorBrewer") # Palettes de couleurs

# Charger
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")


text <- readLines("marque.txt")

# Charger les données comme un corpus
docs <- Corpus(VectorSource(text))
inspect(docs)

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")


# Convertir le texte en minuscule
docs <- tm_map(docs, content_transformer(tolower))
# Supprimer les mots vides anglais
docs <- tm_map(docs, removeWords, stopwords("english"))
# Supprimer votre propre liste de mots non désirés
docs <- tm_map(docs, removeWords, c("veuicule","units","voitres","lm","car","cars","avec accessoires","big va","vopiture","vehicule","vtrs","big van","vehicules","small van","new car","voitures","VOITURES", "VOITURE","voiture","CARS","VEHICULE","VEHICULES","VTRS","SMALL VAN")) 
# Supprimer les ponctuations
docs <- tm_map(docs, removePunctuation)
# Supprimer les espaces vides supplémentaires
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)


dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

#**********************************classification SVM*****************************************#


library(kknn)

app=port [1:15,]
test=port [15:19,]

kknn <- kknn(origine ~ .,train=app,test=test,k =3, distance = 1)# on change le taux de k
summary(kknn)
attributes(kknn)



tableknn = table(test$origine,kknn$fit)








