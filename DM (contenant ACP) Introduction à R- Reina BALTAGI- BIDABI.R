setwd("D:/Assignements")
install.packages("readxl")
library(readxl)
perf_envi<-read_excel("variables_EuroStoxx600.xlsx")
summary(perf_envi)
str(perf_envi)
EPS<-perf_envi$`Environment Pillar Score`
EMP<-perf_envi$EMPLOYEES
MARGIN<-perf_envi$`OPERATING PROFIT MARGIN`
SALES<-perf_envi$`NET SALES OR REVENUES`
MCAP<-perf_envi$`Market Cap(Σ=Avg)`
RCAP<-perf_envi$`RETURN ON INVESTED CAPITAL`
BOARD<-perf_envi$`Value - Board Structure/Independent Board Members`
perf_envi$EPS<-as.numeric(perf_envi$`Environment Pillar Score`)
perf_envi$EMP<-as.numeric(perf_envi$EMPLOYEES)
perf_envi$MARGIN<-as.numeric(perf_envi$`OPERATING PROFIT MARGIN`)
perf_envi$SALES<-as.numeric(perf_envi$`NET SALES OR REVENUES`)
perf_envi$MCAP<-as.numeric(perf_envi$`Market Cap(Σ=Avg)`)
perf_envi$RCAP<-as.numeric(perf_envi$`RETURN ON INVESTED CAPITAL`)
perf_envi$BOARD<-as.numeric(perf_envi$`Value - Board Structure/Independent Board Members`)
any(is.na(perf_envi))
perf_envi<-na.omit(perf_envi)
any(is.na(perf_envi))
#Question 3
#Dataviz, statistiques descriptives et régression linéaire
#Les statistiques descriptives et Les graphes
#Il est important de préciser qu'il existe des variables qualitatives et d'autres quantitatives
#Commençons en premier avec les variables qualitatives!
#Avec la fonction table, il est possible de voir les modalités des variables qualitatives ainsi que leurs fréquences. 
table(perf_envi$`COUNTRY OF DOMICIL`)
table(perf_envi$`CSR Sustainability Committee`)
#Il est également possible de voir les pourcentages d'entreprises dans chaque pays
#Ainsi que le pourcentage des CSR dans les entreprises
prop.table(table(perf_envi$`COUNTRY OF DOMICIL`))
prop.table(table(perf_envi$`CSR Sustainability Committee`))
summary(perf_envi$`CSR Sustainability Committee`)
summary(perf_envi$`COUNTRY OF DOMICIL`)
library(ggplot2)
ggplot(data=perf_envi, aes(x=`COUNTRY OF DOMICIL`))+ geom_bar()+ ggtitle("Répartition des entreprises dans les pays")
ggplot(data=perf_envi, aes(x = "", fill = `CSR Sustainability Committee`)) +
  geom_bar(width = 1) + coord_polar(theta = "y") +
  labs(title = "Présence d'un Comité de Durabilité CSR dans les Entreprises")
#Pour avancer dans les analyses, notamment en arrivant à la régression linéaire (Q4), il est essentiel de transformer ces variables qualitatives en factors
perf_envi$CSR<- factor(perf_envi$`CSR Sustainability Committee`, levels = c("Y", "N"),labels=c("Yes","No"))
perf_envi$COD<- factor(perf_envi$`COUNTRY OF DOMICIL`, levels=c("BD","BG","BM","DK","ES","FA","FN","FR","IM","IR","IT","LX","MA","NL","NW","OE","PO","PT","SD","SW","UK"))
CSR<-perf_envi$CSR
COD<-perf_envi$COD
table(CSR)
table(COD)
# Diagramme en barres pour la variable CSR Sustainability Committee
ggplot(perf_envi, aes(x = CSR)) + 
  geom_bar() + 
  labs(title = "Distribution de CSR Sustainability Committee", 
       x = "CSR Sustainability Committee", 
       y = "Fréquence")
#CSR Selon Pays
ggplot(perf_envi, aes(x = `CSR Sustainability Committee`, fill = `COUNTRY OF DOMICIL`)) + 
  geom_bar(position = "stack") + 
  labs(title = "Répartition de CSR Sustainability Committee selon le Pays")

#CSR selon les Pays
ggplot(perf_envi, aes(x = COD, fill = CSR)) + 
  geom_bar(position = "stack") + 
  labs(title = "Répartition de CSR Sustainability Committee selon le Pays", 
       x = "Pays", 
       y = "Nombre")
#Ce croisement des deux variables qualitatives dans les graphiques
#Ce même croisement peut être visualiser statistiquement, à savoir par des tests de corrélations ou des tableaux de contingence
#Vu que les variables sont qualitatives, Nous ne pouvons pas calculer la corrélation, mais nous pouvons réaliser un tableau de contingence
CSR_COD=table(CSR,COD)
CSR_COD
prop.table(CSR_COD)
round(prop.table(CSR_COD, margin = 2), 2)
round(prop.table(CSR_COD,margin=1),2)
#Calcul du Chi2
test_Chi2<-chisq.test(CSR_COD)
test_Chi2
test_Chi2$residuals
mosaicplot(CSR_COD,shade=TRUE,las=3)
# Calcul du V de Cramer
install.packages("vcd")
library(vcd)
assocstats(CSR_COD)
install.packages("car")
library(car)

perf_envi$CSR <- as.factor(perf_envi$CSR)
perf_envi$COD <- as.factor(perf_envi$COD)

model <- lm(EPS ~ CSR + COD, data = perf_envi)

# Calcul VIF
vif_values <- vif(model)
print(vif_values)
#En se basant sur ce Test VIF, il n'y a pas multicolinéarité
# Il sera intéressant de croiser ainsi les variables qualitatives chacune avec celle dépendante 

#Les Variables Quantitatives

summary(perf_envi)

#La normalité des variables 
#Histogrammes avec fonction de densité pour visualiser la normalité de chaque variable seule

# Histogramme et fonction de densité pour EPS
hist(perf_envi$EPS, prob = TRUE, col = "cornflowerblue", xlim = c(min(perf_envi$EPS), max(perf_envi$EPS)), main = "Histogramme et fonction de densité - EPS", xlab = "Environment Pillar Score (EPS)", ylab = "Densité")
lines(density(perf_envi$EPS, na.rm = TRUE), lwd = 2, col = "orange")
#Pour s'assurer des résultats de l'hist
qqnorm(perf_envi$EPS, main = "QQ Plot - Environment Pillar Score (EPS)", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
qqline(perf_envi$EPS, col = "orange", lwd = 2) 

# Histogramme et fonction de densité pour MARGIN
hist(perf_envi$MARGIN, prob = TRUE, col = "lightgreen", xlim = c(min(perf_envi$MARGIN), max(perf_envi$MARGIN)), main = "Histogramme et fonction de densité - MARGIN", xlab = "Operating Profit Margin", ylab = "Densité")
lines(density(perf_envi$MARGIN, na.rm = TRUE), lwd = 2, col = "red")
#Pour s'assurer des résultat de l'hist
qqnorm(perf_envi$MARGIN, main = "QQ Plot - Operating Profit Margin", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
qqline(perf_envi$MARGIN, col = "red", lwd = 2)


# Histogramme et fonction de densité pour SALES
hist(perf_envi$SALES, prob = TRUE, col = "skyblue", xlim = c(min(perf_envi$SALES), max(perf_envi$SALES)), main = "Histogramme et fonction de densité - SALES", xlab = "Net Sales or Revenues", ylab = "Densité")
lines(density(perf_envi$SALES, na.rm = TRUE), lwd = 2, col = "purple")
#Pour s'assurer des résultats 
qqnorm(perf_envi$SALES, main = "QQ Plot - Net Sales or Revenues", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
qqline(perf_envi$SALES, col = "purple", lwd = 2)

# Histogramme et fonction de densité pour BOARD
hist(perf_envi$BOARD, prob = TRUE, col = "lightcoral", xlim = c(min(perf_envi$BOARD), max(perf_envi$BOARD)), main = "Histogramme et fonction de densité - BOARD", xlab = "Value - Board Structure/Independent Board Members", ylab = "Densité")
lines(density(perf_envi$BOARD, na.rm = TRUE), lwd = 2, col = "darkblue")
#Pour s'assurer des résultats de l'hist
qqnorm(perf_envi$BOARD, main = "QQ Plot - Board Structure/Independent Board Members", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
qqline(perf_envi$BOARD, col = "darkblue", lwd = 2)

#Le QQ-Plot qui montre également la normalité de la distribution des variables
# QQ-Plot pour EPS
qqnorm((perf_envi$EMP - mean(perf_envi$EMP)) / sd(perf_envi$EMP), main = "QQ Plot - EMP")
qqline((perf_envi$EMP - mean(perf_envi$EMP)) / sd(perf_envi$EMP))  # Ajoute une ligne de référence normale

#Pour plus d'explication: Test de normalité

# Test de Jarque-Bera
install.packages("normtest")
library(normtest)
jb.norm.test(perf_envi$RCAP)  

# Test de Kolmogorov-Smirnov
ks.test(perf_envi$MCAP, "pnorm", mean = 1, sd = 1) 

#Matrice de Corrélation entre les Variables Quantitatives

cor_data <- perf_envi[, c("EPS", "EMP", "MARGIN", "SALES", "MCAP", "RCAP", "BOARD")]
#Permet de sélectionner les variables quantitatives spécifiques du data frame
# Calculer la matrice de corrélation
cor_matrix <- cor(cor_data, use = "complete.obs")  # "complete.obs" exclut les NA
heatmap(cor_matrix, main="Matrice de Corrélation des Variables Quantitatives", col=heat.colors(10), scale="column")
print(cor_matrix)


#Croisement de la Variable expliquée avec les variables explicatives

# Scatter plot entre EPS et EMP avec la ligne de régression
plot(perf_envi$EPS, perf_envi$EMP, main="Relation entre EPS et Employment", 
     xlab="Environment Pillar Score (EPS)", ylab="Employment (EMP)", col="blue", pch=16)
abline(lm(perf_envi$EMP ~ perf_envi$EPS), col="red")  # Ajoute une ligne de régression

# Scatter plot entre EPS et MARGIN avec la ligne de régression
plot(perf_envi$EPS, perf_envi$MARGIN, main="Relation entre EPS et Operating Profit Margin", 
     xlab="Environment Pillar Score (EPS)", ylab="Operating Profit Margin (MARGIN)", col="blue", pch=16)
abline(lm(perf_envi$MARGIN ~ perf_envi$EPS), col="red")  # Ajoute une ligne de régression

# Scatter plot entre EPS et SALES avec la ligne de régression
plot(perf_envi$EPS, perf_envi$SALES, main="Relation entre EPS et Net Sales", 
     xlab="Environment Pillar Score (EPS)", ylab="Net Sales (SALES)", col="blue", pch=16)
abline(lm(perf_envi$SALES ~ perf_envi$EPS), col="red")  # Ajoute une ligne de régression

# Scatter plot entre EPS et MCAP avec la ligne de régression
plot(perf_envi$EPS, perf_envi$MCAP, main="Relation entre EPS et Market Cap", 
     xlab="Environment Pillar Score (EPS)", ylab="Market Cap (MCAP)", col="blue", pch=16)
abline(lm(perf_envi$MCAP ~ perf_envi$EPS), col="red")  # Ajoute une ligne de régression

# Scatter plot entre EPS et RCAP avec la ligne de régression
plot(perf_envi$EPS, perf_envi$RCAP, main="Relation entre EPS et Research Capital", 
     xlab="Environment Pillar Score (EPS)", ylab="Research Capital (RCAP)", col="blue", pch=16)
abline(lm(perf_envi$RCAP ~ perf_envi$EPS), col="red")  # Ajoute une ligne de régression

# Scatter plot entre EPS et BOARD avec la ligne de régression
plot(perf_envi$EPS, perf_envi$BOARD, main="Relation entre EPS et Board Size", 
     xlab="Environment Pillar Score (EPS)", ylab="Board Size (BOARD)", col="blue", pch=16)
abline(lm(perf_envi$BOARD ~ perf_envi$EPS), col="red")  # Ajoute une ligne de régression

#Pour voir s'il y a relation
anova_result <- aov(EPS ~ CSR, data = perf_envi)
summary(anova_result) 
anova_result2<-aov(EPS~COD,data=perf_envi)
summary(anova_result2)
#Des graphiques montrant ces relations
library(ggplot2)
# Boxplot pour CSR
ggplot(perf_envi, aes(x = CSR, y = EPS, fill = CSR)) +
  geom_boxplot() +
  labs(title = "Boxplot de EPS en fonction de CSR",
       x = "CSR",
       y = "EPS") +
  theme_minimal()
# Boxplot pour COD
ggplot(perf_envi, aes(x = COD, y = EPS, fill = COD)) +
  geom_boxplot() +
  labs(title = "Boxplot de EPS en fonction de COD",
       x = "COD",
       y = "EPS") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))# Rotation des labels pour mieux les lire
#Pour mieux comprendre ces relations entre variables explicatives et celle expliquée, il est primordial d'établir une régression linéaire

#Régression Linéaire
mod<-lm(EPS~BOARD+COD+CSR+EMP+MARGIN+MCAP+RCAP+SALES , data=perf_envi)
summary(mod)
#Les variables EMP (Employees) et RCAP sont non significatif selon la P-value (Voir rapport)
#On ne peut pas savoir la significativité de la variable qualitative dans sa globalité
#La raison est que les coefficient sont par rapport à la modalité de référence et non par par rapport à Y
#C'est ainsi qu'on peut tester la significativité de COD et CSR par le biais de la fonction drop1()
#Toutefois, avant il essentiel de créer 2 modèles de régression linéaire simple contenant chacun une des deux variables qualitatives explicative avec celle expliquée 
mod_quali1<-lm(EPS~CSR,data=perf_envi)
summary(mod_quali1)
drop1(mod_quali1, .~., test = "F")
mod_quali2<-lm(EPS~COD,data=perf_envi)
summary(mod_quali2)
drop1(mod_quali2,.~.,test="F")
#Les deux sont significative donc on doit les retenir dans le modèle final
#Le modèle sera ainsi: 
mod2<-lm(EPS~BOARD+COD+CSR+MARGIN+MCAP+SALES, data=perf_envi)
summary(mod2)
#On trouve ainsi que MCAP n'est plus significatif 
mod3<-lm(EPS~BOARD+COD+CSR+MARGIN+SALES, data=perf_envi)
summary(mod3)
plot(mod3,which=1:5)
#Pour la partie des ACP à savoir la question 2
#On commence par installer les packages demandés pour utiliser l'ACP
install.packages("FactoMineR")
library(FactoMineR)
install.packages("corrplot")
library(corrplot)
install.packages("factoextra")
library(factoextra)
library(ggplot2)
#Dans cette étape nous devrions séléctionner les variables sur lesquelles on va faire l'ACP, ces variables doivent être quantitatives 
#De plus, le na.omit nous permettera de supprimer les valeurs manquantes, comme l'ACP ne travaille pas avec des variables manquantes
var_acp<- na.omit(perf_envi[,c(5:12)])
summary(var_acp) # On s'assure que les variables sélectionner sont correctes
str(var_acp)
#Avant de faire l'ACP il est primordial de regarder la corrélation entre les variables en question 
#l'ACP lui même repose sur cette corrélation d'où l'importance de cette étape 
# Calculer la matrice de corrélation
matrice_correlation <- cor(var_acp, use = "complete.obs")
print(matrice_correlation)
#Test de corrélation 
bartlett_test <- bartlett.test(var_acp)
print(bartlett_test)
# Créer la heatmap de corrélation avec des coefficients plus visibles
corrplot(
  matrice_correlation, 
  method = "color", 
  type = "full", 
  order = "hclust", 
  tl.col = "black", 
  tl.cex = 0.5, 
  tl.srt = 30, 
  addCoef.col = "black", 
  cl.pos = "r", 
  cl.cex = 1, 
  addCoefasPercent = TRUE, 
  number.cex = 0.8
)
#Après avoir tester les corrélation entre les variables, on va maintenant réaliser l'ACP
#Il est important de noter que pour réaliser l'ACP, les données doivent être centrées et réduites
#Pour faire cela, nous pouvons soit le faire en deux étapes avec scale, soit en une seule étape avec scale.unit= T dans le code de l'ACP 
#Nous allons la faire en 2 étapes
acp_var_centrees_reduites<-scale(var_acp,center = TRUE, scale= TRUE)
resultats_acp<-PCA(acp_var_centrees_reduites, graph = F)
# OU méthode 2: resultats_acp<-PCA(var_acp,scale.unit=TRUE, graph=F)
print(resultats_acp$eig)
print(resultats_acp$var)
#Pour pouvoir savoir quelles composantes seront retenues, nous devrions voir les valeurs propres 
valeurspropres <- resultats_acp$eig
valeurspropres
#Puisque les 2 premières composantes expliquent déjà 66% de la variance ils seront choisis
barplot(valeurspropres[, 2], names.arg=1:nrow(valeurspropres),
        main = "Pourcentage de la variance expliquée par chaque
composante",
        xlab = "Composantes principales",
        ylab = "Pourcentage de variance expliquée",
        col ="steelblue")
# Pour ajouter une ligne qui connecte les barre du plot 
lines(x = 1:nrow(valeurspropres), valeurspropres[, 2],
      type="b", pch=19, col = "red")
fviz_eig(resultats_acp, addlabels= T) #ça fait apparaître les pourcentages 
dimdesc(resultats_acp, axes = c(1, 2, 3)) #Description des variables en fonction de leurs contributions aux trois axes

#Pour la contribution des variables 
#Contribution des variables à la première composante
fviz_contrib(resultats_acp, choice = "var", axes = 1, top = 8)
# Contribution des variables à la deuxième composante 
fviz_contrib(resultats_acp, choice = "var", axes = 2, top = 8)

# Créer le graphique du cercle de corrélation
library(factoextra)

# Créer le cercle de corrélation avec des étiquettes plus petites
fviz_pca_var(resultats_acp,
             col.var = "cos2", # Utiliser la qualité de représentation (cos2) pour la couleur
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), # Palette de couleurs
             repel = TRUE, # Éviter le chevauchement des étiquettes
             labelsize = 2, # Réduire la taille des étiquettes
             title = "Cercle de Corrélation des Variables")

#Pour les Individus, avec leur couleur déterminée par leur qualité de représentation cos2
fviz_pca_ind(resultats_acp,  col.ind="cos2") +
  scale_color_gradient2(low="blue", mid="white",
                        high="red", midpoint=0.50)+
  theme_minimal()
#Pour la dernière question, avec l'extraction de la première composante Fi comme une mesure financière de la variation des prix
#Code pour extraire F1
pc1 <- resultats_acp$ind$coord[, 1]
#Pour Afficher les valeurs de pc1 pour chaque observation
print(pc1)
#Il sera ainsi intéressant de re-tester la régression linéaire en prenant le F1 en considération 
#Cette étape a pour but de savoir si l'ACP apporte un plus au model de régression 
#Pour faire cela nous allons ajouter F1 à la base de données initiale 
perf_envi$F1 <- pc1
#Nous allons ainsi tester le modèle
mod_updated <- lm(EPS ~ BOARD + COD + CSR + EMP + MARGIN + MCAP + RCAP + SALES + pc1, data = perf_envi)
summary(mod_updated)

