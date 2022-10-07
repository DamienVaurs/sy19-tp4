# Projet 1
rm(list=ls())
library('leaps')

# Fonctions de service :
make_formula <- function(reg.fit) {
  
  Formula <- c()
  for (i in 2:length(summary(reg.fit)$which[1,])-1) {
    
    model <- summary(reg.fit)$which[i,] # Masque du modèle i
    model <- model[2:length(model)] # On ne prend pas la colone (Intercept)
    model.names <- names(model)[model]
    
    # Création de la formule correspondante au modèle i
    f <- "y ~"
    for (name in model.names[1:length(model.names)]) {
      f <- paste(f, "+", name)
    }
    
    # Ajout de la formule à la liste des formules
    Formula <- append(Formula, f)
  }
  
  return (Formula)
}

# Partie 1 : Regression
# 1) Modèle linéaire
data.reg <- read.table("sy19-tp4/TPN1_a22_reg_app.txt")

# Séparation en un train set et un test set
nrow <- nrow(data.reg)
idx.train <- sample(nrow, floor(4/5*nrow))
data.reg.train_val <- data.reg[idx.train,]
data.reg.test <- data.reg[-idx.train,]

# - Selection du meilleur sous ensemble de prédicteurs

lm.reg.fit <- regsubsets(y~., data=data.reg, method='forward', nvmax=length(data.reg)-1)
Formula <- make_formula(lm.reg.fit)

# - Application d'une validation croisée sur chacun des bests subsets et détermination du nombre optimal de prédicteurs
n <- nrow(data.reg.train_val)
K <- 10
folds <- sample(1:K, n, replace=TRUE)
CV <- rep(0, length(Formula))

for(i in (1:length(Formula))) {
  for(k in (1:K)) {
    reg <- lm(Formula[i], data=data.reg.train_val[folds != k, ])
    pred <- predict(reg, newdata=data.reg.train_val[folds == k, ])
    CV[i] <- CV[i] + sum((data.reg.train_val$y[folds == k] - pred)^2)
  }
  CV[i] <- CV[i]/n
}
par(mfrow = c(1, 2))
plot(CV, type="b") # 50 prédicteurs semblent être un bon compromis entre nombre de prédicteurs et performances

# Vérification du modèle sur les données de test
best_reg <- lm(Formula[which.min(CV)], data.reg.train_val)
#best_reg <- lm(Formula[50], data.reg.train_val) # Prédicteur lm
best_pred <- predict(best_reg, newdata=data.reg.test)

plot(data.reg.test$y, best_pred)
abline(0,1)
par(mfrow = c(1, 1))

(err <- mean((data.reg.test$y - best_pred)^2))  # erreur quadratique moyenne



# Nested cross validation ?




#clas_data <- read.table("sy19-tp4/TPN1_a22_clas_app.txt")
#head(clas_data)
#clas_data$y

# Régression :
# k-plus proches voisins (TD01)
# Régression linéaire (TD02)
#

# Classification :
# Classifieur de Bayes (TD03)
# Linear/Quadratic classification (cours 3 p 28) (lda/qda)
# Naive models
# Binary problem ? Multinomial problem ? (cf fin cours 3)
# Test de McNemar pour comparer les classifieurs (cours 3 p 47)

# Entrainement des modèles :
# cross-validation sur les données de train ?
# Une fois le modèle choisi, refaire un entrainement sur toutes les données ?

