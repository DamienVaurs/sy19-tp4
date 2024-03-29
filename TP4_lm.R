# Projet 1
encoding="utf-8"
rm(list=ls())
library('leaps')

# Fonctions de service :
make_one_formula <- function(names) {
  # Création de la formule correspondante au modèle i
  f <- "y ~"
  for (name in names) {
    f <- paste(f, "+", name)
  }
  return (f)
}

make_formula <- function(reg.fit) {
  
  Formula <- c()
  for (i in 2:length(summary(reg.fit)$which[1,])-1) {
    
    model <- summary(reg.fit)$which[i,] # Masque du modèle i
    model <- model[2:length(model)] # On ne prend pas la colone (Intercept)
    model.names <- names(model)[model]
    
    names <- model.names[1:length(model.names)]
    f <- make_one_formula(names)
    
    # Ajout de la formule à la liste des formules
    Formula <- append(Formula, f)
  }
  
  return (Formula)
}



# Partie 1 : Regression
# 1) Modèle linéaire
data.reg <- read.table("TPN1_a22_reg_app.txt")
boxplot(data.reg[,-101])   # Vérifier s'il n'y a pas de valeurs abérentes

# Séparation en un train set et un test set
nrow <- nrow(data.reg)
idx.train <- sample(nrow, floor(4/5*nrow))
data.reg.train <- data.reg[idx.train,]
data.reg.test <- data.reg[-idx.train,]


# - Subset selection

# Selection du meilleur sous ensemble de prédicteurs 
lm.reg.fit <- regsubsets(y~., data=data.reg, method='backward', nvmax=length(data.reg)-1)
Formula <- make_formula(lm.reg.fit)

# BIC  -> On trouve un bon modèle avec peu de prédicteurs, mais ce n'est pas le meilleur modèle
lm.reg.fit.summary <- summary(lm.reg.fit)
bic <- lm.reg.fit.summary$outmat[which.min(lm.reg.fit.summary$bic), ]
bic.predicteurs <- names(data.reg)[bic == "*"]
bic.predicteurs <- bic.predicteurs[-length(bic.predicteurs)] # Attention � enlever le y de la formule lorsque le backward est utilis�
plot(lm.reg.fit, scale="bic")  # 46 predicteurs
f <- make_one_formula(bic.predicteurs)  

#y ~ + X1 + X6 + X11 + X12 + X14 + X15 + X17 + X22 + X23 + X25 + X27 + X32 + X33 + X35 + X36 + X37 + X39 + X42 + X46 + X47 + X48 + X49 + X50 + X52 + X54 + X56 + X58 + X59 + X60 + X63 + X68 + X70 + X71 + X72 + X74 + X80 + X83 + X84 + X86 + X87 + X88 + X89 + X90 + X91 + X96 + X99

# Vérification du modèle sur les données de test
best_reg <- lm(f, data.reg.train)
best_pred <- predict(best_reg, newdata=data.reg.test)

plot(data.reg.test$y, best_pred)
abline(0,1)

(err <- mean((data.reg.test$y - best_pred)^2))  # erreur quadratique moyenne
cat("Meilleur modèle pour 'subset selection' avec ", which.min(lm.reg.fit.summary$bic), " prédicteur : ", f)
cat("Erreur quadratique moyenne : ", err)


# - Application d'une validation croisée sur chacun des bests subsets et d?termination du nombre optimal de prédicteurs
#  -> Bon modèle, mais varie beaucoup en fonction des données de train

# n <- nrow(data.reg.train)
# K <- 10
# folds <- sample(1:K, n, replace=TRUE)
# CV <- rep(0, length(Formula))
# 
# for(i in (1:length(Formula))) {
#   for(k in (1:K)) {
#     reg <- lm(Formula[i], data=data.reg.train[folds != k, ])
#     pred <- predict(reg, newdata=data.reg.train[folds == k, ])
#     CV[i] <- CV[i] + sum((data.reg.train$y[folds == k] - pred)^2)
#   }
#   CV[i] <- CV[i]/n
# }
# par(mfrow = c(1, 2))
# plot(CV, type="b") # 50 prédicteurs semblent être un bon compromis entre nombre de prédicteurs et performances
# 
# # Vérification du modèle sur les données de test
# best_reg <- lm(Formula[which.min(CV)], data.reg.train)
# #best_reg <- lm(Formula[40], data.reg.train) # Pr?dicteur lm ?
# best_pred <- predict(best_reg, newdata=data.reg.test)
# 
# plot(data.reg.test$y, best_pred)
# abline(0,1)
# par(mfrow = c(1, 1))
# 
# (err <- mean((data.reg.test$y - best_pred)^2))  # erreur quadratique moyenne
# cat("Meilleur modèle pour 'subset selection' avec ", which.min(CV), " prédicteur : ", Formula[which.min(CV)])
# cat("Erreur quadratique moyenne : ", err)


# Nested cross-validation  -> Trouve un très bon modèle stable en fonction des données de train

n1 <- nrow(data.reg.train)
K1 <- 5
K2 <- 10
folds1 <- sample(1:K1, n1, replace=TRUE)
CV1 <- rep(0, length(Formula))
for (k1 in (1:K1)){
  inner.data <- data.reg.train[folds1 != k1, ]
  n2 <- nrow(inner.data)
  folds2 <- sample(1:K2, n2, replace=TRUE)
  CV2 <- rep(0, length(Formula))
  for(i in (1:length(Formula))) {
    for(k2 in (1:K2)) {
      reg <- lm(Formula[i], data=inner.data[folds2 != k2, ])
      pred <- predict(reg, newdata=inner.data[folds2 == k2, ])
      CV2[i] <- CV2[i] + sum((inner.data$y[folds2 == k2] - pred)^2)
    }
    CV2[i] <- CV2[i]/n2
  }
  CV1 <- CV1 + CV2
}
CV1 <- CV1/K1

par(mfrow = c(1, 2))
sol <- which.min(CV1)
plot(CV1, type="b") # 50 prédicteurs semblent être un bon compromis entre nombre de prédicteurs et performances

# One-standard-error rule :
se <- sd(CV1)/sqrt(length(CV1))
arrows(x0=sol, y0=CV1[sol] - se, x1=sol, y1=CV1[sol] + se, code=0, col="blue", lwd=3)
abline(h=CV1[sol]+se, lty=2, lwd=1)
OSER <- CV1[CV1 <= CV1[sol]+se]
i_OSER <- match(OSER[1], CV1)[1]
abline(v=i_OSER, lty=2, lwd=2)

# Vérification du modèle sur les données de test
best_reg <- lm(Formula[i_OSER], data.reg.train)
best_reg <- lm(Formula[which.min(CV1)], data.reg.train)
#best_reg <- lm(Formula[40], data.reg.train) # Prédicteur lm ?
best_pred <- predict(best_reg, newdata=data.reg.test)

plot(data.reg.test$y, best_pred)
abline(0,1)
par(mfrow = c(1, 1))

(err <- mean((data.reg.test$y - best_pred)^2))  # erreur quadratique moyenne
cat("Meilleur modèle pour 'subset selection' avec ", which.min(CV1), " prédicteur : ", Formula[which.min(CV1)])
cat("Erreur quadratique moyenne : ", err)



# - Regularization (Shrinkage)

#install.packages("glmnet")
#install.packages("Matrix")
library(Matrix)
library(glmnet)

# Standardisation des prédicteurs  -> Pas trop besoin car les variances des prédicteurs ne sont pas très différentes (cf boxplot)
# xtrain.stand <- scale(data.reg.train)
# xtest.stand <- scale(data.reg.test)
# xtest <- data.matrix(xtest.stand[,1:ncol(xtest.stand)-1])
# xtrain <- data.matrix(xtrain.stand[,1:ncol(xtrain.stand)-1])

xtrain <- data.matrix(data.reg.train[,1:ncol(data.reg.train)-1])
xtest <- data.matrix(data.reg.test[,1:ncol(data.reg.test)-1])
ytrain <- data.reg.train$y
ytest <- data.reg.test$y

# Ridge Regression :

cv.out <- cv.glmnet(xtrain, ytrain, alpha=0)
plot(cv.out)
cv.out$lambda.min

fit <- glmnet(xtrain, ytrain, lambda=cv.out$lambda.min, alpha=0)
ridge.pred <- predict(fit, s=cv.out$lambda.min, newx=xtest)
(err <- mean((ytest - ridge.pred)^2))
cat("Erreur quadratique moyenne pour ridge : ", err)

# Lasso  (plus efficace que ridge et réduit la dimension)

cv.out.lasso <- cv.glmnet(xtrain, ytrain, alpha=1)
plot(cv.out.lasso)

fit.lasso <- glmnet(xtrain, ytrain, lambda=cv.out.lasso$lambda.min, alpha=1)
lasso.pred <- predict(fit.lasso, s=cv.out.lasso$lambda.min, newx=xtest)
(err <- (mean((ytest - lasso.pred)^2)))
cat("Erreur quadratique moyenne pour lasso : ", err)

# - Feature extraction  -> (Pas très concluant)

# # PCA
# data.reg.feature <- scale(data.reg)  # Centrer et réduire les données
# pca <- princomp(data.reg.feature)
# Z <- pca$scores
# head(pca)
# lambda <- pca$sdev^2
# 
# plot(cumsum(lambda)/sum(lambda), type="l", xlab="q", ylab="proportion of explained variance")
# 
# # PCR  (MSE similaire ? cross validation, mais avec tous les prédicteurs)
# #install.packages("pls")
# library(pls)
# 
# pcr.fit <- pcr(y ~ .,data=data.reg, scale=TRUE, validation="CV")
# sum <- summary(pcr.fit)
# validationplot(pcr.fit, val.type="MSEP", legendpos="topright")










