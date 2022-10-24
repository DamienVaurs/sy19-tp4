# 1. Apprentissage des modèles.
library(MASS) # lda
library('leaps')

# 1. Apprentissage des mod�les 

# Regression
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

X.reg = read.table("TPN1_a22_reg_app.txt")

lm.reg.fit <- regsubsets(y~., data=X.reg, method='backward', nvmax=length(X.reg)-1)
Formula <- make_formula(lm.reg.fit)

lm.reg.fit.summary <- summary(lm.reg.fit)
bic <- lm.reg.fit.summary$outmat[which.min(lm.reg.fit.summary$bic), ]
bic.predicteurs <- names(X.reg)[bic == "*"]
bic.predicteurs <- bic.predicteurs[-length(bic.predicteurs)] # Attention ? enlever le y de la formule lorsque le backward est utilis?
plot(lm.reg.fit, scale="bic")  # 46 predicteurs
f <- make_one_formula(bic.predicteurs)  

reg <- lm(f, data = X.reg)


# Classification
X.clas = read.table("TPN1_a22_clas_app.txt")
clas <- lda(y ~ ., data = X.clas)



# 2. Création des fonctions de prédiction

prediction_cls <- function(dataset) {
    # Ne pas oublier de charger **à l'intérieur de la fonction** les
    # bibliothèques utilisées.
    library(MASS)

    # Attention à ce que retourne un modèle en prédiction. La lda
    # retourne une liste nommée. On sélectionne donc les classes ici.
    predict(clas, test_set)$class
}

prediction_reg <- function(dataset) {
  predict(reg, newdata=dataset)
}

# 3. Sauvegarder sous forme de fichier .Rdata les fonctions
# `prediction_cls` et `prediction_reg` sans changer leur nom et avec
# les mêmes arguments. Sauvegarder également les objets utilisés dans
# ces fonctions (`clas` et `reg` dans l'exemple) !

save("clas", "reg", "prediction_cls", "prediction_reg", file = "env.Rdata")
