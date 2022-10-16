# Modèle linéaire
encoding="utf-8"

#source("TP4_lm.R", encoding="utf-8")

test_methodes <- function(dataTest) {
  
  best_reg <- lm(f, data.reg.train)
  pred_bic <- predict(best_reg, newdata=data.reg.test)
  err_bic <- mean((data.reg.test$y - pred_bic)^2)
  cat("La méthode BIC donne un modèle avec une erreur quadratique moyenne de :", err_bic, "\n")
  
  best_reg <- lm(Formula[which.min(CV)], data.reg.train)
  pred_cv <- predict(best_reg, newdata=data.reg.test)
  err_cv <- mean((data.reg.test$y - pred_cv)^2)
  cat("Le modèle linéaire avec cross-validation a une erreur quadratique moyenne de :", err_cv, "\n")
  
  best_reg <- lm(Formula[which.min(CV1)], data.reg.train)
  pred_ncv <- predict(best_reg, newdata=data.reg.test)
  err_ncv <- mean((data.reg.test$y - pred_ncv)^2)
  cat("Le modèle linéaire avec nested cross-validation a une erreur quadratique moyenne de :", err_ncv, "\n")
  
  pred_ridge <- predict(fit, s=cv.out$lambda.min, newx=xtest)
  err_ridge <- mean((ytest - pred_ridge)^2)
  cat("La méthode ridge a une erreur quadratique moyenne de :", err_ridge, "\n")
  
  pred_lasso <- predict(fit.lasso, s=cv.out$lambda.min, newx=xtest)
  err_lasso <- (mean((ytest - pred_lasso)^2))
  cat("La méthode lasso a une erreur quadratique moyenne de :", err_lasso, "\n")
  
  par(mfrow = c(2, 3))
  plot(dataTest$y, pred_bic) 
  abline(0,1)
  plot(dataTest$y, pred_cv) 
  abline(0,1)
  plot(dataTest$y, pred_ncv) 
  abline(0,1)
  plot(dataTest$y, pred_ridge) 
  abline(0,1)
  plot(dataTest$y, pred_lasso) 
  abline(0,1)
  
}

test_methodes(data.reg.test)
