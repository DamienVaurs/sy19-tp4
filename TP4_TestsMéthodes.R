# Modèle linéaire

test_methodes <- function(dataTest) {
  
  best_reg <- lm(f, data.reg.train)
  best_pred_1 <- predict(best_reg, newdata=data.reg.test)
  err <- mean((data.reg.test$y - best_pred_1)^2)
  cat("La méthode BIC a une erreur quadratique moyenne de :", err, "\n")
  
  best_reg <- lm(Formula[which.min(CV)], data.reg.train)
  best_pred_2 <- predict(best_reg, newdata=data.reg.test)
  err <- mean((data.reg.test$y - best_pred_2)^2)
  cat("Le modèle linéaire avec cross-validation a une erreur quadratique moyenne de :", err, "\n")
  
  best_reg <- lm(Formula[which.min(CV1)], data.reg.train)
  best_pred_3 <- predict(best_reg, newdata=data.reg.test)
  err <- mean((data.reg.test$y - best_pred_3)^2)
  cat("Le modèle linéaire avec nested cross-validation a une erreur quadratique moyenne de :", err, "\n")
  
  ridge.pred <- predict(fit, s=cv.out$lambda.min, newx=xtest)
  err <- mean((ytest - ridge.pred)^2)
  cat("Le modèle Ridge a une erreur quadratique moyenne de :", err, "\n")
  
  lasso.pred <- predict(fit.lasso, s=cv.out$lambda.min, newx=xtest)
  err <- (mean((ytest - lasso.pred)^2))
  cat("Le modèle Lasso a une erreur quadratique moyenne de :", err, "\n")
  
  par(mfrow = c(2, 3))
  plot(dataTest$y, best_pred_1) 
  abline(0,1)
  plot(dataTest$y, best_pred_2) 
  abline(0,1)
  plot(dataTest$y, best_pred_3) 
  abline(0,1)
  plot(dataTest$y, ridge.pred) 
  abline(0,1)
  plot(dataTest$y, lasso.pred) 
  abline(0,1)
  
}

test_methodes(data.reg.test)
