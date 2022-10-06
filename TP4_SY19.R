#install.packages('leaps')
library('leaps')

# 3 classes sur les données clas_data
# 500 observations de 50 variables
clas_data <- read.table('TPN1_a22_clas_app.txt')

# 500 observations de 100 variables
reg_data <- read.table('TPN1_a22_reg_app.txt')

# Partie 1 : Sélection des prédicteurs
# exhaustive ?









cross_valid <- function(data, K) {
  
  reg <- lm(y~., data = data)
  lm_pred <-  summary(reg)$coefficients[,4]<0.001
  lm_pred_names <- names(lm_pred)[lm_pred]
  
  new_data <- data[lm_pred] 
  new_data$y <- data$y
  
  n <- nrow(new_data)
  folds <- sample(1:K, n, replace = TRUE)
  CV <- rep(0,length(new_data))
  
  
  reg.fit <- regsubsets(y~., data=new_data, method = 'forward', nvmax=length(lm_pred_names))
  plot(reg.fit, scale = "r2")
  
  summary(reg.fit)$which
  
  for (i in (1:length(new_data)-1)){
    for (k in (1:K)) {
      names(new_data[summary(reg.fit)$which[i,]])
      #vect <- names(new_data[summary(reg.fit)$which[i,]])
      vect <- new_data[summary(reg.fit)$which[i,]] 
      vect$y <- new_data$y
      reg <- lm(y~.,data=vect[folds!=k,])
      
      pred <- predict(reg,newdata=new_data[folds==k,])
      CV[i] <- CV[i]+ sum((new_data$y[folds==k]-pred)^2)
    }
    CV[i]<-CV[i]/n
  }
  plot(CV)
}





cross_valid(reg_data, 5)
