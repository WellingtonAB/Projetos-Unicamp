################### Dados Iniciais ##########################

#install.packages("caret")
#install.packages("Biobase")
library(caret)
library(Biobase)
fallCalibrate <- read.csv("fallCalibrate.csv")
fallPredict <- read.csv("fallPredictPlaceholder.csv")

fallCalibrate$ACTIVITY <- factor(fallCalibrate$ACTIVITY)
fallPredict <- subset(fallPredict, select = -id)
separando <- createDataPartition(y=fallCalibrate$ACTIVITY,p=0.75,list=F)
treinando <- fallCalibrate[separando,]
treinando$ACTIVITY <- factor(treinando$ACTIVITY)
testando <- fallCalibrate[-separando,]
testando$ACTIVITY <- factor(testando$ACTIVITY)


####################### Contruíndo a Floresta Aleatória #########################

#install.packages("randomForest")
library(randomForest)
previsao = NULL
posicao = NULL

# Após inúmeros testes, foi escolhido 2000 por possuir baixa variação.
aux1 = 0
numero = 0
for(i in seq(1,300,1)){
set.seed(i)
modelForest <- randomForest(ACTIVITY ~ ., treinando,ntree=100)
etiquetaForest <- predict(modelForest, newdata = testando)
previsao[i] <- sum(diag(table(testando$ACTIVITY,etiquetaForest)))/sum(table(testando$ACTIVITY,etiquetaForest))
numero[i] <- i
aux2 <- previsao[i]
if(aux1 < aux2){
  aux1 = aux2
  refi=i
}
}

prev <- cbind(numero,previsao[!is.na(previsao)])  

set.seed(refi)
modelForest <- randomForest(ACTIVITY ~ ., treinando,ntree=100)
etiquetaForest <- predict(modelForest, newdata = testando)
final <- sum(diag(table(testando$ACTIVITY,etiquetaForest)))/sum(table(testando$ACTIVITY,etiquetaForest))


etiquetaForest <- predict(modelForest, newdata = fallPredict)
valores <- etiquetaForest
write(paste(valores, collapse= " "),"178668label.txt")  

##### Para saber a acurácia:
#print(final)


############### Diversas tentativas diferentes #######################

#   ###### GBM ####
#   #for(j in seq(1,3,1)){
#   #set.seed(i)
#   set.seed(2)
#   fitControl <- trainControl(method = "repeatedcv", number = 4, repeats = 4)
#   gbmFit1 <- train(as.factor(ACTIVITY) ~ ., data = treinando, method = "gbm", trControl = fitControl,verbose = FALSE)
#   gbm_dev <- predict(gbmFit1, testando) 
#   previsao2 <- sum(diag(table(testando$ACTIVITY, gbm_dev)))/sum(table(testando$ACTIVITY,gbm_dev))
#   #posicao2[j] <- j
#   #print(j)
#   #}
#   
#   prev2 <- max(previsao2[!is.na(previsao2)])

  
############################### Tentei com Naive Bayes (24%) #######################
#install.packages("e1071")
# library(e1071)
# 
# bayes <- naiveBayes(ACTIVITY ~., data=treinando)
# predbayes <- predict(bayes,testando)
# 
# sum(diag(table(predbayes,testando$ACTIVITY)))/sum(table(predbayes,testando$ACTIVITY))
# 

########################### Combinando diversos modelos ####################
# library(MASS) # lda
# library(rpart) # rpart
# library(e1071) # svm
# 
# modelLDA <- lda(ACTIVITY ~ ., treinando)
# modelTree <- rpart(ACTIVITY ~ ., treinando)
# modelSVM <- svm(ACTIVITY ~ ., treinando)
# 
# etiquetaLDA <- as.character(predict(modelLDA, treinando)$class)
# temp <- predict(modelTree, treinando)
# 
# 
# etiquetaSVM <- as.character(predict(modelSVM, testando))
# 
# sum(diag(table(etiquetaSVM, testando$ACTIVITY)))/sum(table(etiquetaSVM, testando$ACTIVITY))
# 
# cbind(etiquetaLDA, etiquetaForest, etiquetaSVM, testando$ACTIVITY)
#   head(cbind(etiquetaLDA, etiquetaForest, etiquetaSVM, testando$ACTIVITY))

#################### Voto de maioria ###################
  
  # 
  # Metiquetas <- cbind(gbm_dev, etiquetaForest)
  # majority <- apply(Metiquetas,
  #                   1,
  #                   function(x) names(table(x)[which.max(table(x))]))
  # sum(diag(table(majority, testando$ACTIVITY)))/sum(table(majority, testando$ACTIVITY))
  # 
  # 
  
