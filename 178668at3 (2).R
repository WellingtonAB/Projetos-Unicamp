foodCalibrate <- read.csv("foodCalibrate.csv")
foodPredict <- read.csv("foodTrain.csv")


#Primeirente foi realizado o seguinte teste para descobrir o valor de K, por meio de um equilibrio das variancias B e W:
set.seed(20)

a=NULL
b=NULL
for(i in 1:10){
fit <- kmeans(foodCalibrate[, 3:9], i)
a[i] <- fit$betweenss
b[i] <- fit$tot.withinss
}

tabelasBW <- data.frame(a,b)

names(tabelasBW) <- c("B","W")

plot(tabelasBW$B, type = "l", col="red",xlab="",ylab="")
par(new=TRUE)
plot(tabelasBW$W, type = "l", col="blue",xlab="Clusters K", ylab="Variação B e W", xaxt='n', yaxt='n')

#Como podemos observar, o cluster mais adequado será o 2, por estar próximo da intersecção entre o B e W.

#Entretanto, foi utilizado outro método chamado "Elbow" informado pelo professor:

set.seed(999)

wss <- sapply(1:15, 
              function(k){kmeans(foodCalibrate[, 3:9], k, nstart=1,iter.max = 15 )$tot.withinss})
wss
plot(1:15, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Nº clusters",
     ylab="Total SQ de W")

#Podemos observar que um bom cluster é o K=6, pois está no limite de um decaimento. 
#Considerando que será descoberto a proporção de variância explicada pelos clusters neste relatório.

#Além disso, podemos notar pelo kmeans os processo de colocar o K no calibrate. 
#Gerando os resultados de |B|/|B+W|:
#Se K=2: 48,1%
#Se K=3: 57%
#Se K=4: 67,8%
#Se K=5: 70,1%
#Se K=6: 74,8%
#Se K-7: 76,6%
#Como é diminuído muito rápido, é adequado usarmos K = 6, pois possiu um dos últimos decaimentos mais significativos.


#Podemos verificar abaixo os resultados da matriz 1000X1 das centóides para cada observação.

#1) Primeiramente foi realizado a classificação das observações em cada centróide com a menor distancia: 

set.seed(54321)

resultado = NULL
base2 <- as.numeric(100000)
nomecluster <- NULL

for(i in 1:10000){
  for(j in 1:6){
    base1 <- dist(rbind(foodPredict[,3:9][i,],kmeans(foodPredict[, 3:9], 6)$center[j,]), method = "euclidian")
    if(base1 < base2){
      base2 = base1
      clus = j
    }
  }
  resultado[i] = base2
  nomecluster[i] = clus
  
  base2 = as.numeric(100000)
}

final <- data.frame(resultado,nomecluster)

print(head(final))

write(final[,2],"178668label.txt")


#2)Podemos observar abaixo pelo |W|/|B+W| as porcentagens de cada K clasters e quando
#a porcentagem não for mais relevante. Como citado pelo professor, será utilizado
#as fórmulas para os calculos necessários do projeto:

#Seja n a frequência do vetor com as centróides escolhidas por cada observação.

n <- data.frame(table(final[,2]))



#3) Com os resultados pela distância e as frequência, adquiro os valores das etiquetas do primeiro caso pedido.
print(final[,2])


#4) Podemos observar o calculo do B e W abaixo:

set.seed(11111)

B = 0
W = 0

for(j in 1:7){
  for(k in 1:6){
    B = B + as.numeric(n[k,2])*as.numeric(kmeans(foodPredict[, 3:9], 6)$center[k,j] - mean(foodPredict[, 3:9][,j]))*as.numeric(t((kmeans(foodPredict[, 3:9], 6)$center[k,j] - mean(foodPredict[, 3:9][,j]))))
  }
}

set.seed(11112)

for(k in 1:6){
  for(j in 1:7){
   for(i in 1:10000) 
    W = W + (final[i,2] - kmeans(foodPredict[, 3:9], 6)$center[k,j])*t(final[i,2] - kmeans(foodPredict[, 3:9], 6)$center[k,j])
  }
}


#5) Com os valores de B e W, podemos calculara proporção de variância explicada pelos clusters.

proporcao <- B/(B+W)

write(as.numeric(proporcao), file = "178668prop.txt")


#Por meio da propriedade do traço, podemos considerar em vez de realizar uma matriz 7x7 para W e B, por meio da somas
# dos casos de uma matriz (7x1)*(1x7). Foi desenvolvido a soma dos (1x7)(7x1), pois possui o mesmo resultado do 
# traço da matriz calculada (7x7). 
