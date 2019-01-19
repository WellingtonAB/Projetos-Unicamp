
# Primeiramente carreguei todas as bases de dados para começarmos as devida manipulações nos dados sugeridas pelo professor, por meio de uma função (após ter baixado os arquivos e colocado no diretório). 
# Podemos observar abaixo um for que irá guardar todos os arquivos na lista.


# Como primeiro passo, foi inserido as bibliotecas lubridate e dplyr para o uso de respectivamente de dmy e o %>%.

#install.packages("lubridate")
library(lubridate)
#install.packages("dplyr")
library(dplyr)

# Após o uso das biibliotecas, utilizamos uma lista para os arquivos csv Resultandoem vetor de 14 elementos.
lista <- list.files(pattern = ".csv")

lista <- lista[-which(lista=="centros.csv")]
# Utilizando o for para guardar todos os dados, teremos:  
for (i in 1:length(lista)) {
  
  assign(lista[i], read.csv(lista[i])[,c("codigo_estacao","data","precipitacao")])
  
}

# Considerando que foi ceriado a listadosarquivos para facilitar o uso dos dados armazenados. 
listadosarquivos <- list(barueri.csv,bertioga.csv,braganca_paulista.csv,caldas_mg.csv,campos_do_jordao.csv,casa_branca.csv,itapira.csv,piracicaba.csv,sao_carlos.csv,sao_miguel_arcanjo.csv,sao_paulo_interlagos.csv,sao_paulo_santana.csv,sorocaba.csv,taubate.csv)

# Como a listadosarquivos possui todos os dados, então podemos remover os arquivos que estão ocupando a memória que foram utilizados par formar esta lista.

rm(barueri.csv,bertioga.csv,braganca_paulista.csv,caldas_mg.csv,campos_do_jordao.csv,casa_branca.csv,itapira.csv,piracicaba.csv,sao_carlos.csv,sao_miguel_arcanjo.csv,sao_paulo_interlagos.csv,sao_paulo_santana.csv,sorocaba.csv,taubate.csv)



# Próxima etapa será a criação de uma função que junte os arquivos, com os devidos requisitos informados.



funcao1 <- function(y){
  
  # Inicialmente na função é colocado no lugar de "////" o NA. No caso é necessário este procedimento, pois os dados não foram indentificados.
  y$precipitacao[y$precipitacao == "////"] <- NA
  
  # Poteriormente foi colocado as datas no formato recomendado para a manipulação.
  y$data <- dmy(y$data)
  
  # Durante a função foi necessário a utilização de um auxiliar para que seja facilitado arealização da manipulação dos dados.
  auxiliar <- y[,c("codigo_estacao")][1]
  
  # Após estes procedimetos, foram utilizados o dplyr para somar as datas iguais, sendo o resultado NA caso todas as observações de mesmo dia sejam NA.
  y <- y %>%
    group_by(data) %>%
    summarise(geral = ifelse(all(is.na(as.numeric(precipitacao))), as.numeric(precipitacao)[NA_integer_], sum(as.numeric(as.character(precipitacao)))))
  
  # Depois de adquirir o resullatado, podemos somar os 10 dias e caso apareça NA de algum deles, é somado os valores dos dias da precipitação desconsiderando NA, e após isto, é somado com a média da soma sem NA multiplicadoo com o número de vezes que apparece NA em cada intervalo.
  intervalo = as.Date(seq(as.Date("2017-09-03"), as.Date("2018-08-29"),by="10 days"))
  
  y <- y %>% 
    group_by(data = as.factor(cut(data,intervalo))) %>%
    summarise(somassemNA = sum(geral, na.rm=T), ndeNA = sum(is.na(geral)), resultado = ifelse(all(is.na(geral)), geral[NA_integer_],  somassemNA + somassemNA*ndeNA/n()))
  
  # E para finalizar a função, foram implementados a eliminação de linhas com missing da data e o uso da variável auxiliar.
  y <- y[complete.cases(y$data), c("data","resultado")]
  
  y$codigo_estacao <- auxiliar
  return(y)
}


#Após criar a função, iremos utiliza-la para a criação de uma lista final que conterá todos os arquivos já manipulados como sugerido.

listafinal=list()
for(i in 1:length(listadosarquivos)){
  listafinal[[i]] <- funcao1(read.csv(lista[i]))
}

# Por fim, foi utiilizado o rbind que irá juntar todos os arquivos que estão na listafinal.
completo <- do.call("rbind", listafinal)



#Diante dos procedimentos de manipulação de dados, percebe-se que os valores das precipitações das estações são uma matriz de 14x1 vec. Lembrando que o vec é formado diante de uma matriz mxn, que após o processo de vec, a matriz ficará (m*n)x1.
#Então Var(vec(y))=ΣS⊗ΣT será igual a `r var(completo$resultado, na.rm=T)`, que será 2072,048. Podemos notar que existe uma grande variabilidade dos dados.


#Podemos verificar a variância será igual ao produto de kronecker de sigma maiúsculo de T e o de S.


# Na segunda parte iremos calcular os sigmas maiúsculos. Porém antes, é necessário colocar as dimensões iguais (36) e realizar a junção por meio do merge entre a tabela centro com a completa.

# Primeiro vamos deixar a tabela centros no formato UTF-8.
centro <- read.csv("centros.csv", encoding="UTF-8")

auxiliar3 <- table(completo$codigo_estacao)

# Agora precisamos colocar NA's nas tabelas para conseguirmos dimensões iguais, com o objetivo de calcular matrizes sem ter o problema de dimensão. 

while(dim(completo) != 504 && dim(completo) > 0){
  for(k in 1:length(table(completo$codigo_estacao))){
    if(table(completo$codigo_estacao)[k] < 36){
      
      ## abaixo, podemos notar a utilização do rbind para acrescentar observações novas para ficarem com dimensões iguais.
      
      completo <- rbind(completo, data.frame(codigo_estacao = names(auxiliar3)[k], resultado = NA, data = NA))
      
    }
  }
}

# Com base nisso, podemos também realizar um merge para observarmos a tabela de modo geral.

centro$codigo_estacao <- c("A726", "A739", "A744", "A701", "A755", "A713", "A711", "A715", "A771", "A738", "A765", "A706", "A530", "A728")

completojunto <- merge(completo,centro,by="codigo_estacao")



# É observável abaixo uma função que irá retonar a log verossimilhança de theta (apresentado a fórmula na descrições do professor sobre a atividade).


# Agora podemos criar as funções para o sigma maiúsculo T e S, mi e sigma. 
f <- function(theta){
  
  variancia <- theta[1]
  phideT <- theta[2]
  phideS <- theta[3]
  mi <- theta[4]
  
  matrixT <- matrix(nrow = 36, ncol = 36)
  
  for(t1 in 1:36){
    for(t2 in 1:36){
      matrixT[t1,t2] <- variancia*exp(-abs(t1-t2)/phideT)
    }
  }
  
  matrixS <- matrix(nrow = 14, ncol = 14)
  
  for(s1 in 1:14){
    for(s2 in 1:14){
      bases1 <- as.numeric(centro[s1,c("Latitude","Longitude")])
      bases2 <- as.numeric(centro[s2,c("Latitude","Longitude")])
      modulo <- sqrt(((bases1[1] - bases2[1])**2)+((bases1[2]-bases2[2])**2))
      matrixS[s1,s2] <- exp(-as.numeric(modulo)/as.numeric(phideS))  
    }
  }
  
  
  
  logvero <- ((-(14*36)/2)*log(2*pi) - (36/2)*log(det(matrixS)) - (14/2)*log(det(matrixT)) - (1/2)*t(completojunto$resultado[!is.na(completojunto$resultado)] - mi*as.vector(rep(1,504))[!is.na(completojunto$resultado)])%*% kronecker(solve(matrixS),solve(matrixT))[!is.na(completojunto$resultado),!is.na(completojunto$resultado)]%*%(completojunto$resultado[!is.na(completojunto$resultado)] - mi*as.vector(rep(1,504))[!is.na(completojunto$resultado)]))
  
  return(logvero)
  
}


# Considerando que S= 14 e T = 36, então pela função de log-verossimilhança, teremos:

chutando = c(1,10,1,15) 

finallogvero <- optim(chutando,f,control = list(fnscale = -1), method= "L-BFGS-B", lower= c(0,0.1,0.1,0.1))



# Agora podemos construir previsões usando a esperança condicional.  
# Porém, será necesspario conhecer os valores de cada caso.

matrixT <- matrix(nrow = 36, ncol = 36)

for(t1 in 1:36){
  for(t2 in 1:36){
    matrixT[t1,t2] <- finallogvero$par[1]*exp(-abs(t1-t2)/finallogvero$par[2])
  }
}

matrixS <- matrix(nrow = 14, ncol = 14)

for(s1 in 1:14){
  for(s2 in 1:14){
    bases1 <- as.numeric(centro[s1,c("Latitude","Longitude")])
    bases2 <- as.numeric(centro[s2,c("Latitude","Longitude")])
    modulo <- sqrt(((bases1[1] - bases2[1])**2)+((bases1[2]-bases2[2])**2))
    matrixS[s1,s2] <- exp(-as.numeric(modulo)/as.numeric(finallogvero$par[3]))  
  }
}

logvero <- ((-(14*36)/2)*log(2*pi) - (36/2)*log(det(matrixS)) - (14/2)*log(det(matrixT)) - (1/2)*t(completojunto$resultado[!is.na(completojunto$resultado)] - finallogvero$par[4]*as.vector(rep(1,504))[!is.na(completojunto$resultado)])%*% kronecker(solve(matrixS),solve(matrixT))[!is.na(completojunto$resultado),!is.na(completojunto$resultado)]%*%(completojunto$resultado[!is.na(completojunto$resultado)] - finallogvero$par[4]*as.vector(rep(1,504))[!is.na(completojunto$resultado)]))

# Não foi necessário utiliza-lo, pois  o produto das matrizes obtiam dimens~~oes semlhantes quando desconsideradas o NA.
#for(col in 1: 461){
#  for(lin in 1:36){
    
#    covar[lin,col] <- cov(matrixT[lin,col],matrixS[lin,col])
      
#  }
#}

ynovoestimado <- finallogvero$par[4] + cov(matrixS %x% matrixT)[!is.na(completojunto$resultado),!is.na(completojunto$resultado)] %*% solve(kronecker(matrixS,matrixT))[!is.na(completojunto$resultado),!is.na(completojunto$resultado)] %*% (completojunto$resultado[!is.na(completojunto$resultado)] - (completojunto$resultado[!is.na(completojunto$resultado)] - finallogvero$par[4]*as.vector(rep(1,461))))



#Logo, podemos agora prever em outras cidades o theta. No caso sugerido pelo professor, realizaremos a análise na cidade de Campinas, como é observado abaixo.


# Prevendo de campinas:


prevendomatrixSdeCampinas <- matrix(nrow = 14, ncol = 1)

  for(s2 in 1:14){
    bases2 <- as.numeric(centro[s2,c("Latitude","Longitude")])
    modulo <- sqrt(((-22.9329 - bases2[1])**2)+((-47.0738 - bases2[2])**2))
    prevendomatrixSdeCampinas[s2,1] <- exp(-as.numeric(modulo)/as.numeric(finallogvero$par[3]))  
  }

matrixTnova <-  matrix(nrow = 36, ncol = 36)

for(preve1 in 1:36){
  for(preve2 in 1:36){
    matrixTnova[preve1,preve2] <- finallogvero$par[1]*exp(-abs(preve1-preve2)/finallogvero$par[2])
  }
}


covanovo <- t(prevendomatrixSdeCampinas %x% matrixT)

ynovoestimado <- finallogvero$par[4] + (covanovo %*% solve(matrixS %x% matrixT))[!is.na(completojunto$resultado),!is.na(completojunto$resultado)] %*% (completojunto$resultado[!is.na(completojunto$resultado)] - (completojunto$resultado[!is.na(completojunto$resultado)] - finallogvero$par[4]*as.vector(rep(1,461))))



write(ynovoestimado, sep=",","178668campinas.txt")

