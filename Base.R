#### Questão 0 ####
#Importando Modulos
#install.packages("stargazer")
library(stargazer)

#### Questão 1 ####

#Importando os dados e corrigindo
data <- read.csv("ArquivoExercicio2.csv",sep=";",header=TRUE)

data$Data <- as.Date(data$Data)

#### Questão 2 ####
# Plotando Graficos
plot(data$Data,data$BBSA3,col="Blue",
     main = "Retorno por dia da BBSA3")
abline(v=18320,col="Red")
plot(data$Data,data$PETR4,col="Purple",
     main = "Retorno por dia da PETR4")
abline(v=18320,col="Red")
plot(data$Data,data$CMIG4,col="Orange",
     main = "Retorno por dia da CMIG4")
abline(v=18320,col="Red")

#Analise
#'registrar linhas verticas, mas são os pontos
#'de maiores dispersões
#'
#### Questão 3 ####
names(data)
ativos <- c("BBSA3","PETR4","CMIG4")

excessos <- matrix(,nrow=547,ncol=3)
colnames(excessos) <- ativos

for (ativo in ativos){
  excessos[,ativo] <- data[,ativo] - data$RF
}

rownames(excessos) <- data$Data

output <- matrix(,nrow=4,ncol=3)
colnames(output) <- ativos
estatisticas <- c("Minimo","Media","Maximo","Desvio Padrao")
rownames(output) <- estatisticas
for (ativo in ativos){
  output["Minimo",ativo] <- min(excessos[,ativo])
  output["Media",ativo] <- median(excessos[,ativo])
  output["Maximo",ativo] <- max(excessos[,ativo])
  output["Desvio Padrao",ativo] <- sd(excessos[,ativo])
}
output
#### Questão 4 ####

#("BBSA3","PETR4","CMIG4")

# Ativo BBSA3
CAPM_BBSA3 <- lm(BBSA3 ~ MKT,data = data)
summary(CAPM_BBSA3)


# Ativo PETR4
CAPM_PETR4 <- lm(PETR4 ~ MKT,data = data)
summary(CAPM_PETR4)

# Ativo CMIG4
CAPM_CMIG4 <- lm(CMIG4 ~ MKT,data = data)
summary(CAPM_CMIG4)

#### Questão 5 ####
#Analise o poder explicativo -> Docs da Joana

#### Questão 6 ####
modelo_FFC_BBSA3 <- lm(BBSA3 - RF ~ MKT + SMB + HML + WML,data=data)
summary(modelo_FFC_BBSA3)

modelo_FFC_PETR4 <- lm(PETR4 - RF ~ MKT + SMB + HML + WML,data=data)
summary(modelo_FFC_PETR4)

modelo_FFC_CMIG4 <- lm(CMIG4 - RF ~ MKT + SMB + HML + WML,data=data)
summary(modelo_FFC_CMIG4)

#### Questão 7 ####
modelo_FF_BBSA3 <- lm(BBSA3 - RF ~ MKT + SMB + HML,data=data)

modelo_FF_PETR4 <- lm(PETR4 - RF ~ MKT + SMB + HML,data=data)

modelo_FF_CMIG4 <- lm(CMIG4 - RF ~ MKT + SMB + HML,data=data)


stargazer(CAPM_BBSA3,modelo_FFC_BBSA3,modelo_FF_BBSA3, type = "html", title = "Comparação de regressões - BBSA3",
          column.labels=c("CAPM","Fama-French-Carhart","Fama-French"),
          dep.var.labels.include=F)

stargazer(CAPM_PETR4,modelo_FFC_PETR4,modelo_FF_PETR4, type = "html", title = "Comparação de regressões - PETR4",
          column.labels=c("CAPM","Fama-French-Carhart","Fama-French"),
          dep.var.labels.include=F)

stargazer(CAPM_CMIG4,modelo_FFC_CMIG4,modelo_FF_CMIG4, type = "html", title = "Comparação de regressões - CMIG4",
          column.labels=c("CAPM","Fama-French-Carhart","Fama-French"),
          dep.var.labels.include=F)

setwd("Output")
getwd()
