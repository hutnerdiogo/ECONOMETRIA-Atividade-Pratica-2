#### Questão 1 ####
#Importando os dados e corrigindo
data <- read.csv("ArquivoExercicio2.csv",sep=";",header=TRUE)

data$Data <- as.Date(data$Data)

#### Questão 2 ####
# Plotando Graficos
plot(data$Data,data$BBSA3,col="Blue")

plot(data$Data,data$PETR4,col="Purple")

plot(data$Data,data$CMIG4,col="Orange")

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
excessos
#### Questão 4 ####

#("BBSA3","PETR4","CMIG4")

# Ativo BBSA3
CAPM_BBSA3 <- lm(BBSA3 ~ MKT,data = data)
summary(CAPM_BBSA3)

CAPM_BBSA3_SEM_ALFA <- lm(BBSA3 ~ MKT + 0,data = data)
summary(CAPM_BBSA3_SEM_ALFA)

# Ativo PETR4
CAPM_PETR4 <- lm(PETR4 ~ MKT,data = data)
summary(CAPM_PETR4)

# Ativo CMIG4
CAPM_CMIG4 <- lm(CMIG4 ~ MKT,data = data)
summary(CAPM_CMIG4)
