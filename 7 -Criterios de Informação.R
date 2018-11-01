                       #Aula 7 - Critérios de Informação

library(readxl)                                                        #Carrega os pacotes
variacao_PIB <- read.table("c:/Econometria/Variacao.xls", header = T)  #Carrega os arquivos
var_PIB <- ts(variacao_PIB$variacao_PIB, start =1951, frequency = 1 )  #Cria a serie temporal var_PIB
View(var_PIB)
plot(var_PIB, main="Variacao do PIB Brasileiro", col="Blue", ylab="%PIB", xlab="Ano")

acf(var_PIB)         #Cria a FAC -Função de Autocorrelação (ACF)
pacf(var_PIB)        #cria a FACP - Funçao de Autocorrelação Parcial (PACF)

AR1 <- arima(var_PIB, order = c(1,0,0))   #Estima um modelo autoreressivo de ordem p=1 ,AR(1)
AR2 <- arima(var_PIB, order = c(2,0,0))

MA1 <- arima(var_PIB, order = c(0,0,1))   #Estima um modelo de médias móveis ordem q=1 , MA(1)
MA2 <- arima(var_PIB, order = c(0,0,2))
MA3 <- arima(var_PIB, order = c(0,0,3))
MA4 <- arima(var_PIB, order = c(0,0,4))
MA5 <- arima(var_PIB, order = c(0,0,5))
MA6 <- arima(var_PIB, order = c(0,0,6))
MA7 <- arima(var_PIB, order = c(0,0,7))
MA8 <- arima(var_PIB, order = c(0,0,8))
MA9 <- arima(var_PIB, order = c(0,0,9))

ARMA11 <- arima(var_PIB, order = c(1,0,1))#Estima um modelo autoregressivo de médias móveis ordem p=1 e q=1 ARMA(1,1)
ARMA12 <- arima(var_PIB, order = c(1,0,2))
ARMA13 <- arima(var_PIB, order = c(1,0,3))
ARMA14 <- arima(var_PIB, order = c(1,0,4))
ARMA15 <- arima(var_PIB, order = c(1,0,5))
ARMA16 <- arima(var_PIB, order = c(1,0,6))
ARMA17 <- arima(var_PIB, order = c(1,0,7))
ARMA18 <- arima(var_PIB, order = c(1,0,8))
ARMA19 <- arima(var_PIB, order = c(1,0,9))

ARMA21 <- arima(var_PIB, order = c(2,0,1))
ARMA22 <- arima(var_PIB, order = c(2,0,2))
ARMA23 <- arima(var_PIB, order = c(2,0,3))
ARMA24 <- arima(var_PIB, order = c(2,0,4))
ARMA25 <- arima(var_PIB, order = c(2,0,5))
ARMA26 <- arima(var_PIB, order = c(2,0,6))
ARMA27 <- arima(var_PIB, order = c(2,0,7))
ARMA28 <- arima(var_PIB, order = c(2,0,8))
ARMA29 <- arima(var_PIB, order = c(2,0,9))

AIC(AR1) #Extrai a estatística AIC do modelo AR1
BIC(AR1) #Extrai a estatística BIC Ddo modelo AR1
AIC(AR2)
BIC(AR2)

AIC(MA1)
BIC(MA1)
AIC(MA2)
BIC(MA2)
AIC(MA3)
BIC(MA3)
AIC(MA4)
BIC(MA4)
AIC(MA5)
BIC(MA5)
AIC(MA6)
BIC(MA6)
AIC(MA7)
BIC(MA7)
AIC(MA8)
BIC(MA8)
AIC(MA9)
BIC(MA9)

AIC(ARIMA11) 
BIC(ARIMA11)
AIC(ARIMA12)
BIC(ARIMA12)
AIC(ARIMA13) 
BIC(ARIMA13)
AIC(ARIMA14)
BIC(ARIMA14)
AIC(ARIMA15) 
BIC(ARIMA15)
AIC(ARIMA16)
BIC(ARIMA16)
AIC(ARIMA17) 
BIC(ARIMA17)
AIC(ARIMA18)
BIC(ARIMA18)
AIC(ARIMA17)
BIC(ARIMA17)
AIC(ARIMA18) 
BIC(ARIMA18)
AIC(ARIMA19)
BIC(ARIMA19)

AIC(ARIMA21) 
BIC(ARIMA21)
AIC(ARIMA22)
BIC(ARIMA22)
AIC(ARIMA23) 
BIC(ARIMA23)
AIC(ARIMA24)
BIC(ARIMA24)
AIC(ARIMA25) 
BIC(ARIMA25)
AIC(ARIMA26)
BIC(ARIMA26)
AIC(ARIMA27) 
BIC(ARIMA27)
AIC(ARIMA28)
BIC(ARIMA28)
AIC(ARIMA27)
BIC(ARIMA27)
AIC(ARIMA28) 
BIC(ARIMA28)
AIC(ARIMA29)
BIC(ARIMA29)

#Exemplo aplicação múltipla - Extra (Deve-se completar as estimações antes de executar esse código)

estimacoes <- list(AR1, AR2, MA1, MA2, MA3, MA4, MA5, MA6, MA7, MA8, MA9, 
                   ARMA11,ARMA12, ARMA13, ARMA14,ARMA15, ARMA16,ARMA17,ARMA18,ARMA19,
                   ARMA21,ARMA22,ARMA23,ARMA24,ARMA25,ARMA26,ARMA27,ARMA28,ARMA29)      #Cria uma lista com os estimadores
sapply(estimacoes, AIC)                 #Aplica o comando AIC na lista
sapply(estimacoes, BIC)                 #Aplica o comando BIC na lista

#Exemplo de criação de tabela com resultados - Extra
AIC <- sapply(estimacoes, AIC) 
BIC <- sapply(estimacoes, BIC)
Modelo <- c("AR1", "AR2", "MA1", "MA2", "MA3", "MA4", "MA5", "MA6", "MA7", "MA8", "MA9", "ARMA11","ARMA12", "ARMA13", "ARMA14","ARMA15", "ARMA16","ARMA17","ARMA18","ARMA19","ARMA21","ARMA22","ARMA23","ARMA24","ARMA25","ARMA26","ARMA27","ARMA28","ARMA29")

Resultados <- data.frame(Modelo, AIC, BIC)
View(Resultados)
