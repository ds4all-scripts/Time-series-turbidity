
# Pacotes, dataset e outros -----------------------------------------------

#Pacotes
library(dplyr)
library(tidyverse)
library(reshape)
library(lubridate)
library(reshape2)
library(ggplot2)
library(cowplot)
library(forecast)
library(stringr)
library(tseries)
library(tsibble)

#Diretório
local = "C:\\Users\\User\\Documents\\TCC_EDU"
setwd(local)

#Dataset


TURB = read.csv("PAV.csv",header = T)
TURB$Date = floor_date(as.Date(TURB[,2]),
                       unit = "month")

#Agregando a turbidez máxima mensal


TURB = TURB %>% group_by(Date) %>%
  summarise(Turb.max=mean(Turb))
Turb = ts(TURB$Turb.max,
          start = c(2008, 2),
          frequency = 12)
plot(Turb)

# Análise exploratoria  ---------------------------------------------------

#Dimensoes e quantidade de registros
dim(TURB)

#Data inicial
min(TURB$Date)

#Data final
max(TURB$Date)

#Intervalo dos registros - Assumindo espaçamento igual
TURB$Date[3]-TURB$Date[2]

#Maior e menor turbidez
max(TURB$Turb.max)
min(TURB$Turb.max)

#Media das media
mean(TURB$Turb.max)

start_Date <- min(TURB$Date)
end_Date<- max(TURB$Date)


#Visualizacao simplificada
a = ggplot(data = TURB, aes(x = Date,
                            y = Turb.max)) +
  geom_line(color = 'red', size = 1) +
  scale_x_date(limits = c(start_Date, end_Date))+
  geom_hline(yintercept =c(5,20), linetype = 2)+
  theme_cowplot()+
  theme(legend.title=element_text(family="Times",size=20),
        legend.text=element_text(family="Times",face = "italic",size=15),
        plot.title=element_text(family="Times", face="bold", size=20),
        axis.title.x=element_text(family="Times", face="bold", size=12),
        axis.title.y=element_text(family="Times", face="bold", size=12)) +
    annotate("text", x = end_Date-5,
           y = c(4.5,19.5),
           label = c("bold(\"FDL\")",
                     "bold(\"FD\")"),parse = TRUE)+
  xlab("ano") +
  ylab("Máximas mensais de turbidez(uT)");a




# O que se pode notar no gráfico:
# Não há tendencia (aumento ou reducao);
# Eventos ciclicos;
# Padroes sazonais em cada ciclo.
library(urca)
library(seasonal)

autoplot(a$seasonal )+ggtitle("")+theme_bw()
a = ggseasonplot(Turb, polar = F)+
  ggtitle("")+
  geom_point(size = 2)+
  theme_bw()+
  geom_vline(xintercept = c(.4,.7),
             linetype = 2)+
  facet_grid(~year, scales = "free")+
  theme(axis.text.x = element_text(size = rel(.75),
                                   angle = 90))+
  guides(colour = "none")
library(RColorBrewer)

b = ggsubseriesplot(Turb)+ ggtitle("")+
ylab("Máximas mensais de turbidez (uT)")+
theme_cowplot()+
geom_vline(xintercept = c(6,9),
           linetype = 2)+
  geom_point(size = 2)+
  annotate("text", x = c(3,7.5,11),
           y = c(15.5),
           label = c("bold(\"Período de Alta\")",
                     "bold(\"Período de Baixa\")",
                     "bold(\"Período de Alta\")"),parse = TRUE)

plot_grid(b,a,nrow = 2,labels = "AUTO")
#Analisando 2015 a 2017
TURB = TURB[as.Date(TURB$Date)>= start_Date
            & as.Date(TURB$Date ) <= end_Date,]
b = ggplot(data = TURB,
           aes(x = Date,
               y = Turb.max)) +
  geom_line(color = "red", size = 1) +
  stat_smooth( color = "blue",
               fill = "yellow",
               method = "loess" )+
  theme_bw();b
# Observar que a variavel Turb.max:
# Nao apresenta natureza monotonica ( crescente, descrescente ou constante)
# Forte dependencia (correlação) com o tempo

plot_grid(a,b,nrow = 1, labels = "AUTO")



# Decomposição  -----------------------------------------------------------

# Suavizacao exponencial sazonal de Holt-Winters

# Suavização Exponecial Holt
# Series que apresentam tendencia linear
# modelos aditivos e multiplicativos apresentam formas diferentes
# Interpretar gamma = se prox. de 1, valores recente explicam
# melhor a sazonalidade da serie, no contrario (gamma prox. zero),
# valores mais antigos fornecem melhor explicacao.


# Visualização das funções de autocorreção (FAC) e autocorrelação
# parcial (FACP)

par(mfrow = c(2,1))
acf(Turb, main = "(a)",ylab = "FAC",xlab = "",lag.max = 50)
pacf(Turb,main = "(b)",ylab = "FACP",lag.max = 50)

#Decomposição
dec1 = decompose(Turb)
plot(dec1)

#Visualização apenas da tendência
dec1$trend
plot(dec1$trend)



#Modelo aditivo
Ajuste_Turb = HoltWinters(Turb)
Ajuste_Turb
par(mfrow = c(1,1))
plot(Turb,
     ylab = "Valores observados/Ajustados",
     xlab = "Tempo")
lines(fitted(Ajuste_Des)[,1], col = "red", lwd = 2)
legend("top",
       c("Serie original","Suavizaçao Aditiva de Holt-Winters"),
       col = c("black","red"),
       lwd = c(1,2),
       bty = "n")

# Prever 1 ano
prev_Ad_des = forecast::forecast(Ajuste_Des,
                                 h = 12,
                                 level = c(80,
                                           95))


# Modelo multiplicativo

Ajuste_Turb1 = HoltWinters(Turb,
                          seasonal = "multiplicative")
prev_Ad_des1 = forecast::forecast(Ajuste_Turb1,
                                 h = 12,
                                 level = c(80,95))
lines(fitted(Ajuste_Des)[,1], col = "red", lwd = 2)

plot(prev_Ad_des,
     ylab = "Previções Desemprego [IC 95%]",
     xlab = "Tempo")
lines(prev_Ad_des1)

train = window(Turb,start = c(2008,2),end = c(2015,4))
test = window(Turb,start = c(2015,5))
fit1 <- hw(train,h = 24,seasonal="additive")
fit2 <- hw(train,h = 60,seasonal="multiplicative")
autoplot(Turb) +
  theme_cowplot()+
  autolayer(fit1, series="Aditivo",
            PI=FALSE) +
  autolayer(fit2, series="Multiplicatico",
            PI=FALSE) +
  geom_vline(aes(xintercept = 2017.3),
                 linetype = 2,size = 1)+
  geom_vline(aes(xintercept = 2015.4),
             linetype = 2,size = 1)+
  geom_hline(aes(yintercept = 5 ),
             linetype = 2,size = 1)+
  xlab("Year") +
  ylab("Valor máximo de Turbidez mensal") +
  annotate("text", x = c(2011.6,2016.6,2018.5),
           y = c(15,15,15),
           label = c("bold(\"Treino\")",
                     "bold(\"Teste\")",
                     "bold(\"Prognóstico\")"),parse = TRUE)+

  guides(colour=guide_legend(title="Modelos de Holt-Winters"))
