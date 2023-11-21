####
######Modelo lineal generalizado mixto - GLMM Con distribucion Binomial############
###################################################################################

library(lme4)
library(car)
library(dplyr)
library(openxlsx)
library(ggplot2)
#Load Data set
rasgos<- read.xlsx("Rasgos.xlsx", rowNames = FALSE)

#separar tablas,poR QUE SI.
espinas<- select(rasgos,Forofito,Zona,SinEspinas,ConEspinas,TotalIndividuos2)
Clonalidad <- select(rasgos,Forofito,Zona,Rizoma,Estolon,SinClonalidad,TotalIndividuos3)
EstrategiaTomaNutrient <- select(rasgos,Forofito,Zona,Tanque,Ericoide,Orchidoide,AM,AMDSE,DSE,SinEstrategiaTomaNutrientes,TotalIndividuos4)
Dispersion <- select(rasgos,Forofito,Zona,Anemocoria,Endocoria,SinAyuda,TotalIndividuos5)
habito <- select(rasgos,Forofito,Zona,Hemiepifitas,Liananomadica,Epifita,TotalIndividuos6)
metabolismo<- rasgos[,1:5]


#proporciones.Crear variable nueva dentro de datos #revisar si esta entre 0-1
espinas$presprop <- espinas$ConEspinas/espinas$TotalIndividuos2

Clonalidad$presprop <- (Clonalidad$Rizoma + Clonalidad$Estolon)/Clonalidad$TotalIndividuos3 # Volver en variable binomial como presencia de estructura de clonalidad

EstrategiaTomaNutrient$presprop <- (EstrategiaTomaNutrient$Tanque + EstrategiaTomaNutrient$Ericoide +
                                      EstrategiaTomaNutrient$Orchidoide + EstrategiaTomaNutrient$AM + 
                                      EstrategiaTomaNutrient$AMDSE + EstrategiaTomaNutrient$DSE)/ EstrategiaTomaNutrient$TotalIndividuos4

Dispersion$Anemprop <- Dispersion$Anemocoria/ Dispersion$TotalIndividuos5 
Dispersion$zooprop <- Dispersion$Endocoria / Dispersion$TotalIndividuos5

habito$epifiprop <- habito$Epifita /habito$TotalIndividuos6
habito$hemiprop <- habito$Hemiepifitas / habito$TotalIndividuos6
habito$Lianaprop <- habito$Liananomadica / habito$TotalIndividuos6


metabolismo$camprop<- metabolismo$CAM/metabolismo$TotalIndividuos
metabolismo$c3prop <- metabolismo$C3 /metabolismo$TotalIndividuos

##GRAFICA EXPLORATORIA

ggplot(espinas, aes(x=Zona, y=presprop)) + 
  geom_boxplot()+ xlab("Zonas") + ylab("Proporción de Individuos")+
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent')
  )

ggplot(Clonalidad, aes(x=Zona, y=presprop)) + 
  geom_boxplot() + xlab("Zonas") + ylab("Proporción de Individuos")+
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent')
  )

ggplot(EstrategiaTomaNutrient, aes(x=Zona, y=presprop)) + 
  geom_boxplot()+ xlab("Zonas") + ylab("Proporción de Individuos")+
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent')
  )

ggplot(Dispersion, aes(x=Zona, y=Anemprop)) + 
  geom_boxplot()+ xlab("Zonas") + ylab("Proporción de Individuos")+
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent')
  )

ggplot(Dispersion, aes(x=Zona, y=zooprop)) + 
  geom_boxplot()+ xlab("Zonas") + ylab("Proporción de Individuos")+
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent')
  )

ggplot(habito, aes(x=Zona, y=epifiprop)) + 
  geom_boxplot()+ xlab("Zonas") + ylab("Proporción de Individuos")+
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent')
  )

ggplot(habito, aes(x=Zona, y=hemiprop)) + 
  geom_boxplot()+ xlab("Zonas") + ylab("Proporción de Individuos")+
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent')
  )

ggplot(habito, aes(x=Zona, y=Lianaprop)) + 
  geom_boxplot()+ xlab("Zonas") + ylab("Proporción de Individuos")+
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent')
  )

ggplot(metabolismo, aes(x=Zona, y=camprop)) + 
  geom_boxplot()+ xlab("Zonas") + ylab("Proporción de Individuos")+
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent')
  )

ggplot(metabolismo, aes(x=Zona, y=c3prop)) + 
  geom_boxplot()+ xlab("Zonas") + ylab("Proporción de Individuos")+
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent')
  )


#Modelos mixtso con intercepto aleatorio. No me interasa la pendiente, porque mi efecto fijo es categorico

glm.espinas1 <- glmer (presprop~Zona +(1|Forofito), family = binomial, data=espinas)
summary(glm.espinas1) #no contribuye incluir efecto aleatorio

#glm.clonalidad1 <- glmer (presprop~Zona +(1|Forofito), family = binomial, data=Clonalidad)
#summary(glm.espinas1) #no contribuye incluir efecto aleatorio

glm.nutrien1 <- glmer (presprop~Zona +(1|Forofito), family = binomial, data=EstrategiaTomaNutrient)
summary(glm.nutrien1) #no contribuye incluir efecto aleatorio

glm.Anem1 <- glmer (Anemprop~Zona +(1|Forofito), family = binomial, data=Dispersion)
summary(glm.Anem1) #no contribuye incluir efecto aleatorio

glm.zoo1 <- glmer (zooprop~Zona +(1|Forofito), family = binomial, data=Dispersion)
summary(glm.zoo1) #no contribuye incluir efecto aleatorio MAS O MENOS

glm.epifi1 <- glmer (epifiprop~Zona +(1|Forofito), family = binomial, data=habito)
summary(glm.epifi1) #no contribuye incluir efecto aleatorio

glm.hemi1 <- glmer (hemiprop~Zona +(1|Forofito), family = binomial, data=habito)
summary(glm.hemi1) #no contribuye incluir efecto aleatorio

glm.Lian1 <- glmer (Lianaprop~Zona +(1|Forofito), family = binomial, data=habito)
summary(glm.Lian1) #no contribuye incluir efecto aleatorio

glm.cam1<- glmer(camprop~Zona + (1|Forofito), family=binomial, data=metabolismo)
glm.cam1
summary(glm.cam1)  #No contribuye incluir forofito

glm.c31<- glmer(c3prop~Zona + (1|Forofito), family=binomial, data=metabolismo)
glm.c31  #No incluir forofito




#pRUEBA MAS ADECAUADA POR QEU ES COMPARACION DE MODELOS CON MV. Para significance de X (zonas)
library(car)
Anova(glm.cam1, test="Chisq")# ANOVA
Anova(glm.cam1)

#Sobredispersión # de veces (+ veces) en que la variabilidad de los datos se puede explciar por el modelo 
#la variablidad de los datos es 4 veces mayor que la variabilidad que puede predecir el modelo. Hay sobre dispercion

deviance(glm.cam1)/df.residual(glm.cam1)
#CONCLUSION: usar binomial No mixta...VER Script "binomialXzonaxrasgos"
