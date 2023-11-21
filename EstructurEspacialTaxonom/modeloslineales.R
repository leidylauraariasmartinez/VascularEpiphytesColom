####
######Modelo lineal generalizado mixto - GLMM Con distribucion Poisson############
###################################################################################

library(lme4)
library(car)
library(dplyr)
library(openxlsx)
library(ggplot2)
#Load Data set
df<- read.xlsx("AbundanciaxZona.xlsx", rowNames = FALSE)
#Grafica
ggplot(df, aes(x=Zona, y=Total)) + 
  geom_boxplot()+ xlab("Zonas") + ylab("Individuos")+
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
  )

#Modelos mixto con intercepto aleatorio. 
#No me interasa la pendiente, porque mi efecto fijo es categorico

glm.1 <- glmer (Total~Zona +(1|Forofito), family = poisson, data=df)
summary(glm.1) #no contribuye incluir efecto aleatorio desviacion estandar y varianza demasiado pequeña

#Ahora sin efecto aleatorio

mod1<-glm(Total~Zona,family="poisson", data=df)
summary(mod1)
Anova(mod1)#si hay relacion entre zona y abundancia


#Sobredispersión
mod1$deviance/mod1$df.residual #terrible sobredispersion
#Para ver si el coefficient de sobre dispercion en significativo
library(AER)
dispersiontest(mod1, trafo=1) # Ha= si hay sobre disperison

###Corregir SObredispersion####
###############################

#Con D cuasi-Poisson###
mod2<-glm(Total~Zona,family="quasi"(link="log",variance="mu"), data=df)
summary(mod2)
Anova(mod2)

#devianza explicada Pseudo r cuadrado
(1857-1541)/1857 #Casi Nada

###Modelo binomialnegativo##
library(MASS)
mod3 <- glm.nb(Total~Zona, data=df)
summary(mod3)
Anova(mod3)
#devianza explicada Pseudo r cuadrado
(185.68-163.61)/185.68 #Casi Nada

#Comparar Binomial y Poisson##
########################3###
x1BN<- logLik(mod3)
x2P <- logLik(mod1)
#Otra manera
d <-2*(x1BN-x2P) #estadistico. Con distribucion chicuadrado con un grado de libertad
pval <- 0.5*pchisq(as.numeric (d), df =1, lower.tail = FALSE) #Multiplica significancia por dos
#Que tan probable es encontara en la distribucion chicuadro un valor igual o 
#mas extremso que nuestro estadistico de prueba  es decir el valor P
pval #Si hay sobre dispersion, Mejor usar BN
#Si hay sobre y mejor usar Binomial

#Prueba de comparación múltiple PARA SABER ENTRE QUE FACTORS HAY DIFERENCIAS
library(lsmeans)
library(multcomp)
Comp <- lsmeans(mod3, ~Zona)
pairs(Comp, adj="bon") #Fishser con correcion de bonferroni
cld(Comp, alpha=0.05, adj="bon", Letters=letters)
#Zona 4 diferentes a todas


############MODELOS LINEALES CON VARIABLE DAP#########################
###################################################################################
##################
###################################################################################

library(lme4)
library(car)
library(dplyr)
library(openxlsx)
library(ggplot2)
library(hrbrthemes)


##########################################
##Modelo lineal generalizado mixto - GLMM Con distribucion Poisson
####################################################
#Load Data set
abundancia <- read.xlsx("DAP.Total.xlsx", rowNames = FALSE)
summary(abundancia)
#grafica
ggplot(abundancia, aes(x=DAP, y=Individuos)) + 
  geom_point() +
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent')
  )
#MODELO POISSON
mod1<-glm(Individuos~DAP,family="poisson", data=abundancia)
summary(mod1)
Anova(mod1)#si hay relacion entre zona y abundancia

#Sobredispersión
mod1$deviance/mod1$df.residual #terrible sobredispersion
#Para ver si el cieficioente de sobre dispercion en significativo
library(AER)
dispersiontest(mod1, trafo=1) # Ha= si hay sovbre disperison
###Corregir SObredispersion####
###############################

#Con D cuasi-Poisson###
mod2<-glm(Individuos~DAP,family="quasi"(link="log",variance="mu"), data=abundancia)
summary(mod2)
Anova(mod2)

#devianza explicada Pseudo r cuadrado
(777.23-399.91 )/1857 #Casi Nada

###Modelo binomialnegativo##
library(MASS)
mod3 <- glm.nb(Individuos~DAP, data=abundancia)
summary(mod3)
Anova(mod3)
#devianza explicada Pseudo r cuadrado
(66.565-37.453)/66.565 #Casi Nada

#Comparar Binomial y Poisson##
########################3###
x1BN<- logLik(mod3) #mejro el Binomial Negativo. y explia un poqeuito mas con pseudor
x2P <- logLik(mod1)


