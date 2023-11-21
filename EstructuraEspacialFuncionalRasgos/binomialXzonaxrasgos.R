###########BINOMIAL PARA RASGOS X ZONAS###############
#######################################################
####################################################

#Respuesta en Proporción de Indi
## Load necessary packages
#########################
library(faraway)
library(openxlsx)
library(ggplot2)
library(dplyr)

#Load Data set
rasgos<- read.xlsx("Rasgos.xlsx", rowNames = FALSE)

#separar tablas
espinas<- select(rasgos,Forofito,Zona,SinEspinas,ConEspinas,TotalIndividuos2)
Clonalidad <- select(rasgos,Forofito,Zona,Rizoma,Estolon,SinClonalidad,TotalIndividuos3)
EstrategiaTomaNutrient <- select(rasgos,Forofito,Zona,Tanque,Ericoide,Orchidoide,AM,AMDSE,DSE,SinEstrategiaTomaNutrientes,TotalIndividuos4)
Dispersion <- select(rasgos,Forofito,Zona,Anemocoria,Endocoria,SinAyuda,TotalIndividuos5)
habito <- select(rasgos,Forofito,Zona,Hemiepifitas,Liananomadica,Epifita,TotalIndividuos6)
metabolismo<- rasgos[,1:5]


#Proporciones.Crear variable nueva dentro de datos

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

######################################## Modelos :: BINOMIAL PARA PROPORCIONES##################

#El argumento weights especifica el número de observaciones por "muestra". pARA SABER QUE HAY DIFENRCIAS ENTRE CADA UNA DE LAS UINIDADESE EXPERIMENTALES (Entre zonas)

glm.espi1<- glm(ConEspinas/TotalIndividuos2~Zona, weights=TotalIndividuos2,
               family=binomial, data=espinas)


glm.clon1<- glm (((Clonalidad$Rizoma + Clonalidad$Estolon)/Clonalidad$TotalIndividuos3)~Zona,family=binomial, weights = TotalIndividuos3, data=Clonalidad)

glm.estrat1<- glm (((EstrategiaTomaNutrient$Tanque + EstrategiaTomaNutrient$Ericoide +
                       EstrategiaTomaNutrient$Orchidoide + EstrategiaTomaNutrient$AM + 
                       EstrategiaTomaNutrient$AMDSE + EstrategiaTomaNutrient$DSE)/ EstrategiaTomaNutrient$TotalIndividuos4)~Zona, family=binomial, weights = TotalIndividuos4, data=EstrategiaTomaNutrient)

glm.anemo1<- glm ((Dispersion$Anemocoria/ Dispersion$TotalIndividuos5)~Zona,family=binomial, weights = TotalIndividuos5, data=Dispersion)

glm.zoo1<- glm ( Dispersion$Endocoria/Dispersion$TotalIndividuos5~Zona,family=binomial, weights = TotalIndividuos5, data=Dispersion)

glm.epif1<- glm (habito$Epifita /habito$TotalIndividuos6~Zona,family=binomial, weights = TotalIndividuos6, data=habito)

glm.hemi1<- glm (habito$Hemiepifitas /habito$TotalIndividuos6~Zona,family=binomial, weights = TotalIndividuos6, data=habito)

glm.lian1<- glm (habito$Liananomadica /habito$TotalIndividuos6~Zona,family=binomial, weights = TotalIndividuos6, data=habito)


glm.cam1<- glm(CAM/TotalIndividuos~Zona, weights=TotalIndividuos,
                  family=binomial, data=metabolismo)

glm.c31<- glm(C3/TotalIndividuos~Zona, weights=TotalIndividuos,
               family=binomial, data=metabolismo)


#Significancia de las covariables (la zona) #WALD TESTSI SON O NO SIGNIFICATICA LA VARIABLE X. eSTA Z NECESITA MUCHOS DATOS. pOR LO QUE SE UEDE HACER CON OTA PRUBEA

summary(glm.espi1)
summary(glm.clon1)
summary(glm.estrat1)
summary(glm.anemo1)
summary(glm.zoo1)
summary(glm.epif1)
summary(glm.hemi1)
summary(glm.lian1)
summary(glm.cam1) 
summary(glm.c31)
deviance(glm.cam1)

#Prueba MAS ADECAUADA POR QEU ES COMPARACION DE MODELOS CON MV. Para significance de X (zonas) ANOVA CON PRUEBA COCIENTE DE VEROSIMILITUD
library(car)

Anova(glm.espi1, test="LR")#SIGNIFICATIVO
Anova(glm.clon1, test="LR")#SIGNIFICATIVO
Anova(glm.estrat1, test="LR")#SIGNIFICATIVO
Anova(glm.anemo1, test="LR")#SIGNIFICATIVO
Anova(glm.zoo1, test="LR")#SIGNIFICATIVO
Anova(glm.epif1, test="LR")
Anova(glm.hemi1, test="LR")#
Anova(glm.lian1, test="LR")#
Anova(glm.cam1, test="LR")# SIGNIFICATIVO
Anova(glm.c31, test="LR")# SIGNIFICATIVO

#Ajuste del modelo - Pseudo R^2. Devianza explicada# proprocion de varianza de Y explicada por mi X

(glm.espi1$null.deviance - glm.espi1$deviance)/glm.espi1$null.deviance*100
(glm.clon1$null.deviance - glm.clon1$deviance)/glm.clon1$null.deviance*100
(glm.estrat1$null.deviance - glm.estrat1$deviance)/glm.estrat1$null.deviance*100
(glm.anemo1$null.deviance - glm.anemo1$deviance)/glm.anemo1$null.deviance*100
(glm.zoo1$null.deviance - glm.zoo1$deviance)/glm.zoo1$null.deviance*100
(glm.epif1$null.deviance - glm.epif1$deviance)/glm.epif1$null.deviance*100
(glm.hemi1$null.deviance - glm.hemi1$deviance)/glm.hemi1$null.deviance*100
(glm.lian1$null.deviance - glm.lian1$deviance)/glm.lian1$null.deviance*100
(glm.cam1$null.deviance - glm.cam1$deviance)/glm.cam1$null.deviance*100
(glm.c31$null.deviance - glm.c31$deviance)/glm.c31$null.deviance*100

#Sobredispersión # de veces (+ veces) en que la variabilidad de los datos se puede explciar por el modelo #la variablidad de los datos es 4 veces mayor que la variabilidad que puede predecir el modelo. Hay sobre dispercion

i <- c (glm.espi1$deviance, glm.clon1$deviance, glm.estrat1$deviance, 
        glm.anemo1$deviance, glm.zoo1$deviance, glm.epif1$deviance,glm.hemi1$deviance,
        glm.lian1$deviance, glm.cam1$deviance,glm.c31$deviance ) 

j <- c(glm.espi1$df.residual, glm.clon1$df.residual, glm.estrat1$df.residual, 
       glm.anemo1$df.residual, glm.zoo1$df.residual, glm.epif1$df.residual,glm.hemi1$df.residual,
       glm.lian1$df.residual, glm.cam1$df.residual,glm.c31$df.residual )

glm.cam1$deviance/glm.cam1$df.residual


#acá devianzas:

glm.espi1$deviance/glm.espi1$df.residual
glm.clon1$deviance/glm.clon1$df.residual
glm.estrat1$deviance/glm.estrat1$df.residual
glm.anemo1$deviance/glm.anemo1$df.residual
glm.zoo1$deviance/glm.zoo1$df.residual
glm.epif1$deviance/glm.epif1$df.residual #ok
glm.hemi1$deviance/glm.hemi1$df.residual #OK
glm.lian1$deviance/ glm.lian1$df.residual #oK
glm.cam1$deviance/ glm.cam1$df.residual
glm.c31$deviance/glm.c31$df.residual

#Modelo ajustado con sobredispersión. Usando Distribucion quasibinomial########
#################################################################################
############################################################################

glm.espi1<- glm(ConEspinas/TotalIndividuos2~Zona, weights=TotalIndividuos2,
                family=quasibinomial, data=espinas)


glm.clon1<- glm (((Clonalidad$Rizoma + Clonalidad$Estolon)/Clonalidad$TotalIndividuos3)~Zona,family=quasibinomial, weights = TotalIndividuos3, data=Clonalidad)

glm.estrat1<- glm (((EstrategiaTomaNutrient$Tanque + EstrategiaTomaNutrient$Ericoide +
                       EstrategiaTomaNutrient$Orchidoide + EstrategiaTomaNutrient$AM + 
                       EstrategiaTomaNutrient$AMDSE + EstrategiaTomaNutrient$DSE)/ EstrategiaTomaNutrient$TotalIndividuos4)~Zona, family=quasibinomial, weights = TotalIndividuos4, data=EstrategiaTomaNutrient)

glm.anemo1<- glm ((Dispersion$Anemocoria/ Dispersion$TotalIndividuos5)~Zona,family=quasibinomial, weights = TotalIndividuos5, data=Dispersion)

glm.zoo1<- glm ( Dispersion$Endocoria/Dispersion$TotalIndividuos5~Zona,family=quasibinomial, weights = TotalIndividuos5, data=Dispersion)


glm.cam1<- glm(CAM/TotalIndividuos~Zona, weights=TotalIndividuos,
               family=quasibinomial, data=metabolismo)

glm.c31<- glm(C3/TotalIndividuos~Zona, weights=TotalIndividuos,
              family=quasibinomial, data=metabolismo)

summary(glm.espi1)
summary(glm.clon1)
summary(glm.estrat1)
summary(glm.anemo1)
summary(glm.zoo1)
summary(glm.cam1) 
summary(glm.c31)


Anova(glm.espi1, test="LR")
Anova(glm.clon1, test="LR")#SIGN
Anova(glm.estrat1, test="LR")#SI
Anova(glm.anemo1, test="LR")#S
Anova(glm.zoo1, test="LR")#SI
Anova(glm.cam1, test="LR")# 
Anova(glm.c31, test="LR")# 




summary(glm.cam2)
Anova(glm.cam2, test="LR")

#devianza explicada pseudoR

(glm.espi1$null.deviance - glm.espi1$deviance)/glm.espi1$null.deviance*100
(glm.clon1$null.deviance - glm.clon1$deviance)/glm.clon1$null.deviance*100
(glm.estrat1$null.deviance - glm.estrat1$deviance)/glm.estrat1$null.deviance*100
(glm.anemo1$null.deviance - glm.anemo1$deviance)/glm.anemo1$null.deviance*100
(glm.zoo1$null.deviance - glm.zoo1$deviance)/glm.zoo1$null.deviance*100
(glm.cam1$null.deviance - glm.cam1$deviance)/glm.cam1$null.deviance*100
(glm.c31$null.deviance - glm.c31$deviance)/glm.c31$null.deviance*100

(glm.cam2$null.deviance - glm.cam2$deviance)/glm.cam2$null.deviance*100

#PRUEBAS DE COMPARACION MULTIPLE 

library(lsmeans)
library(multcomp)
library(multcompView)

Compcam <-lsmeans(glm.cam1, ~Zona)
pairs(Compcam, adj="bon") #bonferroni es para variables respuesta no cuantitativas
cld(Compcam,alpha=0.05, adj="bon", Letters=letters)

CompEspina <-lsmeans(glm.espi1, ~Zona)
pairs(CompEspina, adj="bon") #bonferroni es para variables respuesta no cuantitativas
cld(CompEspina,alpha=0.05, adj="bon", Letters=letters)

Compestrategi<-lsmeans(glm.estrat1, ~Zona)
pairs(Compestrategi, adj="bon") #bonferroni es para variables respuesta no cuantitativas
cld(Compestrategi,alpha=0.05, adj="bon", Letters=letters)

Compc3<-lsmeans(glm.c31, ~Zona)
pairs(Compc3, adj="bon") #bonferroni es para variables respuesta no cuantitativas
cld(Compc3,alpha=0.05, adj="bon", Letters=letters)


