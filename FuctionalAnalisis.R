# Load necessary packages
#########################
library(faraway)
library(openxlsx)
library(ggplot2)
library(vctrs)
library(dplyr)
library(lme4)
library(ggeffects)

#Load Data set
rasgos<- read.xlsx("Rasgos.xlsx", rowNames = FALSE)

#separar tablas
espinas<- select(rasgos,Forofito,Zona,DBH,Height,SinEspinas,ConEspinas,TotalIndividuos2)
Clonalidad <- select(rasgos,Forofito,Zona,DBH,Height,Rizoma,Estolon,SinClonalidad,TotalIndividuos3)
EstrategiaTomaNutrient <- select(rasgos,Forofito,Zona,DBH,Height,Tanque,Ericoide,Orchidoide,AM,AMDSE,DSE,SinEstrategiaTomaNutrientes,TotalIndividuos4)
Dispersion <- select(rasgos,Forofito,Zona,DBH,Height,Anemocoria,Endocoria,SinAyuda,TotalIndividuos5)
habito <- select(rasgos,Forofito,Zona,DBH,Height,Hemiepifitas,Liananomadica,Epifita,TotalIndividuos6)
metabolismo<- select(rasgos,Forofito,Zona,DBH,Height,CAM,C3,TotalIndividuos)


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
#1) Binomial SIN efecto aleatorio.
glm.espi1<- glm(presprop~Zona + DBH + Height, weights=TotalIndividuos2,family=binomial, data=espinas)
glm.clon1<- glm (presprop~Zona + DBH + Height,family=binomial, weights = TotalIndividuos3, data=Clonalidad)
glm.estrat1<- glm (presprop~Zona + DBH + Height, family=binomial, weights = TotalIndividuos4, data=EstrategiaTomaNutrient)
glm.anemo1<- glm (Anemprop~Zona+ DBH + Height,family=binomial, weights = TotalIndividuos5, data=Dispersion)
glm.zoo1<- glm (zooprop~Zona + DBH + Height,family=binomial, weights = TotalIndividuos5, data=Dispersion)
glm.epif1<- glm (epifiprop~Zona + DBH + Height,family=binomial, weights = TotalIndividuos6, data=habito)
glm.hemi1<- glm (hemiprop~Zona + DBH + Height, family=binomial, weights = TotalIndividuos6, data=habito)
glm.lian1<- glm (Lianaprop~Zona + DBH + Height,family=binomial, weights = TotalIndividuos6, data=habito)
glm.cam1<- glm(camprop~Zona + DBH + Height, weights=TotalIndividuos,family=binomial, data=metabolismo)
glm.c31<- glm(c3prop~Zona+ DBH + Height, weights=TotalIndividuos, family=binomial, data=metabolismo)
#2) Binomial CON efecto aleatorio.
glm.espi2 <- glmer (presprop~Zona + DBH + Height + (1|Forofito), family = binomial, data=espinas)
glm.clon2 <- glmer (presprop~Zona + DBH + Height + (1|Forofito), family = binomial, data=Clonalidad)
glm.estrat2 <- glmer (presprop~Zona + DBH + Height + (1|Forofito), family = binomial, data=EstrategiaTomaNutrient)
glm.anemo2<- glmer (Anemprop~Zona+ DBH + Height + (1|Forofito), family = binomial, data=Dispersion)
glm.zoo2 <- glmer (zooprop~Zona + DBH + Height + (1|Forofito), family = binomial, data=Dispersion)
glm.epif2<- glmer (epifiprop~Zona + DBH + Height + (1|Forofito), family = binomial, data=habito)
glm.hemi2<- glmer (hemiprop~Zona + DBH + Height + (1|Forofito), family = binomial, data=habito)
glm.lian2<- glmer (Lianaprop~Zona + DBH + Height + (1|Forofito), family = binomial, data=habito)
glm.cam2<- glmer (camprop~Zona + DBH + Height + (1|Forofito), family=binomial, data=metabolismo)
glm.c32<- glmer (c3prop~Zona+ DBH + Height + (1|Forofito), family=binomial, data=metabolismo)

######################################## MODELOS :: QUASIBINOMIAL PROPORCIONES##################
#3) QuasiBinomial SIN efecto aleatorio.
glm.espi3<- glm(presprop~Zona + DBH + Height, weights=TotalIndividuos2,family=quasibinomial, data=espinas)
glm.clon3<- glm (presprop~Zona + DBH + Height,family=quasibinomial, weights = TotalIndividuos3, data=Clonalidad)
glm.estrat3<- glm (presprop~Zona + DBH + Height, family=quasibinomial, weights = TotalIndividuos4, data=EstrategiaTomaNutrient)
glm.anemo3<- glm (Anemprop~Zona+ DBH + Height,family=quasibinomial, weights = TotalIndividuos5, data=Dispersion)
glm.zoo3<- glm (zooprop~Zona + DBH + Height,family=quasibinomial, weights = TotalIndividuos5, data=Dispersion)
glm.epif3<- glm (epifiprop~Zona + DBH + Height,family=quasibinomial, weights = TotalIndividuos6, data=habito)
glm.hemi3<- glm (hemiprop~Zona + DBH + Height, family=quasibinomial, weights = TotalIndividuos6, data=habito)
glm.lian3<- glm (Lianaprop~Zona + DBH + Height,family=quasibinomial, weights = TotalIndividuos6, data=habito)
glm.cam3<- glm(camprop~Zona + DBH + Height, weights=TotalIndividuos,family=quasibinomial, data=metabolismo)
glm.c33<- glm(c3prop~Zona+ DBH + Height, weights=TotalIndividuos, family=quasibinomial, data=metabolismo)

#Chose a best model##

# Calcular AIC y BIC para todos los modelos
AIC_values <- AIC(glm.espi1, glm.espi2, glm.espi3, glm.clon1, glm.clon2, glm.clon3, glm.estrat1, glm.estrat2, 
                  glm.estrat3, glm.anemo1, glm.anemo2, glm.anemo3, 
                  glm.zoo1, glm.zoo2, glm.zoo3, glm.epif1, glm.epif2, glm.epif3, glm.hemi1,
                  glm.hemi2, glm.hemi3, glm.lian1, glm.lian2,
                  glm.lian3, glm.cam1, glm.cam2, glm.cam3, glm.c31, glm.c32, glm.c33 )
BIC_values <- BIC(glm.espi1, glm.espi2, glm.espi3, glm.clon1, glm.clon2, glm.clon3, glm.estrat1, glm.estrat2, 
                  glm.estrat3, glm.anemo1, glm.anemo2, glm.anemo3, 
                  glm.zoo1, glm.zoo2, glm.zoo3, glm.epif1, glm.epif2, glm.epif3, glm.hemi1,
                  glm.hemi2, glm.hemi3, glm.lian1, glm.lian2,
                  glm.lian3, glm.cam1, glm.cam2, glm.cam3, glm.c31, glm.c32, glm.c33)
# Imprimir los valores de AIC y BIC
print(AIC_values) #Binomial with random effect
print(BIC_values) #Binomial with random effect

summary(glm.espi2) #NO signi
summary(glm.clon2) #NO signi
summary(glm.estrat2) # Zone. 
summary(glm.anemo2) #DBH
summary(glm.zoo2) # DBH
summary(glm.epif2) #NO sign
summary(glm.hemi2) #NO sign
summary(glm.lian2) #NO sign
summary(glm.cam2) # Zone and DBH
summary(glm.c32) # Zone and DBH

#PRUEBAS DE COMPARACION MULTIPLE 

library(lsmeans)
library(multcomp)
library(multcompView)

Compestrat<-lsmeans(glm.estrat2, ~Zona)
pairs(Compestrat, adj="bon") #bonferroni es para variables respuesta no cuantitativas
cld(Compcam,alpha=0.05, adj="bon", Letters=letters) #No diference

Companemo <-lsmeans(glm.anemo2, ~Zona)
pairs(Companemo, adj="bon") #bonferroni es para variables respuesta no cuantitativas
cld(Companemo,alpha=0.05, adj="bon", Letters=letters)

Compglm.zoo <-lsmeans(glm.zoo2, ~Zona)
pairs(Compglm.zoo, adj="bon") #bonferroni es para variables respuesta no cuantitativas
cld(Compglm.zoo, alpha=0.05, adj="bon", Letters=letters)

Compcam <- lsmeans(glm.cam2, ~Zona)
pairs(Compcam, adj="bon") #bonferroni es para variables respuesta no cuantitativas
cld(Compcam,alpha=0.05, adj="bon", Letters=letters)

Compc3 <- lsmeans(glm.c32, ~Zona)
pairs(Compc3, adj="bon") #bonferroni es para variables respuesta no cuantitativas
cld(Compc3,alpha=0.05, adj="bon", Letters=letters)

#Grafico de efectos parciales#No se si las necesito en verdad...

effectsAnemo <- ggpredict(glm.anemo2, terms = c("DBH", "Zona"))
plot(effectsAnemo) + labs(x = "DBH (cm)", y = "Anemochory") +
  theme(axis.line = element_line(color = "black"), 
        axis.text = element_text(color = "black", size = 18),
        axis.title = element_text(size = 18, color = "black"),
        panel.grid.minor = element_blank(),
        plot.title = element_blank()) 
effectszoo <- ggpredict(glm.zoo2, terms = c("DBH", "Zona"))
plot(effectszoo) + labs(x = "DBH (cm)", y = "Endozoochory") +
  theme(axis.line = element_line(color = "black"), 
        axis.text = element_text(color = "black", size = 18),
        axis.title = element_text(size = 18, color = "black"),
        panel.grid.minor = element_blank(),
        plot.title = element_blank()) 
effectsc3 <- ggpredict(glm.c32, terms = c("DBH", "Zona"))
plot(effectsc3)


write.csv(espinas, file= "espinas.csv", col.names = TRUE)
write.csv(Dispersion, file= "dispersion.csv", col.names = TRUE)
write.csv(habito, file= "habito.csv", col.names = TRUE)
write.csv(metabolismo, file= "metabolismo.csv", col.names = TRUE)
write.csv(EstrategiaTomaNutrient, file= "estrategiatomadenutrientes.csv", col.names = TRUE)
write.csv(Clonalidad, file= "clonalidad.csv", col.names = TRUE)



####MULTIVARIATE REGRESSION TREES #######
#Here, I am trying to created grups of trees.#
#The goal es identificar grupos homogeneos de hospederos para posteriormente modelas los indices.
library(partykit)

#Load Data set

datacom<- read.xlsx("Fuctional.xlsx", rowNames = FALSE)
summary(datacom)
datacom$Zona <- factor(datacom$Zona)


#a simple basic fitting fuction (of type 1) for a logistic refresion
logit <- function(y, x, start = NULL, weights = NULL, offset = NULL, ...) {
  glm(y ~ 0 + x, family = gaussian, start = start, ...)
}
GruposFuncionales <- mob (Fdiv ~ Zona | DBH + Height + 
                            Spines + Clonality + WindDisper + ZooDisper+ TrueEpiphyte + Hemiepiphyte + 
                            Nomadicvines + Cam + C3 + NutrientInta, data = datacom, fit = logit)

plot(GruposFuncionales)

GruposFuncionales1 <- mob (Feve ~ Zona | DBH + Height + 
                             Spines + Clonality + WindDisper + ZooDisper+ TrueEpiphyte + Hemiepiphyte + 
                             Nomadicvines + Cam + C3 + NutrientInta, data = datacom, fit = logit)

plot(GruposFuncionales1)
summary(GruposFuncionales1, node = 1)

GruposFuncionales2 <- mob (Fric ~ Zona | DBH + Height + 
                             Spines + Clonality + WindDisper + ZooDisper+ TrueEpiphyte + Hemiepiphyte + 
                             Nomadicvines + Cam + C3 + NutrientInta, data = datacom, fit = logit)
plot(GruposFuncionales2)

#No se pueden establecer grupos.. 

####MODELOS BETA PARA TODO LOS DATOS, PORQUE NO EXISTEN GRUPOS####
#############################################################################

library(glmmTMB)
#1) Beta SIN efecto aleatorio.
glm.FRIC<- glmmTMB(Fric ~ Zona + DBH + Height, family = beta_family, data = datacom)
glm.FEVE<- glmmTMB(Feve ~ Zona + DBH + Height, family = beta_family, data = datacom)
glm.FDIV<- glmmTMB(Fdiv ~ Zona + DBH + Height, family = beta_family, data = datacom)

#2) Beta  CON efecto aleatorio.
glm.FRIC2<- glmmTMB (Fric ~ Zona + DBH + Height + (1 | Forofito), family = beta_family, data = datacom)
glm.FEVE2 <- glmmTMB (Feve ~ Zona + DBH + Height + (1 | Forofito), family = beta_family, data = datacom)
glm.FDIV2 <- glmmTMB (Fdiv ~ Zona + DBH + Height + (1 | Forofito), family = beta_family, data = datacom)

#Chose the best model##

AIC_values2 <- AIC(glm.FRIC, glm.FEVE, glm.FDIV, glm.FRIC2, glm.FEVE2, glm.FDIV2)
BIC_values2 <- BIC(glm.FRIC, glm.FEVE, glm.FDIV, glm.FRIC2, glm.FEVE2, glm.FDIV2)
# Imprimir los valores de AIC y BIC
print(AIC_values2) #FRIC2, FEVE, FDIV2
print(BIC_values2) #FRIC, FEVE, FDIV2

summary(glm.FRIC2) #DBH + ZONA
summary(glm.FEVE) # Height + Zona (?)
summary(glm.FDIV2) # Height + Zona (?)
#Grafico de efectos parciales#No se si las necesito en verdad...

effects2 <- ggpredict(glm.FRIC2, terms = c("DBH", "Zona"))
plot(effects2) + labs(x = "DBH (cm)", y = "FRic") +
  theme(axis.line = element_line(color = "black"), 
        axis.text = element_text(color = "black", size = 18),
        axis.title = element_text(size = 18, color = "black"),
        panel.grid.minor = element_blank(),
        plot.title = element_blank()) 

effects3 <- ggpredict(glm.FEVE, terms = c("Height", "Zona"))
plot(effects3) + labs(x = "Height (m)", y = "FEve") +
  theme(axis.line = element_line(color = "black"), 
        axis.text = element_text(color = "black", size = 18),
        axis.title = element_text(size = 18, color = "black"),
        panel.grid.minor = element_blank(),
        plot.title = element_blank()) 


effects4 <- ggpredict(glm.FDIV2, terms = c("Height", "Zona"))
plot(effects4) + labs(x = "Height (m)", y = "FDiv") +
  theme(axis.line = element_line(color = "black"), 
        axis.text = element_text(color = "black", size = 18),
        axis.title = element_text(size = 18, color = "black"),
        panel.grid.minor = element_blank(),
        plot.title = element_blank())




#PRUEBAS DE COMPARACION MULTIPLES

CompFRIC<-lsmeans(glm.FRIC2, ~Zona)
pairs(CompFRIC, adj="bon") #NO SIGNIFICATIVA DIFERENCIA


ComFEVE <-lsmeans(glm.FEVE, ~Zona)
pairs(ComFEVE, adj="bon") #NO SIGNIFICATIVA DIFERENCIA

ComFDIV <-lsmeans(glm.FDIV2, ~Zona)
pairs(ComFDIV, adj="bon") # NO SIGNIFICATIVA DIFERENCIA 


##Interpretations: Small Sample Sizes:  If you have a small number of 
#observations within each zone, the power to detect pairwise 
#differences might be low, even if there's an overall effect.

#Interaction Effects: Other factors interacting with "Zona" could be influencing Fric.
#The overall model might capture these interactions, but lsmeans only considers the main effect of "Zona."

#while individual zone comparisons might not reveal clear differences due to this non-linearity.


####Randomization functions ONLY FRO FRIC########3
library(FD)
library(picante)
library(DarkDiv)
#“Randomize community data matrix abundances within samples (maintains sample species richness)”
set.seed(1)
numberReps <- 100
customRandomize <- function(samp, null.model, iterations, minAbundance = 1) {
  # Randomize matrix using picante's randomizeMatrix
  randomizedPA <- randomizeMatrix(samp = samp, null.model = null.model, iterations = iterations)
  
  # Check for columns with total abundance less than minAbundance
  columnSums <- apply(randomizedPA, 2, sum)
  lowAbundanceCols <- which(columnSums < minAbundance)
  
  # If there are low abundance columns, redistribute abundance
  if (length(lowAbundanceCols) > 0) {
    for (col in lowAbundanceCols) {
      # Select rows with non-zero abundance in the low abundance column
      nonzeroRows <- which(randomizedPA[, col] > 0)
      
      # Redistribute abundance from non-zero rows to zero rows
      randomizedPA[nonzeroRows, col] <- randomizedPA[nonzeroRows, col] - minAbundance
      randomizedPA[which(randomizedPA[, col] == 0), col] <- minAbundance
    }
  }
  
  # Return the modified randomizedPA matrix
  return(randomizedPA)
}

randomizedPA <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith1 <- as.matrix (dbFD (Rasgos, randomizedPA, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA2 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith2 <- as.matrix (dbFD (Rasgos, randomizedPA2, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA3 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith3 <- as.matrix (dbFD (Rasgos, randomizedPA3, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA4 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith4 <- as.matrix (dbFD (Rasgos, randomizedPA4, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA5 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith5 <- as.matrix (dbFD (Rasgos, randomizedPA5, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA6 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith6 <- as.matrix (dbFD (Rasgos, randomizedPA6, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA7 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith7 <- as.matrix (dbFD (Rasgos, randomizedPA7, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA8 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith8 <- as.matrix (dbFD (Rasgos, randomizedPA8, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA9 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith9 <- as.matrix (dbFD (Rasgos, randomizedPA9, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA10 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith10 <- as.matrix (dbFD (Rasgos, randomizedPA10, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)

randomizedPA11 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith11 <- as.matrix (dbFD (Rasgos, randomizedPA11, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA12 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith12 <- as.matrix (dbFD (Rasgos, randomizedPA12, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA13 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith13 <- as.matrix (dbFD (Rasgos, randomizedPA13, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA14 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith14 <- as.matrix (dbFD (Rasgos, randomizedPA14, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA15 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith15 <- as.matrix (dbFD (Rasgos, randomizedPA15, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA16 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith16 <- as.matrix (dbFD (Rasgos, randomizedPA16, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA17 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith17 <- as.matrix (dbFD (Rasgos, randomizedPA17, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA18 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith18 <- as.matrix (dbFD (Rasgos, randomizedPA18, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA19 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith19 <- as.matrix (dbFD (Rasgos, randomizedPA19, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA20 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith20 <- as.matrix (dbFD (Rasgos, randomizedPA20, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)

randomizedPA21 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith21 <- as.matrix (dbFD (Rasgos, randomizedPA21, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA22 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith22 <- as.matrix (dbFD (Rasgos, randomizedPA22, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA23 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith23 <- as.matrix (dbFD (Rasgos, randomizedPA23, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA24 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith24 <- as.matrix (dbFD (Rasgos, randomizedPA24, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA25 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith25 <- as.matrix (dbFD (Rasgos, randomizedPA25, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA26 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith26 <- as.matrix (dbFD (Rasgos, randomizedPA26, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA27 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith27 <- as.matrix (dbFD (Rasgos, randomizedPA27, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA28 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith28 <- as.matrix (dbFD (Rasgos, randomizedPA28, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA29 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith29 <- as.matrix (dbFD (Rasgos, randomizedPA29, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA30 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith30 <- as.matrix (dbFD (Rasgos, randomizedPA30, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)

randomizedPA31 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith31 <- as.matrix (dbFD (Rasgos, randomizedPA31, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA32 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith32 <- as.matrix (dbFD (Rasgos, randomizedPA32, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA33 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith33 <- as.matrix (dbFD (Rasgos, randomizedPA33, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA34 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith34 <- as.matrix (dbFD (Rasgos, randomizedPA34, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA35 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith35 <- as.matrix (dbFD (Rasgos, randomizedPA35, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA36 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith36 <- as.matrix (dbFD (Rasgos, randomizedPA36, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA37 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith37 <- as.matrix (dbFD (Rasgos, randomizedPA37, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA38 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith38 <- as.matrix (dbFD (Rasgos, randomizedPA38, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA39 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith39 <- as.matrix (dbFD (Rasgos, randomizedPA39, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA40 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith40 <- as.matrix (dbFD (Rasgos, randomizedPA40, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)

randomizedPA41 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith41 <- as.matrix (dbFD (Rasgos, randomizedPA41, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA42 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith42 <- as.matrix (dbFD (Rasgos, randomizedPA42, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA43 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith43 <- as.matrix (dbFD (Rasgos, randomizedPA43, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA44 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith44 <- as.matrix (dbFD (Rasgos, randomizedPA44, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA45 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith45 <- as.matrix (dbFD (Rasgos, randomizedPA45, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA46 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith46 <- as.matrix (dbFD (Rasgos, randomizedPA46, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA47 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith47 <- as.matrix (dbFD (Rasgos, randomizedPA47, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA48 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith48 <- as.matrix (dbFD (Rasgos, randomizedPA48, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA49 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith49 <- as.matrix (dbFD (Rasgos, randomizedPA49, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA50 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith50 <- as.matrix (dbFD (Rasgos, randomizedPA50, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)

randomizedPA51 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith51 <- as.matrix (dbFD (Rasgos, randomizedPA51, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA52 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith52 <- as.matrix (dbFD (Rasgos, randomizedPA52, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA53 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith53 <- as.matrix (dbFD (Rasgos, randomizedPA53, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA54 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith54 <- as.matrix (dbFD (Rasgos, randomizedPA54, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA55 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith55 <- as.matrix (dbFD (Rasgos, randomizedPA55, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA56 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith56 <- as.matrix (dbFD (Rasgos, randomizedPA56, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA57 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith57 <- as.matrix (dbFD (Rasgos, randomizedPA57, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA58 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith58 <- as.matrix (dbFD (Rasgos, randomizedPA58, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA59 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith59 <- as.matrix (dbFD (Rasgos, randomizedPA59, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA60 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith60 <- as.matrix (dbFD (Rasgos, randomizedPA60, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)

randomizedPA61 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith61 <- as.matrix (dbFD (Rasgos, randomizedPA61, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA62 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith62 <- as.matrix (dbFD (Rasgos, randomizedPA62, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA63 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith63 <- as.matrix (dbFD (Rasgos, randomizedPA63, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA64 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith64 <- as.matrix (dbFD (Rasgos, randomizedPA64, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA65 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith65 <- as.matrix (dbFD (Rasgos, randomizedPA65, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA66 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith66 <- as.matrix (dbFD (Rasgos, randomizedPA66, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA67 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith67 <- as.matrix (dbFD (Rasgos, randomizedPA67, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA68 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith68 <- as.matrix (dbFD (Rasgos, randomizedPA68, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA69 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith69 <- as.matrix (dbFD (Rasgos, randomizedPA69, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA70 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith70 <- as.matrix (dbFD (Rasgos, randomizedPA70, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)

randomizedPA71 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith71 <- as.matrix (dbFD (Rasgos, randomizedPA71, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA72 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith72 <- as.matrix (dbFD (Rasgos, randomizedPA72, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA73 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith73 <- as.matrix (dbFD (Rasgos, randomizedPA73, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA74 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith74 <- as.matrix (dbFD (Rasgos, randomizedPA74, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA75 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith75 <- as.matrix (dbFD (Rasgos, randomizedPA75, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA76 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith76 <- as.matrix (dbFD (Rasgos, randomizedPA76, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA77 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith77 <- as.matrix (dbFD (Rasgos, randomizedPA77, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA78 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith78 <- as.matrix (dbFD (Rasgos, randomizedPA78, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA79 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith79 <- as.matrix (dbFD (Rasgos, randomizedPA79, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA80 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith80 <- as.matrix (dbFD (Rasgos, randomizedPA80, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)

randomizedPA81 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith81 <- as.matrix (dbFD (Rasgos, randomizedPA81, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA82 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith82 <- as.matrix (dbFD (Rasgos, randomizedPA82, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA83 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith83 <- as.matrix (dbFD (Rasgos, randomizedPA83, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA84 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith84 <- as.matrix (dbFD (Rasgos, randomizedPA84, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA85 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith85 <- as.matrix (dbFD (Rasgos, randomizedPA85, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA86 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith86 <- as.matrix (dbFD (Rasgos, randomizedPA86, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA87 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith87 <- as.matrix (dbFD (Rasgos, randomizedPA87, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA88 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith88 <- as.matrix (dbFD (Rasgos, randomizedPA88, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA89 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith89 <- as.matrix (dbFD (Rasgos, randomizedPA89, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA90 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith90 <- as.matrix (dbFD (Rasgos, randomizedPA90, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)

randomizedPA91 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith91 <- as.matrix (dbFD (Rasgos, randomizedPA91, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA92 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith92 <- as.matrix (dbFD (Rasgos, randomizedPA92, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA93 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith93 <- as.matrix (dbFD (Rasgos, randomizedPA93, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA94 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith94 <- as.matrix (dbFD (Rasgos, randomizedPA94, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA95 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith95 <- as.matrix (dbFD (Rasgos, randomizedPA95, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA96 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith96 <- as.matrix (dbFD (Rasgos, randomizedPA96, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA97 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith97 <- as.matrix (dbFD (Rasgos, randomizedPA97, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA98 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith98 <- as.matrix (dbFD (Rasgos, randomizedPA98, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA99 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith99 <- as.matrix (dbFD (Rasgos, randomizedPA99, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)
randomizedPA100 <- customRandomize(samp = Abundancias, null.model = "richness", iterations = 1, minAbundance = 1)
simFaith100 <- as.matrix (dbFD (Rasgos, randomizedPA100, corr = "lingoes", stand.FRic= TRUE , print.pco = TRUE, dist.bin = 2)$FRic)

# Combine columns of simFaith1 and simFaith2
combinedFRicMatrix <- cbind(simFaith1, simFaith2,simFaith3,simFaith4,simFaith5,simFaith6, simFaith7,simFaith8,simFaith9,simFaith10,
                            simFaith11, simFaith12,simFaith13,simFaith14,simFaith15,simFaith16, simFaith17,simFaith18,simFaith19,simFaith20,
                            simFaith21, simFaith22,simFaith23,simFaith24,simFaith25,simFaith26, simFaith27,simFaith28,simFaith29,simFaith30,
                            simFaith31, simFaith32,simFaith33,simFaith34,simFaith35,simFaith36, simFaith37,simFaith38,simFaith39,simFaith40,
                            simFaith41, simFaith42,simFaith43,simFaith44,simFaith45,simFaith46, simFaith47,simFaith48,simFaith49,simFaith50,
                            simFaith51, simFaith52,simFaith53,simFaith54,simFaith55,simFaith56, simFaith57,simFaith58,simFaith59,simFaith60,
                            simFaith61, simFaith62,simFaith63,simFaith64,simFaith65,simFaith66, simFaith67,simFaith68,simFaith69,simFaith70,
                            simFaith71, simFaith72,simFaith73,simFaith74,simFaith75,simFaith76, simFaith77,simFaith78,simFaith79,simFaith80,
                            simFaith81, simFaith82,simFaith83,simFaith84,simFaith85,simFaith86, simFaith87,simFaith88,simFaith89,simFaith90,
                            simFaith91, simFaith92,simFaith93,simFaith94,simFaith95,simFaith96, simFaith97,simFaith98,simFaith99,simFaith100)

# Set row names of the combined matrix (assuming they are the same for both)
rownames(combinedFRicMatrix) <- rownames(simFaith1)  # Or rownames(simFaith2)

# Print the combined FRic matrix
colnames(combinedFRicMatrix) <- c(colnames(simFaith1), colnames(simFaith2))
as.matrix(combinedFRicMatrix)


averageFRic <- rowMeans(combinedFRicMatrix)

# Create a new column for average FRic
newColumn <- matrix(averageFRic, nrow = nrow(combinedFRicMatrix), ncol = 1)

# Insert the new column into combinedFRicMatrix
combinedFRicMatrix <- cbind(combinedFRicMatrix, newColumn)

# Set column names for the average FRic column
colnames(combinedFRicMatrix)[ncol(combinedFRicMatrix)] <- "AverageFRic"

write.csv(combinedFRicMatrix, file= "aleatroizacion.csv")
#SES ANLAYSES

ses <- rasgos<- read.xlsx("ObservEstima.xlsx", rowNames = FALSE)
#ested whether SES were significantly different from zero using a two-tailed Wilcoxon signed rank test
#(Bernard-Verdier et al., 2012).

# Assuming "ses" is a numeric matrix and the variable you want to test is named "variable"

# Extract the variable to be tested
variableValues <- ses[, "variable"]

# Perform the Wilcoxon signed-rank test
wilcoxTestResults <- wilcox.test(ses$SES, alternative = "two.sided")

# Print the test results
print(wilcoxTestResults)
