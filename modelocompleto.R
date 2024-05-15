####
######Abundance GGL-NB############
###################################################################################

library(lme4)
library(car)
library(dplyr)
library(openxlsx)
library(ggplot2)
library(glmmTMB)
library(ggeffects)
#Load Data set
data<- read.xlsx("modelcompleto.xlsx", rowNames = FALSE)
#Grafica
plot(data)

#####Poisson Distribution###
#1) Random factor with poisson distribution without interaction (total=counting variable)##
glm1 <- glmer (Total~Zone+DBH+Height +(1|Host), family = poisson, data=data)
summary(glm1)
# 2) Random factor. With intercept y pendiente aleatorios + interactions
glm2 <- glmer(Total ~ Zone + DBH + Height + Zone:DBH + Zone:Height + DBH:Height + (1 + DBH | Host), family = poisson, data = data)
summary(glm2)
#3) Random factor. With random intercept and interactions
glm3 <- glmer(Total ~ Zone + DBH + Height + Zone:DBH + Zone:Height + DBH:Height + (1|Host), family = poisson, data = data)
summary(glm3)
#4 Without Random factor. Without interactions
glm4 <- glm (Total ~ Zone + DBH + Height, family="poisson", data=data)
summary(glm4)
#5 Without Random factor. With interactions
glm5 <- glm (Total ~ Zone + DBH + Height+ Zone*DBH + Zone*Height + DBH*Height, family="poisson", data=data)
summary(glm5)

####Negative Binomial distribution####
#6  Random factor with nb distribution without interaction (total=counting variable)##
glm6 <- glmmTMB(Total ~ Zone + DBH + Height +(1 | Host), family = nbinom2, data = data)
summary(glm6)
# 7 Random factor. With intercept y pendiente aleatorios + interactions
glm7 <- glmmTMB (Total ~ Zone + DBH + Height + Zone:DBH + Zone:Height + DBH:Height + (1 + DBH | Host), family = nbinom2, data = data)
summary(glm7)
#8 Random factor. With random intercept and interactions
glm8 <- glmmTMB (Total ~ Zone + DBH + Height + Zone:DBH + Zone:Height + DBH:Height + (1|Host), family = nbinom2, data = data)
summary(glm8)
#9 Without Random factor. Without interactions
glm9 <- glmmTMB (Total ~ Zone + DBH + Height, family= nbinom2, data=data)
summary(glm9)
#10 Without Random factor. With interactions
glm10 <- glmmTMB (Total ~ Zone + DBH + Height + Zone*DBH + Zone*Height + DBH*Height, family=nbinom2, data=data)
summary(glm10)

##Chose a best model##

# Calcular AIC y BIC para todos los modelos
AIC_values <- AIC(glm1, glm2, glm3, glm4, glm5, glm6, glm7, glm8, glm9, glm10)
BIC_values <- BIC(glm1, glm2, glm3, glm4, glm5, glm6, glm7, glm8, glm9, glm10)
# Imprimir los valores de AIC y BIC
print(AIC_values) #The besr Model is glm8
print(BIC_values) #The Best Model is glm6
##I will use glm6: Random factor with nb distribution without interaction to help the biological interpretations.##
summary(glm6)
#Prueba de comparación múltiple PARA SABER ENTRE QUE FACTORS HAY DIFERENCIAS
library(lsmeans)
library(multcomp)
Comp <- lsmeans(glm6, ~Zone)
pairs(Comp, adj="bon") #Fishser con correcion de bonferroni
cld(Comp, alpha=0.05, adj="bon", Letters=letters)
#Zona 4 diferentes a todas

#Grafico de efectos parciales

effects2 <- ggpredict(glm6, terms = c("DBH", "Zone"))
plot(effects2) + labs(x = "DBH (cm)", y = "Abundance") +
  theme(axis.line = element_line(color = "black"), 
        axis.text = element_text(color = "black", size = 18),
        axis.title = element_text(size = 18, color = "black"),
        panel.grid.minor = element_blank(),
        plot.title = element_blank())

####MULTIVARIATE REGRESSION TREES #######
#Here, I am trying to created grups of zones basadas e la composicion de especies.#
#The goal es identificar grupos homogeneos de "zonas" en terminos de composicion de especies y despues reconocer si esos grupos son o no diferentes en terminos de las caracteristicas del hospedero#
library(partykit)
data$Zone <- factor(data$Zone)


#a simple basic fitting fuction (of type 1) for a logistic refresion
logit <- function(y, x, start = NULL, weights = NULL, offset = NULL, ...) {
  glm(y ~ 0 + x, family = poisson, start = start, ...)
}
modelo_arbol <- mob (Total ~ Zone | DBH + Height + 
                       Philodendron1 + Philodendroncanicaule + Philodendronhebetatum + Philodendron6 + Philodendron4 + Philodendron8 + Chlorospathanicolsonii + Stenospermationlongifolium + Stenospermationwallisii + Stenospermationflavum + Anthuriumlicium + Anthuriumscandens + Anthuriumrecavum + Anthuriumricaurtense + Anthuriumdavidsoniae + Anthuriumjesusii + Anthuriumpunctatum + Anthuriumsubcarinatum + Anthuriumcaucana + Anthuriumspnov + Anthuriumpulchellum + Anthurium1 + Anthurium2 + Schefflera1 + Schefflera2 + Schefflera3 + Scheffleraternata + As1 + As2 + As4 + Begoniaantioquensis + Mezobromeliabicolor + Guzmaniacoriostachya + Guzmaniamitis + Guzmaniapennellii + Guzmaniapearcei + Guzmaniakalbreyeri + Racinaeasubalata + Racinaeaspiculosa + Racinaeapenlandii + Pitcairnia1 + Pitcairniamucida + Burmanniakalbreyeri + Burmeistera + Clusia1 + 
                       Clusia2 + Clusia3 + Clusia4 + Clusia5 + Sphaeradeniascandens + Cy2 + Sphaeradeniadanielii + Cavendishiamicayensis + Cavendishiaerythrostegia + Cavendishialeucantha + Disterigmacryptocalix + Disterigmachocoanum + Disterigma1 + Disterigma2 + Disterigma3 + Psammisiaglandulolaminata + Psammisiaspnov + Psammisia1 + Psammisia2 + Sphyrospermumdissimile + Sphyrospermum1 + Sphyrospermumbuxifolium + E1 + E2 + E3 + E4 + E5 + E6 + E7 + E8 + E9 + Columneadimidiata + Columneaferruginea + Columnea1 + Glossoloma1 + Columneacetipes + Kohleriaaffinis + Drymoniateuscheri + Drymonia1 + Drymoniaspnov + Trichodrymoniametamorphophylla 
                     + Mal1 + Ma1 + Ma2 + Ma3 + Miconialoreyoides + Clidemiaepibaterium + Miconiaasperrima + Miconiaurticoides + Miconiagoniostigma + Blakeaanisophylla + Blakeasubbarbata + Blakeapyxidanthus + Blakeabrachyura + M1 + M2 + M3 + M4 + M5 + M6 + M7 + M8 + My1 + Aspidogyne + Camaridium + Cryptocentrum + Cyrtochilumflexuosum + Dichaea + Elleanthus1 + Elleanthuslancifolius + Epidendrum1 + Epidendrumsilvertonei + Lepanthessilverstonei + Lepanthesauriculata + Lepanthesspnov + Lepanthesyubarta + Lephantesectopa + Maxillariacaucana + Maxillarialongissima + O27 + O36 + Ornithidiumaureum + Ornithidiumfulgens + Otoglossumcoronarium + Platystelepyriformis + Pleurothallis1 + Pleurothallis2 + Pleurothallis3 + Pleurothallis4 + Pleurothalliscordata + Pleurothallisflavomarginata + Scaphosepalumodontochilum + Scaphosepalumswertiifolium + 
                       Scaphyglottispunctulata + 
                       Stelis6 + Stelis1 + Stelis2 + Stelis3 + Stelis4 + Stelis5 + Phy1 + Peperomiastriata + Peperomiaheterophylla + 
                       Peperomiaswartziana + Peperomia3 + Peperomiaeburnea + Piperechinocaule + Piperentradense + Piper1 + Piper2 + Notopleurapithecobia + Schraderaacuminata + R1 + R2 + R3 + R4 + Solanumevolvulifolium + Pileatrichosanthes + Pileaflexuosa + U1 + Aspleniumauriculatum + Aspleniumcladolepton + Lomariaensiformis + Blechnumschomburgkii + Blechnumfragile + Polybotryaosmundacea + Polybotrya1 + Polybotrya2 + Polybotrya3 + Ctenitis + Serpocaulonlevigatum + Peclumadivaricata + Serpocaulonfraxinifolium + Stenogrammitismyosuroides + Alansmiasenilis + Peclumacamptophyllaria + Serpocaulonloriceum + Grammitis + Terpsichore1 + Terpsichore2 + Terpsichore3 + 
                       Radiovittariamoritziana, data = data, fit = logit)
print (modelo_arbol, node=2)
print (modelo_arbol, node=3)
plot(modelo_arbol)
summary(modelo_arbol, node = 1)
summary(modelo_arbol, node = 2)
summary(modelo_arbol, node = 3)

#Se pueden establecer dos subgrupos: hospederos con DAP <= a 21.65 y > a 21.65. 

####Analisis de composicion de especies by NMDS Separado por los nodos del MRT
# Crear dos dataframes vacíos
df_menor_2165 <- data.frame()
df_mayor_2165 <- data.frame()

# Filtrar por valores de DBH
df_menor_2165 <- subset(data, DBH <= 21.65)
df_mayor_2165 <- subset(data, DBH > 21.65)

#NMDS with Bray-Curtis distancias . To look composition diferences between the zones.
# Seleccionar las columnas relevantes
columnas_relevantes <- c("Zone", names(df_menor_2165)[6:ncol(df_menor_2165)])

# Crear dataframes con las columnas relevantes
df_menor_2165_relevantes <- df_menor_2165[, columnas_relevantes]
df_mayor_2165_relevantes <- df_mayor_2165[, columnas_relevantes]
write.xlsx(df_menor_2165_relevantes, file="NMDS_Menor_2165.xlsx", colnames=TRUE)
write.xlsx(df_mayor_2165_relevantes, file="NMDS_Mayor_2165.xlsx", colnames=TRUE)

