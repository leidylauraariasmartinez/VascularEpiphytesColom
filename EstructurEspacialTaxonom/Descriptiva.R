# Load necessary packages
library(openxlsx)
library(ggplot2)

##Descripcion Por Zonas. Familias más abundantes
#########################################################
##########################################################
FamiliasxZona <- read.xlsx("ZonaXFamilAbundan.xlsx", rowNames = FALSE)


#Una sola grafica con las familias como variable categorica.
ggplot(FamiliasxZona, aes(x=Forofito, y=Abundancia, fill=Familia ,ymax = 19+1)) + 
  geom_boxplot() + xlab("Zone") + ylab("Individuals")+
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
  )

#Si quiero graficar unitariamente por familias:DEBO CARGAR MATRIZ MUEVA: X=ZONAS. Y1=ABUNDANCIA DE FAMILIA1, Y2=ABUNDANCIA DE FAMILIAS2

FamiliasxZona <- read.xlsx("ZonaFamiliAbunda.xlsx", rowNames = FALSE)


ggplot(FamiliasxZona, aes(x=as.factor(Forofito), y=Araceae)) + 
  geom_boxplot()+ xlab("Zone") + ylab("Individuals")+
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
  )


ggplot(FamiliasxZona, aes(x=as.factor(Forofito), y=Bromeliaceae)) + 
  geom_boxplot()+ xlab("Zone") + ylab("Individuals")+
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
  )



ggplot(FamiliasxZona, aes(x=as.factor(Forofito), y=Ericaceae)) + 
  geom_boxplot()+ xlab("Zone") + ylab("Individuals")+
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
  )



ggplot(FamiliasxZona, aes(x=as.factor(Forofito), y=Orchidaceae)) + 
  geom_boxplot()+ xlab("Zone") + ylab("Individuals")+
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
  )



ggplot(FamiliasxZona, aes(x=as.factor(Forofito), y=Helechos)) + 
  geom_boxplot()+ xlab("Zone") + ylab("Individuals")+
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
  )

########Abundancia de Familias mas abundantes por Categoria de forofito##
#######################################################################
#####################################################################

#si queiero graficar esto, debo cargar matriz nueva con familias como variable categorica (25-01-2022)

FamiliasxCategoria <- read.xlsx("CategoriaXFamilAbundan.xlsx", rowNames = FALSE)

ggplot(FamiliasxCategoria, aes(x=as.factor(Categoria), y=Araceae)) + 
  geom_boxplot() + 
  xlab("Category of DAP") +ylab("Individuals") +
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
  )

ggplot(FamiliasxCategoria, aes(x=as.factor(Categoria), y=Bromeliaceae)) + 
  geom_boxplot() + 
  xlab("Category of DAP") +ylab("Individuals") +
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
  )

ggplot(FamiliasxCategoria, aes(x=as.factor(Categoria), y=Ericaceae)) + 
  geom_boxplot() + 
  xlab("Category of DAP") +ylab("Individuals") +
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
  )

ggplot(FamiliasxCategoria, aes(x=as.factor(Categoria), y=Orchidaceae)) + 
  geom_boxplot() + 
  xlab("Category of DAP") +ylab("Individuals") +
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
  )

ggplot(FamiliasxCategoria, aes(x=as.factor(Categoria), y=Urticaceae)) + 
  geom_boxplot() + 
  xlab("Category of DAP") +ylab("Individuals") +
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
  )

ggplot(FamiliasxCategoria, aes(x=as.factor(Categoria), y=Helechos)) + 
  geom_boxplot() + 
  xlab("Category of DAP") +ylab("Individuals") +
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
  )


##############ENSAMBLAJE: REPRESENTACION FAMILIAS X ZONAS################
#########################################################################

##PorzonasPorFamilias

ZonasFamiResu <- read.xlsx("ZonaFamiliaCort.xlsx",rowNames = TRUE)
data <- as.matrix(ZonasFamiResu) #Transformar en matriz numerica
DATA<- t(data) #tRANSPONER MATRIX, porque corta las familias horizontalmente
# Default Heatmap
heatmap(DATA)
# Use 'scale' to normalize
heatmap(DATA, scale="column") 
# No dendrogram nor reordering for neither column or row
heatmap(DATA, Colv = NA, Rowv = NA, scale="column") #Como no ordena por similaridad las variables, el analisis de la imagen sed complica un poquin.


#CategoriasdearbolesporFamilias
CategorFami <- read.xlsx("CategoriasPorFamilia.xlsx",rowNames = TRUE)
data1 <- as.matrix(CategorFami) #Transformar en matriz numerica
DATA1<- t(data1) #tRANSPONER MATRIX, porque corta las familias horizontalmente
# Default Heatmap
heatmap(DATA1)
# Use 'scale' to normalize
heatmap(DATA1, scale="column")
# No dendrogram nor reordering for neither column or row
heatmap(DATA1, Colv = NA, Rowv = NA, scale="column") #Como no ordena por similaridad las variables, el analisis de la imagen sed complica un poquin.




###########INDICES INDICADORES DE ESPECIES##########
####♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥Kim Nam-joon 김남준; 金南俊國♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥##
####♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥Kim Nam-joon 김남준; 金南俊國♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥##
####♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥Kim Nam-joon 김남준; 金南俊國♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥##
####♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥Kim Nam-joon 김남준; 金南俊國♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥##

############Indices por Familias X Zonas

AbundanciaZonasFamilia <-  read.xlsx("ZonaPorFamilia.xlsx", rowNames = FALSE) # This is our community data #bUSCRA ARCHIVO ENEstruuraeps
AbundanciaZonasFamilia 
Abundancias2<- AbundanciaZonasFamilia[,3:28]
GupoZonas<- AbundanciaZonasFamilia$Zonas
inv = multipatt(Abundancias2, GupoZonas, func = "r.g", control = how(nperm=9999))
summary(inv)


####COMO EL COEFICIENTE DE FIDELIDAD DE ESPECIES SE CALCULA DESDE PRESENCIA AUSENCIA SE HIZO EL ANALISIS NUEVAMNET.
#First, we need to transform the species composition data into presences absence (Φ coefficient is calculated on presence-absence data), 
#and then apply the function multipatt (abbreviation for Multi-level pattern analysis) to calculate fidelity. 
#We set the argument fun = 'r.g', where r indicates correlation coefficient (Φ coefficient is closely related to Pearson's r correlation) and g stands for standardizing clusters to the same size (“group size”).

PresenciaZonasFamilia <-  read.xlsx("ZonaPorPresenciadeFamilia.xlsx", rowNames = FALSE)
PresenciaZonasFamilia
Abundancias2<- PresenciaZonasFamilia[,3:28]
GrupoZonas<- PresenciaZonasFamilia$Zona
inv = multipatt(Abundancias2, GrupoZonas, func = "r.g", control = how(nperm=9999))
summary(inv)

################Indices por Especies x Zonas##########

presenespeci <- read.xlsx("Prese-AuseEspeciZona.xlsx", rowNames = FALSE)
presenespeci <- presenespeci[,2:190]

fidelidad = multipatt(presenespeci[,2:189], presenespeci$Zona, func = "r.g", control = how(nperm=9999))
summary(fidelidad)

###########Indices Por familia X DAPcategoria############
PresenciaDAPFamilia <-  read.xlsx("FamiliaPresen-Ause.DAP.xlsx", rowNames = FALSE)

presen<- PresenciaDAPFamilia[,2:27]
GrupoCategoria<- PresenciaDAPFamilia$Categoria
inv = multipatt(presen, GrupoCategoria, func = "r.g", control = how(nperm=9999))
summary(inv)


##Indices por Especeis X DAP####
DAPesp <-  read.xlsx("AbundEspec.DAP.xlsx", rowNames = FALSE)
#Convertir en data.frame de presnecia ausencia
presesp <- presesp[,3:190]
presesp[presesp > 1] <- 1
presesp
#Listo ;), Seguimos con la estimacion de indice.Crear vector con categorias
catego <-DAPesp$Categoria
inv = multipatt(presesp, catego, func = "r.g", control = how(nperm=9999))
summary(inv)


#####DESCRIPCION DE LAS ABUNDANCIAS POR LAS ZONAS POR ALTURA Y DAP DE HOSPEDEROS##########
##########################################################################################
###########################################################################################

# Load necessary packages
library(openxlsx)
library(ggplot2)

#Load Data set
df <- read.xlsx("HospederoZonasAbundancias.xlsx", rowNames = FALSE)

#Separar datos por Variable de forofito
Altura <-df[,1:3]

ggplot(Altura, aes(x=Zona, y=Abundancia, fill=Altura)) + 
  geom_boxplot()+ xlab("Zona") + ylab("Individuos")+
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent')
  )

DAP<- df[,4:6]
ggplot(DAP, aes(x=Zona, y=Abundancia, fill=DAP)) + 
  geom_boxplot()+ xlab("Zone") + ylab("Individuals")+
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent')
  )
