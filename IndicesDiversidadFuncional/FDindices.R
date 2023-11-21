#Cargar paquetes 
library(FD) #Solo estiamcion de indices
library(openxlsx)
library(mFD) #VI¿isualizacion y estimacion de FD indices


####DATOS POR MUESTRA.PARA REGRESIONES########
#Tomando como unidad de muestreo cada zona en cada hospedero (35 Z1's, 35z2's....)

###CargarDatos: Matriz de abundancia y otra de los Rasgos (=orden de las spp e = ortografia)
Rasgos <- read.xlsx("Ragos.xlsx", rowNames = TRUE)
Abundancias <- read.xlsx("Abundancia.xlsx", rowNames = TRUE)
summary(Rasgos) #Para revisar como esta tomando las variables

#Convertir los rasgos  a  variables tipo factor
Rasgos$Forma.de.vida <- as.factor(Rasgos$Forma.de.vida)
Rasgos$Clonalidad <- as.factor(Rasgos$Clonalidad)
Rasgos$Espinescencia <- as.factor(Rasgos$Espinescencia)
Rasgos$`Raices/Estrategia.de.toma.de.nurtientes`<- as.factor(Rasgos$`Raices/Estrategia.de.toma.de.nurtientes`)
Rasgos$Sindrome.de.dispersion <- as.factor(Rasgos$Sindrome.de.dispersion)
Rasgos$Metabolismo <- as.factor(Rasgos$Metabolismo)
summary(Rasgos)


#Como mis datos son todos categoricos: La matriz de disimilitud debe ser dada por el metodo de Gower, usando "gowdis".
disimilaridad <- gowdis(Rasgos)


############고통 없는 삶은 의미가 없습니다 #############################################################

###########################################################################
####VISUALIZAR DIVERSIDAD FUNCIONAL EN UN ESPACIO MULTIDIMENSIONAL#########
###########################################################################

##tuorial completo: https://cmlmagneville.github.io/mFD/articles/mFD_general_workflow.html

#Input necesarios:
Rasgos
AbundanciasZonas <- read.xlsx("Abundanciax4zonas.xlsx", rowNames = TRUE)
TipoVariable <- read.xlsx("TipodeVariables.xlsx", rowNames = FALSE) #Ver la clave en las especificaciones del paquete mFD. N=nominal. 

# Species traits summary:
traits_summ <- mFD::sp.tr.summary(
  tr_cat     = TipoVariable,   
  sp_tr      = Rasgos, 
  stop_if_NA = FALSE)

traits_summ #revisar qeu todo esta Ok


#Computing distances between species based on functional traits. 
#traits-based distances between species in order to build the functional space in which indices will be computed
#My metric is gower... again, it is for the data nature

sp_dist <- mFD::funct.dist(
  sp_tr         = Rasgos,
  tr_cat        = TipoVariable,
  metric        = "gower",
  scale_euclid  = "scale_center",
  weight_type   = "equal",
  stop_if_NA    = FALSE)

#MEJOR USAR ENTIDADES FE porqeu nuchos ceros.
##(FE), i.e groups of species with same trait values when species are described with categorical and/or ordinal traits.|
##How to Deal With Functional Entities
##https://cmlmagneville.github.io/mFD/articles/How_to_deal_with_Functional_Entities.html
#EN MI CASO NO LO HICE PORQUE QUERIA TENER UNIFORMIDAD ENTRE MIS RESULTADOS TAXONOMICOS Y FUNCIONALES

#Computing functional spaces & their quality
#Compute multimensional functional spaces and assess their quality
#PCoA using the trait-based distances (and if required a functional dendrogram)

fspaces_quality <- mFD::quality.fspaces(
  sp_dist             = sp_dist,
  maxdim_pcoa         = 10,
  deviation_weighting = "absolute",
  fdist_scaling       = FALSE,
  fdendro             = "average")

fspaces_quality #Parametros del PCoA

#El mejor espacio es el 3d. The space with the best quality has the lowest quality metric

#Illustrating the quality of the selected functional spaces
  mFD::quality.fspaces.plot(
    fspaces_quality            = fspaces_quality,
    quality_metric             = "mad",
    fspaces_plot               = c("tree_average", "pcoa_2d", "pcoa_3d", 
                                   "pcoa_4d", "pcoa_5d", "pcoa_6d"),
    name_file                  = "prueba", #Así queda guardado el archivo en la carpeta de trabajo
    range_dist                 = NULL,
    range_dev                  = NULL,
    range_qdev                 = NULL,
    gradient_deviation         = c(neg = "darkblue", nul = "grey80", pos = "darkred"),
    gradient_deviation_quality = c(low = "yellow", high = "red"),
    x_lab                      = "Trait-based distance")
  
#Se debe revisar si efectivamente la 3d es la mas adecuada

##Test correlation between functional axes and traits
##mFD allows to test for correlations between traits and functional axes and then illustrate possible correlations

#crear una matriz  con las dimesiones
sp_faxes_coord<- fspaces_quality$"details_fspaces"$"sp_pc_coord"
  
##Plotear
fruits_tr_faxes <- mFD::traits.faxes.cor(
  sp_tr          = Rasgos, 
  sp_faxes_coord = sp_faxes_coord[ , c("PC1", "PC2", "PC3", "PC4")], 
  plot           = TRUE,
  stop_if_NA = FALSE)

#We can print only traits with significant effect on position along one of the axis and look at the plots
fruits_tr_faxes$"tr_faxes_stat"[which(fruits_tr_faxes$"tr_faxes_stat"$"p.value" < 0.05), ]
# Return plots:
fruits_tr_faxes$"tr_faxes_plot"


######윤기야 나랑 결혼해줘 #######
####Plot functional space#########
##################################
# craer matriz con las coordenadas de las especeis .sp_faxes_coord is a matrix of species coordinates
sp_faxes_coord_fruits <- fspaces_quality$"details_fspaces"$"sp_pc_coord"

#Antes de observar las diferencias entre las zonas... ver como es el ensamblaje geenral:
big_plot <- mFD::funct.space.plot(
  sp_faxes_coord  = sp_faxes_coord_fruits[ , c("PC1", "PC2", "PC3" )],
  faxes           = c("PC1", "PC2", "PC3"),
  name_file       = "bigplot",
  faxes_nm        = NULL, #nombresdelosejes
  range_faxes     = c(NA, NA), #si quiero acotar ejes
  color_bg        = "grey95", #colordelfondo
  color_pool      = "darkgreen",
  fill_pool       = "white",
  shape_pool      = 21,
  size_pool       = 1, 
  plot_ch         = TRUE, #dibujar o no el casco convexo 2D llenado por el grupo global de especies. El color, el relleno y la opacidad del casco convexo se pueden elegir a través de otras entradas, consulte la ayuda de la función
  color_ch        = "black",
  fill_ch         = "white",
  alpha_ch        = 0.5,
  plot_vertices   = TRUE, #destacar spp de los vertices del casco 
  color_vert      = "blueviolet",
  fill_vert       = "blueviolet",
  shape_vert      = 23,
  size_vert       = 1,
  plot_sp_nm      = NULL, #is a vector containing species names to plot. If NULL, no species names plotted
  nm_size         = 3,
  nm_color        = "black",
  nm_fontface     = "plain",
  check_input     = TRUE)

#####                                                  ####
##### Compute functional diversity indices & plot them######
#####                                                 ######

AbundanciasZonasMatrix <- data.matrix(AbundanciasZonas) # como mis abundancias son class: data.frame, tengo que convertirlas en claass=matrix.

alpha_fd_indices_fruits <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord_fruits[ , c("PC1", "PC2", "PC3")],
  asb_sp_w         = AbundanciasZonasMatrix,
  ind_vect         = c("fdis", "feve", "fric", "fdiv"),
  scaling          = TRUE, #escalarFric [0-1]
  check_input      = TRUE,
  details_returned = TRUE) #guardar info para plotear despues

alpha_fd_indices_fruits$functional_diversity_indices #Indices por zonas (finalmente.) 

# you can plot functional indices using

plots_alpha <- mFD::alpha.multidim.plot(
  output_alpha_fd_multidim = alpha_fd_indices_fruits,
  plot_asb_nm              = c("Z1", "Z2"), #las que quiero comparar
  ind_nm                   = c("fdis", "feve", "fric", "fdiv"), #indices que quiero
  faxes                    = NULL,
  faxes_nm                 = NULL,
  range_faxes              = c(NA, NA),
  color_bg                 = "grey95",
  shape_sp                 = c(pool = 3, asb1 = 21, asb2 = 21),
  size_sp                  = c(pool = 0.7, asb1 = 1, asb2 = 1),
  color_sp                 = c(pool = "grey50", asb1 = "#1F968BFF", asb2 = "#DCE319FF"),
  color_vert               = c(pool = "grey50", asb1 = "#1F968BFF", asb2 = "#DCE319FF"),
  fill_sp                  = c(pool = NA, asb1 = "#1F968BFF", asb2 = "#DCE319FF"),
  fill_vert                = c(pool = NA, asb1 = "#1F968BFF", asb2 = "#DCE319FF"),
  color_ch                 = c(pool = NA, asb1 = "#1F968BFF", asb2 = "#DCE319FF"),
  fill_ch                  = c(pool = "white", asb1 = "#1F968BFF", asb2 = "#DCE319FF"),
  alpha_ch                 = c(pool = 1, asb1 = 0.3, asb2 = 0.3),
  shape_centroid_fdis      = c(asb1 = 22,  asb2 = 24),
  shape_centroid_fdiv      = c(asb1 = 22,  asb2 = 24),
  size_sp_nm               = 3, 
  color_sp_nm              = "black",
  plot_sp_nm               = NULL,
  fontface_sp_nm           = "plain",
  save_file                = TRUE, #para guardar las imagenes directamente en la carpeta de trabajo. Le pone el nombre que se le da la gana ¬¬, pero tiene un buen sistema 
  check_input              = TRUE) 

#FRic representation: the colored shapes reflect the convex-hull of the studied assemblages and the white shape reflects the convex-hull of the global pool of species:
#FDiv representation: the gravity centers of vertices (i.e. species with the most extreme functional traits) of each assemblages are plotted as a square and a triangle. The two colored circles represent the mean distance of species to the gravity center for each assemblage. Species of each assemblage have different size given their relative weight into the assemblage
#FEve representation: colored traits represent the Minimum Spanning Tree linking species of each assemblage. Species of each assemblage have different size given their relative weight into the assemblage.

plots_alpha <- mFD::alpha.multidim.plot(
  output_alpha_fd_multidim = alpha_fd_indices_fruits,
  plot_asb_nm              = c("Z1", "Z3"), #las que quiero comparar
  ind_nm                   = c("fdis", "feve", "fric", "fdiv"), #indices que quiero
  faxes                    = NULL,
  faxes_nm                 = NULL,
  range_faxes              = c(NA, NA),
  color_bg                 = "grey95",
  shape_sp                 = c(pool = 3, asb1 = 21, asb2 = 21),
  size_sp                  = c(pool = 0.7, asb1 = 1, asb2 = 1),
  color_sp                 = c(pool = "grey50", asb1 = "#1F968BFF", asb2 = "#DCE319FF"),
  color_vert               = c(pool = "grey50", asb1 = "#1F968BFF", asb2 = "#DCE319FF"),
  fill_sp                  = c(pool = NA, asb1 = "#1F968BFF", asb2 = "#DCE319FF"),
  fill_vert                = c(pool = NA, asb1 = "#1F968BFF", asb2 = "#DCE319FF"),
  color_ch                 = c(pool = NA, asb1 = "#1F968BFF", asb2 = "#DCE319FF"),
  fill_ch                  = c(pool = "white", asb1 = "#1F968BFF", asb2 = "#DCE319FF"),
  alpha_ch                 = c(pool = 1, asb1 = 0.3, asb2 = 0.3),
  shape_centroid_fdis      = c(asb1 = 22,  asb2 = 24),
  shape_centroid_fdiv      = c(asb1 = 22,  asb2 = 24),
  size_sp_nm               = 3, 
  color_sp_nm              = "black",
  plot_sp_nm               = NULL,
  fontface_sp_nm           = "plain",
  save_file                = TRUE, #para guardar las imagenes directamente en la carpeta de trabajo. Le pone el nombre que se le da la gana ¬¬, pero tiene un buen sistema 
  check_input              = TRUE)


plots_alpha <- mFD::alpha.multidim.plot(
  output_alpha_fd_multidim = alpha_fd_indices_fruits,
  plot_asb_nm              = c("Z1", "Z4"), #las que quiero comparar
  ind_nm                   = c("fdis", "feve", "fric", "fdiv"), #indices que quiero
  faxes                    = NULL,
  faxes_nm                 = NULL,
  range_faxes              = c(NA, NA),
  color_bg                 = "grey95",
  shape_sp                 = c(pool = 3, asb1 = 21, asb2 = 21),
  size_sp                  = c(pool = 0.7, asb1 = 1, asb2 = 1),
  color_sp                 = c(pool = "grey50", asb1 = "#1F968BFF", asb2 = "#DCE319FF"),
  color_vert               = c(pool = "grey50", asb1 = "#1F968BFF", asb2 = "#DCE319FF"),
  fill_sp                  = c(pool = NA, asb1 = "#1F968BFF", asb2 = "#DCE319FF"),
  fill_vert                = c(pool = NA, asb1 = "#1F968BFF", asb2 = "#DCE319FF"),
  color_ch                 = c(pool = NA, asb1 = "#1F968BFF", asb2 = "#DCE319FF"),
  fill_ch                  = c(pool = "white", asb1 = "#1F968BFF", asb2 = "#DCE319FF"),
  alpha_ch                 = c(pool = 1, asb1 = 0.3, asb2 = 0.3),
  shape_centroid_fdis      = c(asb1 = 22,  asb2 = 24),
  shape_centroid_fdiv      = c(asb1 = 22,  asb2 = 24),
  size_sp_nm               = 3, 
  color_sp_nm              = "black",
  plot_sp_nm               = NULL,
  fontface_sp_nm           = "plain",
  save_file                = TRUE, #para guardar las imagenes directamente en la carpeta de trabajo. Le pone el nombre que se le da la gana ¬¬, pero tiene un buen sistema 
  check_input              = TRUE)


plots_alpha <- mFD::alpha.multidim.plot(
  output_alpha_fd_multidim = alpha_fd_indices_fruits,
  plot_asb_nm              = c("Z2", "Z3"), #las que quiero comparar
  ind_nm                   = c("fdis", "feve", "fric", "fdiv"), #indices que quiero
  faxes                    = NULL,
  faxes_nm                 = NULL,
  range_faxes              = c(NA, NA),
  color_bg                 = "grey95",
  shape_sp                 = c(pool = 3, asb1 = 21, asb2 = 21),
  size_sp                  = c(pool = 0.7, asb1 = 1, asb2 = 1),
  color_sp                 = c(pool = "grey50", asb1 = "#1F968BFF", asb2 = "#DCE319FF"),
  color_vert               = c(pool = "grey50", asb1 = "#1F968BFF", asb2 = "#DCE319FF"),
  fill_sp                  = c(pool = NA, asb1 = "#1F968BFF", asb2 = "#DCE319FF"),
  fill_vert                = c(pool = NA, asb1 = "#1F968BFF", asb2 = "#DCE319FF"),
  color_ch                 = c(pool = NA, asb1 = "#1F968BFF", asb2 = "#DCE319FF"),
  fill_ch                  = c(pool = "white", asb1 = "#1F968BFF", asb2 = "#DCE319FF"),
  alpha_ch                 = c(pool = 1, asb1 = 0.3, asb2 = 0.3),
  shape_centroid_fdis      = c(asb1 = 22,  asb2 = 24),
  shape_centroid_fdiv      = c(asb1 = 22,  asb2 = 24),
  size_sp_nm               = 3, 
  color_sp_nm              = "black",
  plot_sp_nm               = NULL,
  fontface_sp_nm           = "plain",
  save_file                = TRUE, #para guardar las imagenes directamente en la carpeta de trabajo. Le pone el nombre que se le da la gana ¬¬, pero tiene un buen sistema 
  check_input              = TRUE)


plots_alpha <- mFD::alpha.multidim.plot(
  output_alpha_fd_multidim = alpha_fd_indices_fruits,
  plot_asb_nm              = c("Z3", "Z4"), #las que quiero comparar
  ind_nm                   = c("fdis", "feve", "fric", "fdiv"), #indices que quiero
  faxes                    = NULL,
  faxes_nm                 = NULL,
  range_faxes              = c(NA, NA),
  color_bg                 = "grey95",
  shape_sp                 = c(pool = 3, asb1 = 21, asb2 = 21),
  size_sp                  = c(pool = 0.7, asb1 = 1, asb2 = 1),
  color_sp                 = c(pool = "grey50", asb1 = "#1F968BFF", asb2 = "#DCE319FF"),
  color_vert               = c(pool = "grey50", asb1 = "#1F968BFF", asb2 = "#DCE319FF"),
  fill_sp                  = c(pool = NA, asb1 = "#1F968BFF", asb2 = "#DCE319FF"),
  fill_vert                = c(pool = NA, asb1 = "#1F968BFF", asb2 = "#DCE319FF"),
  color_ch                 = c(pool = NA, asb1 = "#1F968BFF", asb2 = "#DCE319FF"),
  fill_ch                  = c(pool = "white", asb1 = "#1F968BFF", asb2 = "#DCE319FF"),
  alpha_ch                 = c(pool = 1, asb1 = 0.3, asb2 = 0.3),
  shape_centroid_fdis      = c(asb1 = 22,  asb2 = 24),
  shape_centroid_fdiv      = c(asb1 = 22,  asb2 = 24),
  size_sp_nm               = 3, 
  color_sp_nm              = "black",
  plot_sp_nm               = NULL,
  fontface_sp_nm           = "plain",
  save_file                = TRUE, #para guardar las imagenes directamente en la carpeta de trabajo. Le pone el nombre que se le da la gana ¬¬, pero tiene un buen sistema 
  check_input              = TRUE)




