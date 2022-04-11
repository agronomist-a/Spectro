file.choose()
ruta_1 <- "/media/antonio/TESIS/29_MARZO_ROT ISC/CSV/ISR.csv"
ruta_2 <- "/media/antonio/TESIS/29_MARZO_ROT ISC/CSV/ISS.csv"
ruta_3 <- "/media/antonio/TESIS/29_MARZO_ROT ISC/CSV/RR.csv"
ruta_4 <- "/media/antonio/TESIS/29_MARZO_ROT ISC/CSV/RS.csv"

ISR <- read.csv(ruta_1, header = TRUE, sep = ";")
ISS <- read.csv(ruta_2, header = TRUE, sep = ";")
RR <- read.csv(ruta_3, header = TRUE, sep = ";")
RS <- read.csv(ruta_4, header = TRUE, sep = ";")
library(tidyverse)

#Se define la funcion para separar y fragmentar
Spectroan <- function(Raw_data) {
  
  colnames(Raw_data) <- c("id","consec", "lg", "ref")
  
  #Rango de las bandas
  AzulRW1 <- seq(451, 496, by = 1)
  VerdeRW1 <- seq(496, 571, by = 1)
  RojoRW1 <- seq(621,701, by = 1)
  RED_EDGERW1 <- seq(700,751,by = 1)
  NIRRW1 <- seq(751,901, by = 1)
  
  #cortes lg
  
  cutAzulRW1 <- cut(Raw_data$lg, breaks = AzulRW1)     
  cutVerdeRW1 <- cut(Raw_data$lg, breaks = VerdeRW1)
  cutRojoRW1 <- cut(Raw_data$lg, breaks = RojoRW1)
  cutRED_EDGERW1 <- cut(Raw_data$lg, breaks = RED_EDGERW1)
  cutNIRRW1 <- cut(Raw_data$lg, breaks = NIRRW1)
  
  #segmentacion
  
  list_azulRW1 <- tapply(Raw_data$ref, cutAzulRW1, as.data.frame)
  list_verdeRW1 <- tapply(Raw_data$ref, cutVerdeRW1, as.data.frame)
  list_rojoRW1 <- tapply(Raw_data$ref, cutRojoRW1, as.data.frame)
  list_red_edgeRW1 <-tapply(Raw_data$ref, cutRED_EDGERW1, as.data.frame)
  list_NIRRW1 <- tapply(Raw_data$ref, cutNIRRW1, as.data.frame)
  
  df_azulRW1 <- as.data.frame(list_azulRW1)
  df_verdeRW1 <- as.data.frame(list_verdeRW1)
  df_rojoRW1 <- as.data.frame(list_rojoRW1)
  df_red_edgeRW1 <- as.data.frame(list_red_edgeRW1)
  df_NIRRW1 <- as.data.frame(list_NIRRW1)
  
  #Conversion de filas a columnas, se indica de la primera a la ?ltima fila y renombrar columnas
  long_azulRW1 <- pivot_longer(df_azulRW1,
                               cols =  X.451.452.:X.495.496.,
                               names_to = "nm",
                               values_to = "Reflactance") 
  
  #Creciopn de las nuevas columnas
  etiquetas_azulRW1 <- c(rep(seq(451,495,1), each = 20, times = 1))
  repeticiones_azulRW1 <- c(rep(seq(1,20,1), each = 1, times = 45))
  
  #ordenamiento de las columnas segun los nanometros
  dff_azulRW1 <- long_azulRW1[with(long_azulRW1, order(long_azulRW1$nm)), ]
  
  dff_azulRW1$nm <- etiquetas_azulRW1
  dff_azulRW1$repeticiones <- repeticiones_azulRW1 
  Banda <- "Azul"
  Azul <- cbind(Banda, dff_azulRW1)
  
  #Iteracion del c?digo para las 4 longitudes de onda restantes
  
  #VERDERW1
  
  long_verdeRW1 <- pivot_longer(df_verdeRW1,
                                cols =   X.496.497.:X.570.571.,
                                names_to = "nm",
                                values_to = "Reflactance") 
  
  etiquetas_verdeRW1 <- c(rep(seq(496,570,1), each = 20, times = 1))
  repeticiones_verdeRW1 <- c(rep(seq(1,20,1), each = 1, times = 75))
  
  dff_verdeRW1 <- long_verdeRW1[with(long_verdeRW1, order(long_verdeRW1$nm)), ]
  
  dff_verdeRW1$nm <- etiquetas_verdeRW1
  dff_verdeRW1$repeticiones <- repeticiones_verdeRW1 
  Banda <- "Verde"
  Verde <- cbind(Banda, dff_verdeRW1)
  
  #ROJORW1
  
  long_rojoRW1 <- pivot_longer(df_rojoRW1,
                               cols =   X.621.622.:X.700.701.,
                               names_to = "nm",
                               values_to = "Reflactance") 
  
  etiquetas_rojoRW1 <- c(rep(seq(621,700,1), each = 20, times = 1))
  repeticiones_rojoRW1 <- c(rep(seq(1,20,1), each = 1, times = 80))
  
  dff_rojoRW1 <- long_rojoRW1[with(long_rojoRW1, order(long_rojoRW1$nm)), ]
  
  dff_rojoRW1$nm <- etiquetas_rojoRW1
  dff_rojoRW1$repeticiones <- repeticiones_rojoRW1 
  Banda <- "Rojo"
  Rojo <- cbind(Banda, dff_rojoRW1)
  
  #Red_edgeRW1
  
  long_red_edgeRW1 <- pivot_longer(df_red_edgeRW1,
                                   cols =   X.700.701.:X.750.751.,
                                   names_to = "nm",
                                   values_to = "Reflactance") 
  
  etiquetas_red_edgeRW1 <- c(rep(seq(700,750,1), each = 20, times = 1))
  repeticiones_red_edgeRW1 <- c(rep(seq(1,20,1), each = 1, times = 51))
  
  dff_red_edgeRW1 <- long_red_edgeRW1[with(long_red_edgeRW1, order(long_red_edgeRW1$nm)), ]
  
  dff_red_edgeRW1$nm <- etiquetas_red_edgeRW1
  dff_red_edgeRW1$repeticiones <- repeticiones_red_edgeRW1 
  Banda <- "Red_edge"
  Red_edge <- cbind(Banda, dff_red_edgeRW1)
  
  #NIR_INFRAROJORW1
  
  long_NIRRW1 <- pivot_longer(df_NIRRW1,
                              cols =   X.751.752.:X.900.901.,
                              names_to = "nm",
                              values_to = "Reflactance") 
  
  etiquetas_NIRRW1 <- c(rep(seq(751,900,1), each = 20, times = 1))
  repeticiones_NIRRW1 <- c(rep(seq(1,20,1), each = 1, times = 150))
  
  dff_NIRRW1 <- long_NIRRW1[with(long_NIRRW1, order(long_NIRRW1$nm)), ]
  
  dff_NIRRW1$nm <- etiquetas_NIRRW1
  dff_NIRRW1$repeticiones <- repeticiones_NIRRW1 
  Banda <- "NIR"
  NIR <- cbind(Banda, dff_NIRRW1)
  
  output <- rbind(Azul,Verde,Rojo,Red_edge,NIR) 
  output <- as.data.frame(output)
  write_excel_csv(output, "output.csv")
  
  
  return(output)} #Definción de la función

#Se define el directorio
setwd("/media/antonio/TESIS/29_MARZO_ROT ISC/")

ISR <- Spectroan(Raw_data = ISR)
ISS <- Spectroan(Raw_data = ISS)
RR <- Spectroan(Raw_data = RR)
RS <- Spectroan(Raw_data = RS)

#etiquetas
Spectro_tag <- function(data){
  
  tag1 <- c(rep("Rottboellia_S", 8020))
  data <- cbind(tag1, data)
  colnames(data) <- c("ID", "Banda", "nm", "Reflectance", "Repeticiones")  
  data$ID <- as.factor(data$ID)  
  data$Banda <- as.factor(data$Banda) 
  data <- as.data.frame(data) }

#Uso de la funcion
ISR <- Spectro_tag(data = ISR)
ISS <- Spectro_tag(data = ISS)
RR <- Spectro_tag(data = RR)
RS <- Spectro_tag(data = RS)  
#Union de los data frame
Ischaemum <-rbind(ISR, ISS)
Rottboellia <- rbind(RR, RS)

Spectro_plot <- function(data){
  
  stat_descrip_nm_error <- data %>% 
    group_by(Banda, ID, nm) %>% 
    summarise(error = mean_se(Reflectance))
  
  Etiqueta <- data.frame(x= c(473.5, 533.5,661,725, 825),
                         y=c(0.1, 0.15,0.095, 0.5, 0.65),
                         label = c("Azul","Verde", "Rojo","Rojo borde","Infrarrojo cercano"))
  
  firma_espectral_error <- ggplot(stat_descrip_nm_error, aes(x=nm, y = error$y, color = ID))+
    geom_line()+
    geom_point(size=0.5)+
    geom_errorbar(aes(ymin=error$ymin, ymax=error$ymax))+
    #facet_wrap(~Banda)+
    geom_text(data=Etiqueta, aes(x=x, y=y, label=label), color = c("blue", "green", "red", "red3", "black")) +
    labs(x = "Longitud de onda (nm)", 
         y = "Reflectancia", 
         #  subtitle = "Ubicación: Pital
         #Fecha: 17-Feb-21", 
         #title = "Firma espectral de Asystasia gangetica y dos estados fenológicos de piña (A. comosus)", 
         #    color = "Especie", 
         caption = "El sombreado es el error estándar y las líneas representan el promedio (n=20)") +
    # scale_color_hue(labels = c("A. gangetica floración (sin sombra)", "A. gangetica floración (sarán 1)", "A. gangetica floración (sarán 2)", "Piña 14 meses (sin sombra)", "Piña 14 meses (sarán 1)", "Piña 14 meses (sarán 2)"))+
    scale_x_continuous(breaks = c(451, 496,571, 621, 701, 750, 900))+ #en lugar de 571 va 636!!!!
    scale_y_continuous(limits = c(0,1))+
    
    theme_bw()+
    theme(panel.grid.minor = element_blank(), 
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "grey",linetype = 5,size = 0.25),
          axis.text.x = element_text(colour = "black", size = 11),
          axis.text.y = element_text(colour = "black", size = 11),
          legend.title.align = 0.5,
          axis.title.y = element_text(size = 12),
          axis.title.x = element_text(size = 12),
          axis.line = element_line(color = "black"),
          legend.position = "bottom")
  #axis.ticks.x = element_blank())
  
}

plots <- Spectro_plot(data = Ischaemum)
plots_Rottboellia <- Spectro_plot(data = Rottboellia)

library(patchwork)
plots +
  plots_Rottboellia

    
    
