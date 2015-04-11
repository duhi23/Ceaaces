## Actualizacion de graficos 

old.dir <- "/Users/Diego/Dropbox/Proyectos/Ceaaces"
new.dir <- "/Users/Diego/Dropbox/Proyectos/Ceaaces/Datos"

setwd(new.dir)
list.files()

## Cargamos los archivos originales
options(stringsAsFactors=FALSE)
library(gdata)
base <- read.xls("puntajes_ponderados.xlsx", sheet=1, header=TRUE)
str(base)

plot(base$Evaluacion.Global.IES)

var <- c("Codigo", "Evaluacion Global IES", "Academia", "Eficiencia Academica", "Investigacion", "Organizacion",
         "Infraestructura", "Posgrado meta", "Carrera docente meta", "Institucionalizacion meta", "Produccion cientifica",	
         "Remuneraciones meta", "Formacion posgrado", "Dedicacion meta", "Remuneracion TC", "Tiempo completo meta",	
         "Doctores TC", "Libros revisados por pares", "Gestion interna meta", "Calidad del gasto", "Transparencia meta", 
         "Biblioteca meta", "Vinculacion colectividad meta", "Reglamentacion meta", "Tasa de retencion inicial pregrado",	
         "Eficiencia terminal pregrado", "Estabilidad meta", "TIC meta", "Investigacion regional", "Espacios docentes meta",	
         "Admision a estudios de pregrado", "Porcentaje TC", "Estudiantes por docente TC", "Planificacion de la investigacion",	
         "Espacios de Bienestar", "Calidad Aulas", "Bienestar meta", "Aulas meta", "Oficinas TC", "Escalafon meta",	
         "Libros", "Eficiencia terminal posgrado", "Titularidad TC", "Espacio", "Remuneracion MT-TP",	"Posgrado en Formacion", 
         "Derechos mujeres meta", "Conectividad", "Admision a estudios de posgrado", "Informacion para la evaluacion", 
         "Horas-clase TC", "Titularidad", "Tiempos parciales y medio tiempo meta", "Transparencia1", "Regimen Academico", 
         "Accion afirmativa", "Uso del seguimiento a graduados", "Programas de vinculacion", "Rendicion anual de cuentas",
         "Etica y responsabilidad", "Horas-clase MT-TP", "Salas para MT-TP", "Innovacion Tecnologica", 
         "Gestion de biblioteca", "Cobertura Estudiantes", "Concurso", "Docencia-Mujeres", "Direccion-Mujeres", 
         "Consultas por usuario", "Evaluacion", "Escalafon1", "Presupuesto de programas de vinculacion", "oferta", 
         "mantenimiento", "CA08", "CA05", "CA03", "CA01", "CA02", "CA07", "CA09", "CA10", "region", "categoria", "distrito",
         "nestudiantes", "intervalos")

base$oferta <- factor(base$oferta, levels=c("Pregrado", "Posgrado", "Pregrado y Posgrado"))
base$region <- factor(base$region, levels=c("Costa", "Sierra", "Amazonia"))
base$mantenimiento <- factor(base$mantenimiento, levels=c("Publica", "Privada", "Cofinanciada"))
base$distrito <- factor(base$distrito, levels=c("Guayas", "Pichincha", "Otros"))
# Intervalos - Numero de estudiantes
base$intervalos <- cut(base$nestudiantes, breaks=c(0,2000,6062,13320,73032), dig.lab=5)

library(ggplot2)
library(stringr)

colores <- c("green4", "deepskyblue3", "goldenrod3", "gold", "darkorange", "deepskyblue1", "orange", "orangered", "hotpink" )

for(i in c(2:72)){
  mypath <- file.path("","Users","Diego","Dropbox","Proyectos","Ceaaces","Actualizacion","Oferta",
                      paste(var[i], "_oferta", ".png", sep = ""))
  png(file=mypath)
  grafico <- ggplot(base, aes(x=base[,73], y=base[,i], fill=as.factor(base[,73]))) + labs(x=" ") +
    geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(fill=" ") + labs(title=var[i]) +
    labs(y="Indicador")+stat_boxplot(geom ='errorbar') + scale_fill_manual(values=colores) +
    theme(legend.position="none") + theme(text = element_text(size=15)) +
    #annotate("text",y=aggregate(base[,i] ~ base[,73], base, function(i) round(median(i),2))[,2],
    #         x=c(1,2,3), label=str_replace(aggregate(base[,i] ~ base[,73], base, function(i) round(median(i),2))[,2], "0.","0,"), size=6, colour="snow") +
    annotate("segment",x=0.5, xend=4.5, y=median(base[,i]), yend=median(base[,i]), colour="red2", linetype="dotted", size=1.1)
  print(grafico)
  dev.off()
}

est <- function(i){
  val <- rbind(aggregate(base[,i] ~ base[,73], base, function(i) round(min(i),4))[,2], # min
               aggregate(base[,i] ~ base[,73], base, function(i) round(max(i),4))[,2], # max
               aggregate(base[,i] ~ base[,73], base, function(i) round(median(i),4))[,2], # median
               aggregate(base[,i] ~ base[,73], base, function(i) round(sd(i),4))[,2], # sd
               aggregate(base[,i] ~ base[,73], base, function(i) round(IQR(i),4))[,2], # IQR
               aggregate(base[,i] ~ base[,73], base, function(i) round(quantile(i, probs=0.25),4))[,2], # Q1
               aggregate(base[,i] ~ base[,73], base, function(i) round(quantile(i, probs=0.75),4))[,2]) # Q3
  result <- cbind(c("min", "max", "median", "sd", "IQR", "Q1", "Q3"), as.data.frame(val))
  colnames(result) <- c("nom", unlist(dimnames(table(base[,73]))))
  return(result)
}


library(plyr)
library(TeachingDemos)
source("boxout.R")

# Valores atipicos
boxplot.with.outlier.label(base[,6]~base[,74], base$Codigo, push_text_right = .6, range = .2,
                           segement_width_as_percent_of_label_dist = 0.35, data=base, spread_text = F)

# Creacion base para analizar los campos CINE

campo1 <- data.frame(subset(base, base$CA01=="SI")[,1:7], campo=rep(1,45))
campo2 <- data.frame(subset(base, base$CA02=="SI")[,1:7], campo=rep(2,39))
campo3 <- data.frame(subset(base, base$CA03=="SI")[,1:7], campo=rep(3,53))
campo5 <- data.frame(subset(base, base$CA05=="SI")[,1:7], campo=rep(5,49))
campo7 <- data.frame(subset(base, base$CA07=="SI")[,1:7], campo=rep(7,43))
campo8 <- data.frame(subset(base, base$CA08=="SI")[,1:7], campo=rep(8,35))
campo9 <- data.frame(subset(base, base$CA09=="SI")[,1:7], campo=rep(9,42))
campo10 <- data.frame(subset(base, base$CA10=="SI")[,1:7], campo=rep(10,44))

datos <- rbind(campo1, campo2, campo3, campo5, campo7, campo8, campo9, campo10)
datos$ncampo <- factor(datos$campo, levels=c(1,2,3,4,5,6,7,8,9,10), 
                      labels=c("CA01", "CA02", "CA03", "CA04", "CA05", "CA06", "CA07", "CA08", "CA09", "CA10"))



for(i in c(2:7)){
  mypath <- file.path("","Users","Diego","Dropbox","Proyectos","Ceaaces","Actualizacion","Areas",
                      paste(var[i], "_areas", ".png", sep = ""))
  png(file=mypath)
  grafico <- ggplot(datos, aes(x=datos[,9], y=datos[,i], fill=as.factor(datos[,9]))) + labs(x=" ") +
    geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(fill=" ") + labs(title=var[i]) +
    labs(y="Indicador")+stat_boxplot(geom ='errorbar') + scale_fill_manual(values=colores) +
    theme(legend.position="none") + theme(text = element_text(size=15)) +
    annotate("segment",x=0.5, xend=8.5, y=median(datos[,i]), yend=median(datos[,i]), colour="red2", linetype="dotted", size=1.1)
  print(grafico)
  dev.off()
}


esta <- function(i){
  val <- rbind(aggregate(datos[,i] ~ datos[,8], datos, function(i) round(min(i),4))[,2], # min
               aggregate(datos[,i] ~ datos[,8], datos, function(i) round(max(i),4))[,2], # max
               aggregate(datos[,i] ~ datos[,8], datos, function(i) round(median(i),4))[,2], # median
               aggregate(datos[,i] ~ datos[,8], datos, function(i) round(sd(i),4))[,2], # sd
               aggregate(datos[,i] ~ datos[,8], datos, function(i) round(IQR(i),4))[,2], # IQR
               aggregate(datos[,i] ~ datos[,8], datos, function(i) round(quantile(i, probs=0.25),4))[,2], # Q1
               aggregate(datos[,i] ~ datos[,8], datos, function(i) round(quantile(i, probs=0.75),4))[,2]) # Q3
  result <- cbind(c("min", "max", "median", "sd", "IQR", "Q1", "Q3"), as.data.frame(val))
  colnames(result) <- c("nom", unlist(dimnames(table(datos[,8]))))
  return(result)
}

esta(3)


# Valores atipicos
boxplot.with.outlier.label(datos[,3] ~ datos[,8], datos$Codigo, push_text_right = .6, range = .2,
                           segement_width_as_percent_of_label_dist = 0.35, data=datos, spread_text = F)

