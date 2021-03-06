#############################################
#####     Actualizacion de graficos     #####
###  Graficos Indicadores/Macrocriterios  ###
#############################################


datos <- read.table(file="/Users/Diego/Dropbox/Proyectos/Ceaaces/Datos/base.csv", 
                   header=TRUE, sep=";", dec=",", stringsAsFactors=FALSE)

vari <- c("Codigo", "Nombre", "Formacion posgrado", "Posgrado en formacion", "Doctores TC", "Estudiantes por docente TC",
"Horas clase TC", "Porcentaje de profesores TC", "Horas-clase MT-TP", "Titularidad", "Titularidad TC", "Concurso",
"Escalafon", "Evaluacion", "Remuneracion TC", "Remuneracion MT-TP", "Direccion mujeres", "Docencia mujeres", 
"Eficiencia terminal posgrado", "Eficiencia terminal pregrado", "Tasa de retencion inicial pregrado",
"Admision a estudios de pregrado", "Admision a estudios de posgrado", "Planificacion de la investigacion",
"Investigacion regional", "Produccion cientifica", "Libros revisados por pares", "Uso del seguimiento a graduados",
"Programas de vinculacion", "Presupuesto de programas de vinculacion", "Rendicion anual de cuentas",
"Transparencia financiera", "Etica y responsabilidad", "Calidad del gasto", "Informacion para la evaluacion",
"Regimen academico", "Accion afirmativa", "Espacio para estudiantes", "Titulos de libros", "Gestion de biblioteca", 
"Consultas por usuario", "Conectividad", "Innovacion tecnologica", "Cobertura a estudiantes", "Calidad de aulas", 
"Oficinas TC", "Oficinas MT-TP", "Espacios de bienestar", "oferta", "mantenimiento")

datos$mantenimiento <- factor(datos$mantenimiento, levels=c("Publica", "Privada", "Cofinanciada"))

library(ggplot2)
library(stringr)
colores <- c("green4", "deepskyblue3", "goldenrod3", "gold", "darkorange", "deepskyblue1", "orange", "orangered", "hotpink" )

for(i in c(3,4,5,6,7,8,9,10,11,12,15,16,17,18,19,20,21,25,26,27,29,30,34,38,39,41,42,44,45,46,47)){
  mypath <- file.path("","Users","Diego","Dropbox","Proyectos","Ceaaces","Actualizacion","Informe N1",
                      paste(vari[i], "_informe", ".png", sep = ""))
  png(file=mypath)
  grafico <- ggplot(datos, aes(x=datos[,50], y=datos[,i], fill=as.factor(datos[,50]))) + labs(x=" ") +
    geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(fill=" ") + labs(title=vari[i]) +
    labs(y="Indicador")+stat_boxplot(geom ='errorbar') + scale_fill_manual(values=colores) +
    theme(legend.position="none") + theme(text = element_text(size=15)) +
    annotate("segment",x=0.5, xend=4.5, y=median(datos[,i]), yend=median(datos[,i]), colour="red2", linetype="dotted", size=1.1)
  print(grafico)
  dev.off()
}

esta <- function(i){
  val <- rbind(aggregate(datos[,i] ~ datos[,50], datos, function(i) round(min(i),4))[,2], # min
               aggregate(datos[,i] ~ datos[,50], datos, function(i) round(max(i),4))[,2], # max
               aggregate(datos[,i] ~ datos[,50], datos, function(i) round(median(i),4))[,2], # median
               aggregate(datos[,i] ~ datos[,50], datos, function(i) round(sd(i),4))[,2], # sd
               aggregate(datos[,i] ~ datos[,50], datos, function(i) round(IQR(i),4))[,2], # IQR
               aggregate(datos[,i] ~ datos[,50], datos, function(i) round(quantile(i, probs=0.25),4))[,2], # Q1
               aggregate(datos[,i] ~ datos[,50], datos, function(i) round(quantile(i, probs=0.75),4))[,2]) # Q3
  result <- cbind(c("min", "max", "median", "sd", "IQR", "Q1", "Q3"), as.data.frame(val))
  colnames(result) <- c("nom", unlist(dimnames(table(datos[,50]))))
  return(list(result,colnames(datos)[i]))
}

esta(27)

lapply(c(13,14,22,23,24,28,31,32,33,35,36,37,40,43,48), function(i){list(table(datos[[i]],datos[[50]]), colnames(datos)[i])})

# Graficos histogramas - variables cualitativas
for(i in c(13,14,22,23,24,28,31,32,33,35,36,37,40,43,48)){
  mypath <- file.path("","Users","Diego","Dropbox","Proyectos","Ceaaces","Actualizacion","Informe N1",
                      paste(vari[i], "_cualitativa", ".png", sep = ""))
  png(file=mypath)
  datos[[i]] <- factor(datos[[i]], levels=c("ALTO","MEDIO","BAJO"))
  grafico <- ggplot(datos, aes(x=datos[[i]], fill=as.factor(datos[[i]]))) + geom_histogram(binwidth=4)+
    scale_fill_manual(values=colores) + labs(x=" ") + labs(title=vari[i]) + labs(fill="NIVEL") +
    labs(y="CASOS") + theme(text=element_text(size=14), legend.position="none") + facet_grid(. ~ mantenimiento)
  print(grafico)
  dev.off()
}

ggplot(datos, aes(x=datos[[i]], fill=as.factor(datos[[i]]))) + geom_histogram(binwidth=1)+
  scale_fill_manual(values=colores) + labs(x=" ") + labs(title=vari[i]) + labs(fill="NIVEL") +
  labs(y="CASOS") + theme(text=element_text(size=14), legend.position="none") + facet_grid(mantenimiento ~ .)

# Distribucion de IES
ggplot(datos, aes(x=datos[["mantenimiento"]], fill=as.factor(datos[["mantenimiento"]]))) + geom_histogram(binwidth=4)+
  scale_fill_manual(values=colores) + labs(x=" ") + labs(title="Tipo de Financiamiento") + labs(fill="Financiamiento") +
  labs(y="CASOS") + theme(text=element_text(size=14), legend.position="right")

# Graficos Informe 2
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
  mypath <- file.path("","Users","Diego","Dropbox","Proyectos","Ceaaces","Actualizacion","Categoria",
                      paste(var[i], "_categoria", ".png", sep = ""))
  png(file=mypath)
  grafico <- ggplot(base, aes(x=base[,84], y=base[,i], fill=as.factor(base[,84]))) + labs(x=" ") +
    geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(fill=" ") + labs(title=var[i]) +
    labs(y="Indicador")+stat_boxplot(geom ='errorbar') + scale_fill_manual(values=colores) +
    theme(legend.position="none") + theme(text = element_text(size=15)) +
    #annotate("text",y=aggregate(base[,i] ~ base[,84], base, function(i) round(median(i),2))[,2],
    #         x=c(1,2,3), label=str_replace(aggregate(base[,i] ~ base[,84], base, function(i) round(median(i),2))[,2], "0.","0,"), size=6, colour="snow") +
    annotate("segment",x=0.5, xend=4.5, y=median(base[,i]), yend=median(base[,i]), colour="red2", linetype="dotted", size=1.1)
  print(grafico)
  dev.off()
}

est <- function(i){
  val <- rbind(aggregate(base[,i] ~ base[,84], base, function(i) round(min(i),4))[,2], # min
               aggregate(base[,i] ~ base[,84], base, function(i) round(max(i),4))[,2], # max
               aggregate(base[,i] ~ base[,84], base, function(i) round(median(i),4))[,2], # median
               aggregate(base[,i] ~ base[,84], base, function(i) round(sd(i),4))[,2], # sd
               aggregate(base[,i] ~ base[,84], base, function(i) round(IQR(i),4))[,2], # IQR
               aggregate(base[,i] ~ base[,84], base, function(i) round(quantile(i, probs=0.25),4))[,2], # Q1
               aggregate(base[,i] ~ base[,84], base, function(i) round(quantile(i, probs=0.75),4))[,2]) # Q3
  result <- cbind(c("min", "max", "median", "sd", "IQR", "Q1", "Q3"), as.data.frame(val))
  colnames(result) <- c("nom", unlist(dimnames(table(base[,84]))))
  return(list(result,colnames(base)[i]))
}

est(11)
# Ejecutamos el calculo para toda la base
lapply(c(60:72), function(i){est(i)})

library(plyr)
library(TeachingDemos)
source("boxout.R")

# Valores atipicos
boxplot.with.outlier.label(base[,11]~base[,84], base$Codigo, push_text_right = .6, range = .2,
                           segement_width_as_percent_of_label_dist = 0.35, data=base, spread_text = F)

# Percent over global median 3:7 -- 74 >
pmda <- function(i,j){
  100*round(prop.table(table(median(base[,i])>base[,i], base[,j]), 2), 4)
}

pmda(3,87)


# Creacion base para analizar los campos CINE

campo1 <- data.frame(subset(base, base$CA01==1)[,1:7], campo=rep(1,37))
campo2 <- data.frame(subset(base, base$CA02==1)[,1:7], campo=rep(2,37))
campo3 <- data.frame(subset(base, base$CA03==1)[,1:7], campo=rep(3,52))
campo4 <- data.frame(subset(base, base$CA04==1)[,1:7], campo=rep(4,42))
campo5 <- data.frame(subset(base, base$CA05==1)[,1:7], campo=rep(5,38))
campo6 <- data.frame(subset(base, base$CA06==1)[,1:7], campo=rep(6,32))
campo7 <- data.frame(subset(base, base$CA07==1)[,1:7], campo=rep(7,35))
campo8 <- data.frame(subset(base, base$CA08==1)[,1:7], campo=rep(8,41))

datos <- rbind(campo1, campo2, campo3, campo4, campo5, campo6, campo7, campo8)
datos$ncampo <- factor(datos$campo, levels=c(1,2,3,4,5,6,7,8), 
                      labels=c("CA01", "CA02", "CA03", "CA04", "CA05", "CA06", "CA07", "CA08"))


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


esti <- function(i){
  val <- rbind(aggregate(datos[,i] ~ datos[,9], datos, function(i) round(min(i),4))[,2], # min
               aggregate(datos[,i] ~ datos[,9], datos, function(i) round(max(i),4))[,2], # max
               aggregate(datos[,i] ~ datos[,9], datos, function(i) round(median(i),4))[,2], # median
               aggregate(datos[,i] ~ datos[,9], datos, function(i) round(sd(i),4))[,2], # sd
               aggregate(datos[,i] ~ datos[,9], datos, function(i) round(IQR(i),4))[,2], # IQR
               aggregate(datos[,i] ~ datos[,9], datos, function(i) round(quantile(i, probs=0.25),4))[,2], # Q1
               aggregate(datos[,i] ~ datos[,9], datos, function(i) round(quantile(i, probs=0.75),4))[,2]) # Q3
  result <- cbind(c("min", "max", "median", "sd", "IQR", "Q1", "Q3"), as.data.frame(val))
  colnames(result) <- c("nom", unlist(dimnames(table(datos[,9]))))
  return(result)
}

esti(3)

median(datos[,i])
unlist(dimnames(table(datos[,8])))

unique(datos[,8])

pmd <- function(i,j){
  100*round(prop.table(table(median(base[,i])>base[,i], base[,j]), 2), 4)
}

pmd(3,74)


# Valores atipicos
boxplot.with.outlier.label(datos[,6] ~ datos[,9], datos$Codigo, push_text_right = .6, range = .2,
                           segement_width_as_percent_of_label_dist = 0.35, data=datos, spread_text = F)

