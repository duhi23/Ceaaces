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

variables <- c("Codigo", "Evaluacion Global IES", "Academia meta", "Eficiencia Academica meta", "Investigacion meta", "Organizacion meta",
               "Infraestructura meta", "Posgrado meta", "Carrera docente meta", "Institucionalizacion meta", "Produccion cientifica",	
               "Remuneraciones meta", "Formacion posgrado", "Dedicacion meta", "Remuneracion TC", "Tiempo completo meta",	
               "Doctores TC", "Libros revisados por pares", "Gestion interna meta", "Calidad del gasto", "Transparencia meta", 
               "Biblioteca meta", "Vinculacion colectividad meta", "Reglamentacion meta", "Tasa de retencion inicial pregrado",	
               "Eficiencia terminal pregrado", "Estabilidad meta", "TIC meta", "Investigacion regional", "Espacios docentes meta",	
               "Admision a estudios de pregrado", "Porcentaje TC", "Estudiantes por docente TC", "Planificacion de la investigacion",	
               "Espacios de Bienestar", "Calidad Aulas", "Bienestar meta", "Aulas meta", "Oficinas TC", "Escalafon meta",	
               "Libros", "Eficiencia terminal posgrado", "Titularidad TC", "Espacio", "Remuneracion MT/TP",	"Posgrado en Formacion", 
               "Derechos mujeres meta", "Conectividad", "Admision a estudios de posgrado", "Informacion para la evaluacion", 
               "Horas-clase TC", "Titularidad", "Tiempos parciales y medio tiempo meta", "Transparencia1", "Regimen Academico", 
               "Accion afirmativa", "Uso del seguimiento a graduados", "Programas de vinculacion", "Rendicion anual de cuentas",
               "Etica y responsabilidad", "Horas-clase MT/TP", "Salas para MT/TP", "Innovacion Tecnologica", 
               "Gestion de biblioteca", "Cobertura Estudiantes", "Concurso", "Docencia-Mujeres", "Direccion-Mujeres", 
               "Consultas por usuario", "Evaluacion", "Escalafon1", "Presupuesto de programas de vinculacion", "oferta", 
               "mantenimiento", "CA08", "CA05", "CA03", "CA01", "CA02", "CA07", "CA09", "CA10", "region")

## Reciclamos variables utilizadas vcomo filtros de la base anterior
#write.table(data_new, file="base_anterior.txt", row.names=FALSE, sep=";", dec=",")

options(stringsAsFactors=FALSE)
data <- read.csv(file="/Users/Diego/Dropbox/Proyectos/Ceaaces/Datos/macro_criterios.csv", 
                 header=TRUE, dec=",", sep=";")

library(ggplot2)

var <-c("Institucion", "Codigo", "Academia", "Investigacion", "Infraestructura", "Organizacion",
        "Eficiencia Academica", "Oferta", "Tipo Mantenimiento", "CA08", "CA05", "CA03", "CA01",
        "CA02", "CA07", "CA09", "CA10", "Region")

for(i in c(3:7)){
  mypath <- file.path("","Users","Diego","Dropbox","Proyectos","Ceaaces","Actualizacion",
                      paste(var[i], "_region", ".png", sep = ""))
  png(file=mypath)
  grafico <- ggplot(data, aes(x=data[,18], y=data[,i], fill=as.factor(data[,18]))) + labs(x=" ") +
    geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(fill=" ") + labs(title=var[i]) +
    labs(y="Indicador")+stat_boxplot(geom ='errorbar') + scale_fill_manual(values=colores) +
    theme(legend.position="none") + theme(text = element_text(size=15)) +
    annotate("text",y=aggregate(data[,i] ~ data[,18], data, function(i) round(median(i),2))[,2],
             x=c(1,2,3), label=aggregate(data[,i] ~ data[,18], data, function(i) round(median(i),2))[,2], size=6, colour="snow") +
    annotate("segment",x=0.5, xend=3.5, y=median(data[,i]), yend=median(data[,i]), colour="red2", linetype="dotted", size=1.1)
  print(grafico)
  dev.off()
}



