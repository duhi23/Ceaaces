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

var <- c("Codigo", "Evaluacion Global IES", "Academia meta", "Eficiencia Academica meta", "Investigacion meta", "Organizacion meta",
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

library(ggplot2)

for(i in c(3:7)){
  mypath <- file.path("","Users","Diego","Dropbox","Proyectos","Ceaaces","Actualizacion",
                      paste(var[i], "_oferta", ".png", sep = ""))
  png(file=mypath)
  grafico <- ggplot(base, aes(x=base[,73], y=base[,i], fill=as.factor(base[,73]))) + labs(x=" ") +
    geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(fill=" ") + labs(title=var[i]) +
    labs(y="Indicador")+stat_boxplot(geom ='errorbar') + scale_fill_manual(values=colores) +
    theme(legend.position="none") + theme(text = element_text(size=15)) +
    annotate("text",y=aggregate(base[,i] ~ base[,73], base, function(i) round(median(i),2))[,2],
             x=c(1,2,3), label=str_replace(aggregate(base[,i] ~ base[,73], base, function(i) round(median(i),2))[,2], "0.","0,"), size=6, colour="snow") +
    annotate("segment",x=0.5, xend=3.5, y=median(base[,i]), yend=median(base[,i]), colour="red2", linetype="dotted", size=1.1)
  print(grafico)
  dev.off()
}


val <- rbind(aggregate(base[,i] ~ base[,73], base, function(i) round(min(i),4))[,2], # min
aggregate(base[,i] ~ base[,73], base, function(i) round(max(i),4))[,2], # max
aggregate(base[,i] ~ base[,73], base, function(i) round(sd(i),4))[,2], # sd
aggregate(base[,i] ~ base[,73], base, function(i) round(IQR(i),4))[,2], # IQR
aggregate(base[,i] ~ base[,73], base, function(i) round(quantile(i, probs=0.25),4))[,2], # Q1
aggregate(base[,i] ~ base[,73], base, function(i) round(quantile(i, probs=0.75),4))[,2]) # Q3

result <- cbind(c("min", "max", "sd", "IQR", "Q1", "Q3"), as.data.frame(val))

