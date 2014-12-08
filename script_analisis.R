##### Análisis de indicadores #####
setwd("C:/Users/SONY/Dropbox/Proyectos/Ceaaces")
list.files()

data <- read.table(file="base.csv", header=TRUE, sep=";", dec=",", stringsAsFactors=FALSE)
head(data)

# 1 - Grado
# 2 - Posgrado
# 3 - Grado & Posgrado

#save(list = ls(all = TRUE), file = "Ceaaces.RData")
#load(file="Ceaaces.RData")

by(data$Formaci.n.posgrado, data$oferta, summary)
boxplot(Formaci.n.posgrado~oferta, data=data, main='Formación Posgrado')

by(data$Posgrado.en.formaci.n, data$oferta, summary)
boxplot(Posgrado.en.formaci.n~oferta, data=data, main='Posgrado en Formación')

by(data$Doctores.TC, data$oferta, summary)
boxplot(Doctores.TC~oferta, data=data, main='Doctores Tiempo Completo')

by(data$Estudiantes.por.docente.TC, data$oferta, summary)
boxplot(Estudiantes.por.docente.TC~oferta, data=data, main='Estudiantes por Docente TC')





