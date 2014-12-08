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

### Análisis por oferta académica
by(data$Formaci.n.posgrado, data$oferta, summary)
boxplot(Formaci.n.posgrado~oferta, data=data, main='Formación Posgrado')

by(data$Posgrado.en.formaci.n, data$oferta, summary)
boxplot(Posgrado.en.formaci.n~oferta, data=data, main='Posgrado en Formación')

by(data$Doctores.TC, data$oferta, summary)
boxplot(Doctores.TC~oferta, data=data, main='Doctores Tiempo Completo')

by(data$Estudiantes.por.docente.TC, data$oferta, summary)
boxplot(Estudiantes.por.docente.TC~oferta, data=data, main='Estudiantes por Docente TC')

# 1 - Pública
# 2 - Privada - Autofinanciada
# 3 - Cofinanciada

### Análisis por tipo de mantenimiento

# Criterio Academia
layout(matrix(c(1,2,3,4),2,2,byrow=TRUE))
boxplot(Formaci.n.posgrado~mantenimiento, data=data, main='Formación Posgrado')
boxplot(Posgrado.en.formaci.n~mantenimiento, data=data, main='Posgrado en Formación')
boxplot(Doctores.TC~mantenimiento, data=data, main='Doctores Tiempo Completo')
boxplot(Estudiantes.por.docente.TC~mantenimiento, data=data, main='Estudiantes por Docente TC')

layout(matrix(c(1,2,3,4),2,2,byrow=TRUE))
boxplot(Horas.clase.TC~mantenimiento, data=data, main='Horas Clase TC')
boxplot(Porcentaje.de.profesores.TC~mantenimiento, data=data, main='Porcentaje profesores TC')
boxplot(Horas.clase.MT.TP~mantenimiento, data=data, main='Horas Clase MT/TP')
boxplot(Titularidad~mantenimiento, data=data, main='Titularidad')

layout(matrix(c(1,2,3,4),2,2,byrow=TRUE))
boxplot(Titularidad.TC~mantenimiento, data=data, main='Titularidad TC')
boxplot(Concurso~mantenimiento, data=data, main='Concurso')
boxplot(Direcci.n.mujeres~mantenimiento, data=data, main='Dirección mujeres')
boxplot(Docencia.mujeres~mantenimiento, data=data, main='Docencia mujeres')

# Eficiencia Académica
layout(matrix(c(1,2,3,4),2,2,byrow=TRUE))
boxplot(Eficiencia.terminal.posgrado~mantenimiento, data=data, main='Eficiencia terminal Posgrado')
boxplot(Eficiencia.terminal.pregrado~mantenimiento, data=data, main='Eficiencia terminal Pregrado')
boxplot(Tasa.de.retenci.n.inicial.pregrado~mantenimiento, data=data, main='Tasa retención Pregrado')
boxplot(Admisi.n.a.estudios.de.pregrado~mantenimiento, data=data, main='Admisión a estudios Pregrado')

# Eficiencia Investigación
layout(matrix(c(1,2,3,4),2,2,byrow=TRUE))
boxplot(Investigaci.n.regional~mantenimiento, data=data, main='Investigación Regional')
boxplot(Producci.n.cient.fica~mantenimiento, data=data, main='Producción científica')
boxplot(Libros.revisados.por.pares~mantenimiento, data=data, main='Libros Revisados por pares')

# Eficiencia Organización
layout(matrix(c(1,2,3,4),2,2,byrow=TRUE))
boxplot(Programas.de.vinculaci.n~mantenimiento, data=data, main='Programas de vinculación')
boxplot(Presupuesto.de.programas.de.vinculaci.n~mantenimiento, data=data, main='Presupuesto Programas Vinculación')
boxplot(Calidad.del.gasto~mantenimiento, data=data, main='Calidad del Gasto')

# Eficiencia Infraestructura
layout(matrix(c(1,2,3,4),2,2,byrow=TRUE))
boxplot(Espacio.para.estudiantes~mantenimiento, data=data, main='Espacio para estudiantes')
boxplot(T.tulos.de.libros~mantenimiento, data=data, main='Títulos de libros')
boxplot(Consultas.por.usuario~mantenimiento, data=data, main='Consultas por usuario')
boxplot(Conectividad~mantenimiento, data=data, main='Conectividad')

layout(matrix(c(1,2,3,4),2,2,byrow=TRUE))
boxplot(Cobertura.a.estudiantes~mantenimiento, data=data, main='Cobertura a estudiantes')
boxplot(Calidad.de.aulas~mantenimiento, data=data, main='Calidad de aulas')
boxplot(Oficinas.TC~mantenimiento, data=data, main='Oficinas TC')
boxplot(Oficinas.MT.TP~mantenimiento, data=data, main='Oficinas MT/TP')









