##### Análisis de indicadores #####
#setwd("C:/Users/SONY/Dropbox/Proyectos/Ceaaces")
setwd("C:/Users/dph3/Desktop/LaTeX/ceaaces")
list.files()

data <- read.table(file="base.csv", header=TRUE, sep=";", dec=",", stringsAsFactors=FALSE)
head(data)

# 1 - Grado
# 2 - Posgrado
# 3 - Grado & Posgrado

#save(list = ls(all = TRUE), file = "Ceaaces.RData")
#load(file="Ceaaces.RData")

data$mantenimiento <- factor(data$mantenimiento, levels=c(1,2,3), labels=c('P?blica','Privada','Cofinanciada'))

library(plyr)
library(TeachingDemos)
source("http://www.r-statistics.com/wp-content/uploads/2011/01/boxplot-with-outlier-label-r.txt") # Load the function
boxplot.with.outlier.label(data$Formaci.n.posgrado~data$mantenimiento, data$C.digo, push_text_right = .6, range = .2,
                           segement_width_as_percent_of_label_dist = 0.35, data=data, spread_text = F)

colores <- c("green4","deepskyblue3","goldenrod3","greenyellow","yellow","gold","orange","orangered","red3")
# 1 - P?blica
# 2 - Privada - Autofinanciada
# 3 - Cofinanciada

### Análisis por tipo de mantenimiento
colore <- c("green3","yellow","orange","red")
ggplot(data,aes(x=mantenimiento,fill=as.factor(mantenimiento)))+geom_histogram(binwidth=4)+scale_fill_manual(values=colore)+
  labs(x="MANTENIMIENTO")+labs(y="IES") +labs(fill="MANTENIMIENTO") + theme(text = element_text(size = 16))


## Guardar gráficas
#mypath <- file.path("C:","Users","Diego Paul","Dropbox","Proyectos","Ceaaces","Graficos",
#                   paste("formacion_posgrado", ".png", sep = ""))
mypath <- file.path("C:","Users","SONY","Dropbox","Proyectos","Ceaaces","Graficos",
                    paste("formacion_posgrado", ".png", sep = ""))
png(file=mypath)
ggplot(data, aes(x=mantenimiento, y=Formaci.n.posgrado, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Formación Posgrado")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))+
  geom_text(data = aggregate(Formaci.n.posgrado ~ mantenimiento , data, function(i) round(median(i),2)), 
  aes(label = Formaci.n.posgrado,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=8)
dev.off()


#C:\Users\SONY\Dropbox\Proyectos\Ceaaces\Graficos
#C:\Users\Diego Paul\Dropbox\Proyectos\Ceaaces\Graficos

# Criterio Academia
source("multiplot.R")
# Formaci?n Posgrado
p1 <- ggplot(data, aes(x=mantenimiento, y=Formaci.n.posgrado, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Formación Posgrado")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))

boxplot.with.outlier.label(data$Formaci.n.posgrado~data$mantenimiento, data$C.digo, push_text_right = .6, range = .2,
                           segement_width_as_percent_of_label_dist = 0.35, data=data, spread_text = F)

# Posgrado en formaci?n
p2 <- ggplot(data, aes(x=mantenimiento, y=Posgrado.en.formaci.n, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Posgrado en formación")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))

boxplot.with.outlier.label(data$Posgrado.en.formaci.n~data$mantenimiento, data$C.digo, push_text_right = .6, range = .2,
                           segement_width_as_percent_of_label_dist = 0.35, data=data, spread_text = F)

# Doctores TC
p3 <- ggplot(data, aes(x=mantenimiento, y=Doctores.TC, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Doctores tiempo completo")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))

boxplot.with.outlier.label(data$Doctores.TC~data$mantenimiento, data$C.digo, push_text_right = .6, range = .2,
                           segement_width_as_percent_of_label_dist = 0.35, data=data, spread_text = F)

# Estudiantes por docente TC
p4 <- ggplot(data, aes(x=mantenimiento, y=Estudiantes.por.docente.TC, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Estudiantes por docente TC")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))

boxplot.with.outlier.label(data$Estudiantes.por.docente.TC~data$mantenimiento, data$C.digo, push_text_right = .6, range = .2,
                           segement_width_as_percent_of_label_dist = 0.35, data=data, spread_text = F)

# Horas clase TC
p5 <- ggplot(data, aes(x=mantenimiento, y=Horas.clase.TC, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Horas clase TC")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))

boxplot.with.outlier.label(data$Horas.clase.TC~data$mantenimiento, data$C.digo, push_text_right = .6, range = .2,
                           segement_width_as_percent_of_label_dist = 0.35, data=data, spread_text = F)

# Horas clase TC
p6 <- ggplot(data, aes(x=mantenimiento, y=Porcentaje.de.profesores.TC, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Porcentaje profesores TC")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))

boxplot.with.outlier.label(data$Porcentaje.de.profesores.TC~data$mantenimiento, data$C.digo, push_text_right = .6, range = .2,
                           segement_width_as_percent_of_label_dist = 0.35, data=data, spread_text = F)

multiplot(p1, p2, p3, p4, p5, p6, cols=3)

# Hora clase MT/TP
p1 <- ggplot(data, aes(x=mantenimiento, y=Horas.clase.MT.TP, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Horas clase MT/TP")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))

boxplot.with.outlier.label(data$Horas.clase.MT.TP~data$mantenimiento, data$C.digo, push_text_right = .6, range = .2,
                           segement_width_as_percent_of_label_dist = 0.35, data=data, spread_text = F)

# Titularidad
p2 <- ggplot(data, aes(x=mantenimiento, y=Titularidad, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Titularidad")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))

boxplot.with.outlier.label(data$Titularidad~data$mantenimiento, data$C.digo, push_text_right = .6, range = .2,
                           segement_width_as_percent_of_label_dist = 0.35, data=data, spread_text = F)

# Titularidad TC
p3 <- ggplot(data, aes(x=mantenimiento, y=Titularidad.TC, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Titularidad TC")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))
# Concurso
p4 <- ggplot(data, aes(x=mantenimiento, y=Concurso, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Concurso")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))

boxplot.with.outlier.label(data$Concurso~data$mantenimiento, data$C.digo, push_text_right = .6, range = .2,
                           segement_width_as_percent_of_label_dist = 0.35, data=data, spread_text = F)

# Direccion mujeres
p5 <- ggplot(data, aes(x=mantenimiento, y=Direcci.n.mujeres, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Dirección mujeres")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))

boxplot.with.outlier.label(data$Direcci.n.mujeres~data$mantenimiento, data$C.digo, push_text_right = .6, range = .2,
                           segement_width_as_percent_of_label_dist = 0.35, data=data, spread_text = F)

# Docencia mujeres
p6 <- ggplot(data, aes(x=mantenimiento, y=Docencia.mujeres, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Docencia mujeres")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))

boxplot.with.outlier.label(data$Docencia.mujeres~data$mantenimiento, data$C.digo, push_text_right = .6, range = .2,
                           segement_width_as_percent_of_label_dist = 0.35, data=data, spread_text = F)

multiplot(p1, p2, p3, p4, p5, p6, cols=3)

# Remuneraciones

p1 <- ggplot(data, aes(x=mantenimiento, y=Remuneraci.n.TC, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Remuneración TC")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))

boxplot.with.outlier.label(data$Remuneraci.n.TC~data$mantenimiento, data$C.digo, push_text_right = .6, range = .2,
                           segement_width_as_percent_of_label_dist = 0.35, data=data, spread_text = F)

p2 <- ggplot(data, aes(x=mantenimiento, y=Remuneraci.n.MT.TP, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Remuneración MT/TP")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))

boxplot.with.outlier.label(data$Remuneraci.n.MT.TP~data$mantenimiento, data$C.digo, push_text_right = .6, range = .2,
                           segement_width_as_percent_of_label_dist = 0.35, data=data, spread_text = F)

multiplot(p1, p2, cols=2)







# Eficiencia Académica
# Eficiencia terminal posgrado
p1 <- ggplot(data, aes(x=mantenimiento, y=Eficiencia.terminal.posgrado, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Eficiencia terminal posgrado")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))

boxplot.with.outlier.label(data$Eficiencia.terminal.posgrado~data$mantenimiento, data$C.digo, push_text_right = .6, range = .2,
                           segement_width_as_percent_of_label_dist = 0.35, data=data, spread_text = F)

# Eficiencia terminal pregrado
p2 <- ggplot(data, aes(x=mantenimiento, y=Eficiencia.terminal.pregrado, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Eficiencia terminal pregrado")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))

boxplot.with.outlier.label(data$Eficiencia.terminal.pregrado~data$mantenimiento, data$C.digo, push_text_right = .6, range = .2,
                           segement_width_as_percent_of_label_dist = 0.35, data=data, spread_text = F)

# Tasa de retencion inicial pregrado
p3 <- ggplot(data, aes(x=mantenimiento, y=Tasa.de.retenci.n.inicial.pregrado, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Tasa retencion inicial pregrado")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))


boxplot.with.outlier.label(data$Tasa.de.retenci.n.inicial.pregrado~data$mantenimiento, data$C.digo, push_text_right = .6, range = .2,
                           segement_width_as_percent_of_label_dist = 0.35, data=data, spread_text = F)

multiplot(p1, p2, p3, cols=3)


# Eficiencia Investigación
# Investigación regional
p1 <- ggplot(data, aes(x=mantenimiento, y=Investigaci.n.regional, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Investigación regional")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))

boxplot.with.outlier.label(data$Investigaci.n.regional~data$mantenimiento, data$C.digo, push_text_right = .6, range = .2,
                           segement_width_as_percent_of_label_dist = 0.35, data=data, spread_text = F)

# Producción científica
p2 <- ggplot(data, aes(x=mantenimiento, y=Producci.n.cient.fica, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Producción científica")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))

boxplot.with.outlier.label(data$Producci.n.cient.fica~data$mantenimiento, data$C.digo, push_text_right = .6, range = .2,
                           segement_width_as_percent_of_label_dist = 0.35, data=data, spread_text = F)

# Libros revisados por pares
p3 <- ggplot(data, aes(x=mantenimiento, y=Libros.revisados.por.pares, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Libros revisados por pares")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))

boxplot.with.outlier.label(data$Libros.revisados.por.pares~data$mantenimiento, data$C.digo, push_text_right = .6, range = .2,
                           segement_width_as_percent_of_label_dist = 0.35, data=data, spread_text = F)

multiplot(p1, p2, p3, cols=3)


# Eficiencia Organización
# Programas de vinculacion
p1 <- ggplot(data, aes(x=mantenimiento, y=Programas.de.vinculaci.n, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Programas de vinculación")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))

boxplot.with.outlier.label(data$Programas.de.vinculaci.n~data$mantenimiento, data$C.digo, push_text_right = .6, range = .2,
                           segement_width_as_percent_of_label_dist = 0.35, data=data, spread_text = F)

# Presupuesto programas vinculacion
p2 <- ggplot(data, aes(x=mantenimiento, y=Presupuesto.de.programas.de.vinculaci.n, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Presupuesto programas vinculación")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))

boxplot.with.outlier.label(data$Presupuesto.de.programas.de.vinculaci.n~data$mantenimiento, data$C.digo, push_text_right = .6, range = .2,
                           segement_width_as_percent_of_label_dist = 0.35, data=data, spread_text = F)

# calidad del gasto
p3 <- ggplot(data, aes(x=mantenimiento, y=Calidad.del.gasto, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Calidad del gasto")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))

boxplot.with.outlier.label(data$Calidad.del.gasto~data$mantenimiento, data$C.digo, push_text_right = .6, range = .2,
                           segement_width_as_percent_of_label_dist = 0.35, data=data, spread_text = F)

multiplot(p1, p2, p3, cols=3)


# Eficiencia Infraestructura
# Espacios para estudiantes
p1 <- ggplot(data, aes(x=mantenimiento, y=Espacio.para.estudiantes, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Espacios para estudiantes")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))

boxplot.with.outlier.label(data$Espacio.para.estudiantes~data$mantenimiento, data$C.digo, push_text_right = .6, range = .2,
                           segement_width_as_percent_of_label_dist = 0.35, data=data, spread_text = F)

# Titulos de libros 
p2 <- ggplot(data, aes(x=mantenimiento, y=T.tulos.de.libros, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Titulo de libros")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))

boxplot.with.outlier.label(data$T.tulos.de.libros~data$mantenimiento, data$C.digo, push_text_right = .6, range = .2,
                           segement_width_as_percent_of_label_dist = 0.35, data=data, spread_text = F)

# Consultas por usuario
p3 <- ggplot(data, aes(x=mantenimiento, y=Consultas.por.usuario, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Consultas por usuario")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))

boxplot.with.outlier.label(data$Consultas.por.usuario~data$mantenimiento, data$C.digo, push_text_right = .6, range = .2,
                           segement_width_as_percent_of_label_dist = 0.35, data=data, spread_text = F)

# Conectividad
p4 <- ggplot(data, aes(x=mantenimiento, y=Conectividad, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Conectividad")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))

boxplot.with.outlier.label(data$Conectividad~data$mantenimiento, data$C.digo, push_text_right = .6, range = .2,
                           segement_width_as_percent_of_label_dist = 0.35, data=data, spread_text = F)

# Cobertura a estudiantes
p5 <- ggplot(data, aes(x=mantenimiento, y=Cobertura.a.estudiantes, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Cobertura a estudiantes")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))

boxplot.with.outlier.label(data$Cobertura.a.estudiantes~data$mantenimiento, data$C.digo, push_text_right = .6, range = .2,
                           segement_width_as_percent_of_label_dist = 0.35, data=data, spread_text = F)

# Calidad de aulas
p6 <- ggplot(data, aes(x=mantenimiento, y=Calidad.de.aulas, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Calidad de aulas")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))

boxplot.with.outlier.label(data$Calidad.de.aulas~data$mantenimiento, data$C.digo, push_text_right = .6, range = .2,
                           segement_width_as_percent_of_label_dist = 0.35, data=data, spread_text = F)

multiplot(p1, p2, p3, p4, p5, p6, cols=3)

# Oficinas TC
p1 <- ggplot(data, aes(x=mantenimiento, y=Oficinas.TC, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Oficinas TC")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))

boxplot.with.outlier.label(data$Oficinas.TC~data$mantenimiento, data$C.digo, push_text_right = .6, range = .2,
                           segement_width_as_percent_of_label_dist = 0.35, data=data, spread_text = F)

# Oficinas MT/TP
p2 <- ggplot(data, aes(x=mantenimiento, y=Oficinas.MT.TP, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Oficinas MT/TP")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))

boxplot.with.outlier.label(data$Oficinas.MT.TP~data$mantenimiento, data$C.digo, push_text_right = .6, range = .2,
                           segement_width_as_percent_of_label_dist = 0.35, data=data, spread_text = F)

multiplot(p1, p2, cols=2)





#ggplot(data,aes(x=STATUS_ACTUAL,fill=as.factor(STATUS_ACTUAL)))+geom_histogram(binwidth=4)+scale_fill_manual(values=colore)+
#  labs(x="ESTATUS ACTUAL")+labs(y="CASOS") +labs(fill="ESTATUS")


colores <- c("green3","gold","orange","orangered","red3")
ggplot(data, aes(x=mantenimiento, y=Formaci.n.posgrado,fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour = "red",outlier.shape = 17,outlier.size = 1.5) + 
  stat_boxplot(geom ='errorbar') + labs(x="TIPO MANTENIMIENTO")+labs(y="INDICADOR") + labs(fill="MANTENIMIENTO") +
  scale_fill_manual(values=colores) + theme(legend.position="bottom") + theme(text = element_text(size = 16))

##### Información de oferta académica #####

oferta <- read.table("oferta_academica.csv", header=TRUE, sep=";")
data_new <- merge(data,oferta, by.x="C.digo", by.y="CODIGO")
save(list = ls(all = TRUE), file = "Ceaaces.RData")

# 01 - Educaci?n
# 02 - Artes y humanidades
# 03 - Ciencias Sociales, Periodismo e informaci?n
# 05 - Ciencias Naturales, matem?ticas y estad?stica


data_new <- rbind(data[,c(1:50)],data[,c(1:50)],data[,c(1:50)],data[,c(1:50)],
                  data[,c(1:50)],data[,c(1:50)],data[,c(1:50)],data[,c(1:50)])
res <- c(data$CA01,data$CA02,data$CA03,data$CA05,data$CA07,data$CA08,data$CA09,data$CA10)
#names(res) <- c("AREA")
campo <- c(rep("CA01",56),rep("CA02",56),rep("CA03",56),rep("CA05",56),rep("CA07",56),
           rep("CA08",56),rep("CA09",56),rep("CA10",56))
#names(res) <- c("CAMPO")
data_new <- cbind(data_new,res,campo)
data_new$res <- factor(data_new$res, levels=c(1,2),labels=c("NO","SI"))


ggplot(data_new, aes(x=res, y=Formaci.n.posgrado,fill=as.factor(res))) + 
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 4.5) + 
  stat_boxplot(geom ='errorbar') + labs(x="Educación")+labs(y="INDICADOR") + labs(fill="Educación") + 
  facet_grid(. ~ campo)

ggplot(data, aes(x=res, y=Formaci.n.posgrado,fill=as.factor(res))) + 
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 4.5) + 
  stat_boxplot(geom ='errorbar') + labs(x="Educación")+labs(y="INDICADOR") + labs(fill="Educación") + 
  facet_grid(. ~ campo)



# CAMPO EDUCACION
ggplot(data, aes(x=CA01)) + geom_histogram(binwidth=3) + geom_bar(fill=c("gold","green3"), colour="black") +
  labs(x=" ") + labs(y="CASOS") + labs(title="Campo Educación") + theme(text = element_text(size = 16))

# CAMPO ARTE/HUMANIDADES
ggplot(data, aes(x=CA02)) + geom_histogram(binwidth=3) + geom_bar(fill=c("gold","green3"), colour="black") +
  labs(x=" ") + labs(y="CASOS") + labs(title="Campo Arte y humanidades") + theme(text = element_text(size = 16))

# CAMPO CIENCIAS SOCIALES
ggplot(data, aes(x=CA03)) + geom_histogram(binwidth=3) + geom_bar(fill=c("gold","green3"), colour="black") +
  labs(x=" ") + labs(y="CASOS") + labs(title="Campo Ciencias Sociales") + theme(text = element_text(size = 16))

# CAMPO CIENCIAS
ggplot(data, aes(x=CA05)) + geom_histogram(binwidth=3) + geom_bar(fill=c("gold","green3"), colour="black") +
  labs(x=" ") + labs(y="CASOS") + labs(title="Campo Ciencias") + theme(text = element_text(size = 16))

# CAMPO INGENIERIA
ggplot(data, aes(x=CA07)) + geom_histogram(binwidth=3) + geom_bar(fill=c("gold","green3"), colour="black") +
  labs(x=" ") + labs(y="CASOS") + labs(title="Campo Ingeniería") + theme(text = element_text(size = 16))

# CAMPO AGRICULTURA
ggplot(data, aes(x=CA08)) + geom_histogram(binwidth=3) + geom_bar(fill=c("gold","green3"), colour="black") +
  labs(x=" ") + labs(y="CASOS") + labs(title="Campo Agricultura") + theme(text = element_text(size = 16))

# CAMPO SALUD
ggplot(data, aes(x=CA09)) + geom_histogram(binwidth=3) + geom_bar(fill=c("gold","green3"), colour="black") +
  labs(x=" ") + labs(y="CASOS") + labs(title="Campo Salud") + theme(text = element_text(size = 16))

# CAMPO SERVICIOS
ggplot(data, aes(x=CA10)) + geom_histogram(binwidth=3) + geom_bar(fill=c("gold","green3"), colour="black") +
  labs(x=" ") + labs(y="CASOS") + labs(title="Campo Servicios") + theme(text = element_text(size = 16))


# CRITERIO ACADEMIA - CAMPO EDUCACION
# Formaci?n Posgrado
p1 <- ggplot(data, aes(x=CA01, y=Formaci.n.posgrado, fill=as.factor(CA01))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Formación Posgrado")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))
# Posgrado en formaci?n
p2 <- ggplot(data, aes(x=CA01, y=Posgrado.en.formaci.n, fill=as.factor(CA01))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Posgrado en formación")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))
# Doctores TC
p3 <- ggplot(data, aes(x=CA01, y=Doctores.TC, fill=as.factor(CA01))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Doctores tiempo completo")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))
# Estudiantes por docente TC
p4 <- ggplot(data, aes(x=CA01, y=Estudiantes.por.docente.TC, fill=as.factor(CA01))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Estudiantes por docente TC")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))
# Horas clase TC
p5 <- ggplot(data, aes(x=CA01, y=Horas.clase.TC, fill=as.factor(CA01))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Horas clase TC")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))
# Horas clase TC
p6 <- ggplot(data, aes(x=CA01, y=Porcentaje.de.profesores.TC, fill=as.factor(CA01))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Porcentaje profesores TC")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))
# Hora clase MT/TP
p7 <- ggplot(data, aes(x=CA01, y=Horas.clase.MT.TP, fill=as.factor(CA01))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Horas clase MT/TP")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))
# Titularidad
p8 <- ggplot(data, aes(x=CA01, y=Titularidad, fill=as.factor(CA01))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Titularidad")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))
# Titularidad TC
p9 <- ggplot(data, aes(x=CA01, y=Titularidad.TC, fill=as.factor(CA01))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Titularidad TC")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))
# Concurso
p10 <- ggplot(data, aes(x=CA01, y=Concurso, fill=as.factor(CA01))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Concurso")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))
# Direccion mujeres
p11 <- ggplot(data, aes(x=CA01, y=Direcci.n.mujeres, fill=as.factor(CA01))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Dirección mujeres")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))
# Docencia mujeres
p12 <- ggplot(data, aes(x=CA01, y=Docencia.mujeres, fill=as.factor(CA01))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Docencia mujeres")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))

multiplot(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, cols=4)



# Eficiencia Académica
# Eficiencia terminal posgrado
p1 <- ggplot(data, aes(x=CA01, y=Eficiencia.terminal.posgrado, fill=as.factor(CA01))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Eficiencia terminal posgrado")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))
# Eficiencia terminal pregrado
p2 <- ggplot(data, aes(x=CA01, y=Eficiencia.terminal.pregrado, fill=as.factor(CA01))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Eficiencia terminal pregrado")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))
# Tasa de retencion inicial pregrado
p3 <- ggplot(data, aes(x=CA01, y=Tasa.de.retenci.n.inicial.pregrado, fill=as.factor(CA01))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Tasa retencion inicial pregrado")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))
# Admision a estudios de pregrado
p4 <- ggplot(data, aes(x=Admisi.n.a.estudios.de.pregrado)) + geom_histogram(binwidth=3) + geom_bar(fill=c("orange","gold","green3"), colour="black") +
  labs(x=" ") + labs(y="CASOS") + labs(title="Admisión estudios pregrado") + theme(text = element_text(size = 16))+
  facet_grid(CA01 ~ .)
# Admision a estudios de posgrado
p5 <- ggplot(data, aes(x=data$Admisi.n.a.estudios.de.posgrado)) + geom_histogram(binwidth=3) + geom_bar(fill=c("orange","gold","green3"), colour="black") +
  labs(x=" ") + labs(y="CASOS") + labs(title="Admisión estudios posgrado") + theme(text = element_text(size = 16))+
  facet_grid(CA01 ~ .)

multiplot(p1, p2, p3, p4, p5, cols=3)

data$Admisi.n.a.estudios.de.pregrado <- ifelse(data$Admisi.n.a.estudios.de.pregrado=="BAJO",1,
                                               ifelse(data$Admisi.n.a.estudios.de.pregrado=="MEDIO",2,3))
data$Admisi.n.a.estudios.de.pregrado <- factor(data$Admisi.n.a.estudios.de.pregrado,levels=c(1,2,3),
                                               labels=c("BAJO","MEDIO","ALTO"))

data$Admisi.n.a.estudios.de.posgrado <- ifelse(data$Admisi.n.a.estudios.de.posgrado=="BAJO",1,
                                               ifelse(data$Admisi.n.a.estudios.de.posgrado=="MEDIO",2,3))
data$Admisi.n.a.estudios.de.posgrado <- factor(data$Admisi.n.a.estudios.de.posgrado,levels=c(1,2,3),
                                               labels=c("BAJO","MEDIO","ALTO"))

# Eficiencia Investigación
# Investigación regional
p1 <- ggplot(data, aes(x=CA01, y=Investigaci.n.regional, fill=as.factor(CA01))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Investigación regional")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))
# Producción científica
p2 <- ggplot(data, aes(x=CA01, y=Producci.n.cient.fica, fill=as.factor(CA01))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Producción científica")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))
# Libros revisados por pares
p3 <- ggplot(data, aes(x=CA01, y=Libros.revisados.por.pares, fill=as.factor(CA01))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Libros revisados por pares")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))
# Planificacion de la investigación
p4 <- ggplot(data, aes(x=Planificaci.n.de.la.investigaci.n)) + geom_histogram(binwidth=3) + geom_bar(fill=c("orange","gold","green3"), colour="black") +
  labs(x=" ") + labs(y="CASOS") + labs(title="Planificación de la investigación") + theme(text = element_text(size = 16))+
  facet_grid(CA01 ~ .)

multiplot(p1, p2, p3, cols=3)

data$Planificaci.n.de.la.investigaci.n <- ifelse(data$Planificaci.n.de.la.investigaci.n=="BAJO",1,
                                                 ifelse(data$Planificaci.n.de.la.investigaci.n=="MEDIO",2,3))
data$Planificaci.n.de.la.investigaci.n <- factor(data$Planificaci.n.de.la.investigaci.n,levels=c(1,2,3),
                                                 labels=c("BAJO","MEDIO","ALTO"))


# Eficiencia Organización
# Programas de vinculacion
p1 <- ggplot(data, aes(x=CA01, y=Programas.de.vinculaci.n, fill=as.factor(CA01))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Programas de vinculación")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))
# Presupuesto programas vinculacion
p2 <- ggplot(data, aes(x=CA01, y=Presupuesto.de.programas.de.vinculaci.n, fill=as.factor(CA01))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Presupuesto programas vinculación")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))
# calidad del gasto
p3 <- ggplot(data, aes(x=CA01, y=Calidad.del.gasto, fill=as.factor(CA01))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Calidad del gasto")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))

multiplot(p1, p2, p3, cols=3)



# Eficiencia Infraestructura
# Espacios para estudiantes
p1 <- ggplot(data, aes(x=CA01, y=Espacio.para.estudiantes, fill=as.factor(CA01))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Espacios para estudiantes")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))
# Titulos de libros 
p2 <- ggplot(data, aes(x=CA01, y=T.tulos.de.libros, fill=as.factor(CA01))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Libros")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))
# Consultas por usuario
p3 <- ggplot(data, aes(x=CA01, y=Consultas.por.usuario, fill=as.factor(CA01))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Consultas por usuario")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))
# Conectividad
p4 <- ggplot(data, aes(x=CA01, y=Conectividad, fill=as.factor(CA01))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Conectividad")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))
# Cobertura a estudiantes
p5 <- ggplot(data, aes(x=CA01, y=Cobertura.a.estudiantes, fill=as.factor(CA01))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Cobertura a estudiantes")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))
# Calidad de aulas
p6 <- ggplot(data, aes(x=CA01, y=Calidad.de.aulas, fill=as.factor(CA01))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Calidad de aulas")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))
# Oficinas TC
p7 <- ggplot(data, aes(x=CA01, y=Oficinas.TC, fill=as.factor(CA01))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Oficinas TC")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))
# Oficinas MT/TP
p8 <- ggplot(data, aes(x=CA01, y=Oficinas.MT.TP, fill=as.factor(CA01))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Oficinas MT/TP")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))

multiplot(p1, p2, p3, p4, p5, p6, p7, p8, cols=4)


### Tablas de contingencia

table(data$Escalaf.n,data$mantenimiento)
table(data$Evaluaci.n,data$mantenimiento)
table(data$Admisi.n.a.estudios.de.pregrado,data$mantenimiento)
table(data$Admisi.n.a.estudios.de.posgrado,data$mantenimiento)
table(data$Planificaci.n.de.la.investigaci.n,data$mantenimiento)
table(data$Uso.del.seguimiento.a.graduados,data$mantenimiento)
table(data$Rendici.n.anual.de.cuentas,data$mantenimiento)
table(data$Transparencia.financiera,data$mantenimiento)
table(data$X.tica.y.responsabilidad,data$mantenimiento)
table(data$Informaci.n.para.la.evaluaci.n,data$mantenimiento)
table(data$R.gimen.acad.mico,data$mantenimiento)
table(data$Acci.n.afirmativa,data$mantenimiento)
table(data$Gesti.n.de.biblioteca,data$mantenimiento)
table(data$Innovaci.n.tecnol.gica,data$mantenimiento)
table(data$Espacios.de.bienestar,data$mantenimiento)