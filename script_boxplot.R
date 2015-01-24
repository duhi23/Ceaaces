#### Graficos Finales en ggplot2 ####

#Formación posgrado
mypath <- file.path("C:","Users","SONY","Dropbox","Proyectos","Ceaaces","Graficos",
                    paste("formacion_posgrado", ".png", sep = ""))
png(file=mypath)
ggplot(data, aes(x=mantenimiento, y=Formaci.n.posgrado, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Formación Posgrado")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))+
  geom_text(data = aggregate(Formaci.n.posgrado ~ mantenimiento , data, function(i) round(median(i),2)), 
  aes(label = Formaci.n.posgrado,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)
dev.off()

# Posgrado en formación
mypath <- file.path("C:","Users","SONY","Dropbox","Proyectos","Ceaaces","Graficos",
                    paste("posgrado_en_formacion", ".png", sep = ""))
png(file=mypath)
ggplot(data, aes(x=mantenimiento, y=Posgrado.en.formaci.n, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Posgrado en formación")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))+
  geom_text(data = aggregate(Posgrado.en.formaci.n ~ mantenimiento , data, function(i) round(median(i),2)), 
  aes(label = Posgrado.en.formaci.n,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)
dev.off()

#Doctores Tiempo Completo
mypath <- file.path("C:","Users","SONY","Dropbox","Proyectos","Ceaaces","Graficos",
                    paste("doctores_TC", ".png", sep = ""))
png(file=mypath)
ggplot(data, aes(x=mantenimiento, y=Doctores.TC, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Doctores TC")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))+
  geom_text(data = aggregate(Doctores.TC ~ mantenimiento , data, function(i) round(median(i),2)), 
  aes(label = Doctores.TC,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)
dev.off()

#Estudiantes por Docente TC
mypath <- file.path("C:","Users","SONY","Dropbox","Proyectos","Ceaaces","Graficos",
                    paste("estudiantes_docentes_TC", ".png", sep = ""))
png(file=mypath)
ggplot(data, aes(x=mantenimiento, y=Estudiantes.por.docente.TC, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Estudiantes por docentes TC")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))+
  geom_text(data = aggregate(Estudiantes.por.docente.TC ~ mantenimiento , data, function(i) round(median(i),2)), 
  aes(label = Estudiantes.por.docente.TC,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)
dev.off()

#Horas clase TC
mypath <- file.path("C:","Users","SONY","Dropbox","Proyectos","Ceaaces","Graficos",
                    paste("horas_clase_TC", ".png", sep = ""))
png(file=mypath)
ggplot(data, aes(x=mantenimiento, y=Horas.clase.TC, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Horas clase TC")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))+
  geom_text(data = aggregate(Horas.clase.TC ~ mantenimiento , data, function(i) round(median(i),2)), 
  aes(label = Horas.clase.TC,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)
dev.off()

#Porcentaje Profesores TC
mypath <- file.path("C:","Users","SONY","Dropbox","Proyectos","Ceaaces","Graficos",
                    paste("porcentaje_profesores_TC", ".png", sep = ""))
png(file=mypath)
ggplot(data, aes(x=mantenimiento, y=Porcentaje.de.profesores.TC, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Porcentaje Profesores TC")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))+
  geom_text(data = aggregate(Porcentaje.de.profesores.TC ~ mantenimiento , data, function(i) round(median(i),2)), 
  aes(label = Porcentaje.de.profesores.TC,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)
dev.off()

#Horas clase MT/TP
mypath <- file.path("C:","Users","SONY","Dropbox","Proyectos","Ceaaces","Graficos",
                    paste("horas_Clase_MT_TP", ".png", sep = ""))
png(file=mypath)
ggplot(data, aes(x=mantenimiento, y=Horas.clase.MT.TP, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Horas clase MT/TP")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))+
  geom_text(data = aggregate(Horas.clase.MT.TP ~ mantenimiento , data, function(i) round(median(i),2)), 
  aes(label = Horas.clase.MT.TP,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)
dev.off()

#Titularidad
mypath <- file.path("C:","Users","SONY","Dropbox","Proyectos","Ceaaces","Graficos",
                    paste("titularidad", ".png", sep = ""))
png(file=mypath)
ggplot(data, aes(x=mantenimiento, y=Titularidad, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Titularidad")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))+
  geom_text(data = aggregate(Titularidad ~ mantenimiento , data, function(i) round(median(i),2)), 
  aes(label = Titularidad,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)
dev.off()

#Titularidad TC
mypath <- file.path("C:","Users","SONY","Dropbox","Proyectos","Ceaaces","Graficos",
                    paste("titularidad_TC", ".png", sep = ""))
png(file=mypath)
ggplot(data, aes(x=mantenimiento, y=Titularidad.TC, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Titularidad TC")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))+
  geom_text(data = aggregate(Titularidad.TC ~ mantenimiento , data, function(i) round(median(i),2)), 
  aes(label = Titularidad.TC,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)
dev.off()

#Concurso
mypath <- file.path("C:","Users","SONY","Dropbox","Proyectos","Ceaaces","Graficos",
                    paste("Concurso", ".png", sep = ""))
png(file=mypath)
ggplot(data, aes(x=mantenimiento, y=Concurso, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Concurso")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))+
  geom_text(data = aggregate(Concurso ~ mantenimiento , data, function(i) round(median(i),2)), 
  aes(label = Concurso,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)
dev.off()

#Remuneracion TC   
mypath <- file.path("C:","Users","SONY","Dropbox","Proyectos","Ceaaces","Graficos",
                    paste("remuneracion_TC", ".png", sep = ""))
png(file=mypath)
ggplot(data, aes(x=mantenimiento, y=Remuneraci.n.TC, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Remuneración TC")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))+
  geom_text(data = aggregate(Remuneraci.n.TC ~ mantenimiento , data, function(i) round(median(i),2)), 
  aes(label=Remuneraci.n.TC,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)
dev.off()

#Remuneracion MT/TP
mypath <- file.path("C:","Users","SONY","Dropbox","Proyectos","Ceaaces","Graficos",
                    paste("remuneracion_MT_TP", ".png", sep = ""))
png(file=mypath)
ggplot(data, aes(x=mantenimiento, y=Remuneraci.n.MT.TP, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Remuneración MT/TP")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))+
  geom_text(data = aggregate(Remuneraci.n.MT.TP ~ mantenimiento , data, function(i) round(median(i),2)), 
            aes(label=Remuneraci.n.MT.TP,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)
dev.off()

#Dirección mujeres
mypath <- file.path("C:","Users","SONY","Dropbox","Proyectos","Ceaaces","Graficos",
                    paste("direccion_mujeres", ".png", sep = ""))
png(file=mypath)
ggplot(data, aes(x=mantenimiento, y=Direcci.n.mujeres, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Dirección mujeres")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))+
  geom_text(data = aggregate(Direcci.n.mujeres ~ mantenimiento , data, function(i) round(median(i),2)), 
  aes(label=Direcci.n.mujeres,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)
dev.off()

#Docencia mujeres
mypath <- file.path("C:","Users","SONY","Dropbox","Proyectos","Ceaaces","Graficos",
                    paste("docencia_mujeres", ".png", sep = ""))
png(file=mypath)
ggplot(data, aes(x=mantenimiento, y=Docencia.mujeres, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Docencia mujeres")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))+
  geom_text(data = aggregate(Docencia.mujeres ~ mantenimiento , data, function(i) round(median(i),2)), 
            aes(label=Docencia.mujeres,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)
dev.off()

#Eficiencia terminal posgrado
mypath <- file.path("C:","Users","SONY","Dropbox","Proyectos","Ceaaces","Graficos",
                    paste("eficiencia_terminal_posgrado", ".png", sep = ""))
png(file=mypath)
ggplot(data, aes(x=mantenimiento, y=Eficiencia.terminal.posgrado, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Eficiencia terminal Posgrado")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))+
  geom_text(data = aggregate(Eficiencia.terminal.posgrado ~ mantenimiento , data, function(i) round(median(i),2)), 
  aes(label=Eficiencia.terminal.posgrado,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)
dev.off()

#Eficiencia terminal pregrado
mypath <- file.path("C:","Users","SONY","Dropbox","Proyectos","Ceaaces","Graficos",
                    paste("eficiencia_terminal_pregrado", ".png", sep = ""))
png(file=mypath)
ggplot(data, aes(x=mantenimiento, y=Eficiencia.terminal.pregrado, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Eficiencia terminal Pregrado")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))+
  geom_text(data = aggregate(Eficiencia.terminal.pregrado ~ mantenimiento , data, function(i) round(median(i),2)), 
  aes(label=Eficiencia.terminal.pregrado,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)
dev.off()

#Tasa retención inicial pregrado
mypath <- file.path("C:","Users","SONY","Dropbox","Proyectos","Ceaaces","Graficos",
                    paste("tasa_retencion_inicial_pregrado", ".png", sep = ""))
png(file=mypath)
ggplot(data, aes(x=mantenimiento, y=Tasa.de.retenci.n.inicial.pregrado, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Tasa retención inicial Pregrado")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))+
  geom_text(data = aggregate(Tasa.de.retenci.n.inicial.pregrado ~ mantenimiento , data, function(i) round(median(i),2)), 
  aes(label=Tasa.de.retenci.n.inicial.pregrado,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)
dev.off()

# Investigación regional
mypath <- file.path("C:","Users","SONY","Dropbox","Proyectos","Ceaaces","Graficos",
                    paste("investigacion_regional", ".png", sep = ""))
png(file=mypath)
ggplot(data, aes(x=mantenimiento, y=Investigaci.n.regional, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Investigación regional")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))+
  geom_text(data = aggregate(Investigaci.n.regional ~ mantenimiento , data, function(i) round(median(i),2)), 
  aes(label=Investigaci.n.regional,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)
dev.off()

#Producción científica
mypath <- file.path("C:","Users","SONY","Dropbox","Proyectos","Ceaaces","Graficos",
                    paste("produccion_cientifica", ".png", sep = ""))
png(file=mypath)
ggplot(data, aes(x=mantenimiento, y=Producci.n.cient.fica, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Producción científica")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))+
  geom_text(data = aggregate(Producci.n.cient.fica ~ mantenimiento , data, function(i) round(median(i),2)), 
            aes(label=Producci.n.cient.fica,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)
dev.off()

#Libros revisados por pares
mypath <- file.path("C:","Users","SONY","Dropbox","Proyectos","Ceaaces","Graficos",
                    paste("libros_revisados_pares", ".png", sep = ""))
png(file=mypath)
ggplot(data, aes(x=mantenimiento, y=Libros.revisados.por.pares, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Libros revisados por pares")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))+
  geom_text(data = aggregate(Libros.revisados.por.pares ~ mantenimiento , data, function(i) round(median(i),2)), 
  aes(label=Libros.revisados.por.pares,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)
dev.off()

#Programas de vinculación
mypath <- file.path("C:","Users","SONY","Dropbox","Proyectos","Ceaaces","Graficos",
                    paste("programas_vinculacion", ".png", sep = ""))
png(file=mypath)
ggplot(data, aes(x=mantenimiento, y=Programas.de.vinculaci.n, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Programas de vinculación")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))+
  geom_text(data = aggregate(Programas.de.vinculaci.n ~ mantenimiento , data, function(i) round(median(i),2)), 
            aes(label=Programas.de.vinculaci.n,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)
dev.off()

#Presupuesto programas vinculacion
mypath <- file.path("C:","Users","SONY","Dropbox","Proyectos","Ceaaces","Graficos",
                    paste("presupuesto_programas_vinculacion", ".png", sep = ""))
png(file=mypath)
ggplot(data, aes(x=mantenimiento, y=Presupuesto.de.programas.de.vinculaci.n, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Presupuesto programas vinculación")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))+
  geom_text(data = aggregate(Presupuesto.de.programas.de.vinculaci.n ~ mantenimiento , data, function(i) round(median(i),2)), 
  aes(label=Presupuesto.de.programas.de.vinculaci.n,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)
dev.off()

#Calidad del gasto
mypath <- file.path("C:","Users","SONY","Dropbox","Proyectos","Ceaaces","Graficos",
                    paste("calidad_gasto", ".png", sep = ""))
png(file=mypath)
ggplot(data, aes(x=mantenimiento, y=Calidad.del.gasto, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Presupuesto programas vinculación")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))+
  geom_text(data = aggregate(Calidad.del.gasto ~ mantenimiento , data, function(i) round(median(i),2)), 
  aes(label=Calidad.del.gasto,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)
dev.off()

#Espacio para estudiantes
mypath <- file.path("C:","Users","SONY","Dropbox","Proyectos","Ceaaces","Graficos",
                    paste("espacio_estudiantes", ".png", sep = ""))
png(file=mypath)
ggplot(data, aes(x=mantenimiento, y=Espacio.para.estudiantes, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Espacio para estudiantes")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))+
  geom_text(data = aggregate(Espacio.para.estudiantes ~ mantenimiento , data, function(i) round(median(i),2)), 
            aes(label=Espacio.para.estudiantes,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)
dev.off()

#Titulos de libros
mypath <- file.path("C:","Users","SONY","Dropbox","Proyectos","Ceaaces","Graficos",
                    paste("titulos_libros", ".png", sep = ""))
png(file=mypath)
ggplot(data, aes(x=mantenimiento, y=T.tulos.de.libros, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Títulos de libros")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))+
  geom_text(data = aggregate(T.tulos.de.libros ~ mantenimiento , data, function(i) round(median(i),2)), 
  aes(label=T.tulos.de.libros,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)
dev.off()

#Consultas por usuarios
mypath <- file.path("C:","Users","SONY","Dropbox","Proyectos","Ceaaces","Graficos",
                    paste("consultas_usuario", ".png", sep = ""))
png(file=mypath)
ggplot(data, aes(x=mantenimiento, y=Consultas.por.usuario, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Consultas por usuarios")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))+
  geom_text(data = aggregate(Consultas.por.usuario ~ mantenimiento , data, function(i) round(median(i),2)), 
            aes(label=Consultas.por.usuario,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)
dev.off()

#Conectividad
mypath <- file.path("C:","Users","SONY","Dropbox","Proyectos","Ceaaces","Graficos",
                    paste("conectividad", ".png", sep = ""))
png(file=mypath)
ggplot(data, aes(x=mantenimiento, y=Conectividad, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Conectividad")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))+
  geom_text(data = aggregate(Conectividad ~ mantenimiento , data, function(i) round(median(i),2)), 
            aes(label=Conectividad,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)
dev.off()

#Cobertura estudiantes
mypath <- file.path("C:","Users","SONY","Dropbox","Proyectos","Ceaaces","Graficos",
                    paste("cobertura_estudiantes", ".png", sep = ""))
png(file=mypath)
ggplot(data, aes(x=mantenimiento, y=Cobertura.a.estudiantes, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Cobertura a estudiantes")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))+
  geom_text(data = aggregate(Cobertura.a.estudiantes ~ mantenimiento , data, function(i) round(median(i),2)), 
            aes(label=Cobertura.a.estudiantes,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)
dev.off()

#Calidad aulas
mypath <- file.path("C:","Users","SONY","Dropbox","Proyectos","Ceaaces","Graficos",
                    paste("calidad_aulas", ".png", sep = ""))
png(file=mypath)
ggplot(data, aes(x=mantenimiento, y=Calidad.de.aulas, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Calidad de aulas")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))+
  geom_text(data = aggregate(Calidad.de.aulas ~ mantenimiento , data, function(i) round(median(i),2)), 
            aes(label=Calidad.de.aulas,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)
dev.off()

#Oficinas TC
mypath <- file.path("C:","Users","SONY","Dropbox","Proyectos","Ceaaces","Graficos",
                    paste("oficinas_TC", ".png", sep = ""))
png(file=mypath)
ggplot(data, aes(x=mantenimiento, y=Oficinas.TC, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Oficinas TC")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))+
  geom_text(data = aggregate(Oficinas.TC ~ mantenimiento , data, function(i) round(median(i),2)), 
            aes(label=Oficinas.TC,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)
dev.off()

#Oficinas MT/TP
mypath <- file.path("C:","Users","SONY","Dropbox","Proyectos","Ceaaces","Graficos",
                    paste("oficinas_MT_TP", ".png", sep = ""))
png(file=mypath)
ggplot(data, aes(x=mantenimiento, y=Oficinas.MT.TP, fill=as.factor(mantenimiento))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Oficinas MT/TP")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))+
  geom_text(data = aggregate(Oficinas.MT.TP ~ mantenimiento , data, function(i) round(median(i),2)), 
            aes(label=Oficinas.MT.TP,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)
dev.off()


### Análisis de las áreas según clasificación CINE 1997

# Área Educación
ggplot(data, aes(x=CA01, y=Producci.n.cient.fica, fill=as.factor(CA01))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Producción Científica")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15)) +
  geom_text(data = aggregate(Producci.n.cient.fica ~ CA01 , data, function(i) round(median(i),2)), 
  aes(label=Producci.n.cient.fica,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)

ggplot(data, aes(x=CA01, y=Formaci.n.posgrado, fill=as.factor(CA01))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Formación posgrado")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15)) +
  geom_text(data = aggregate(Formaci.n.posgrado ~ CA01 , data, function(i) round(median(i),2)), 
  aes(label=Formaci.n.posgrado,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)

ggplot(data, aes(x=CA01, y=Remuneraci.n.TC, fill=as.factor(CA01))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Remuneración TC")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15)) +
  geom_text(data = aggregate(Remuneraci.n.TC ~ CA01 , data, function(i) round(median(i),2)), 
  aes(label=Remuneraci.n.TC,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)

ggplot(data, aes(x=CA01, y=Doctores.TC, fill=as.factor(CA01))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Doctores TC")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15)) +
  geom_text(data = aggregate(Doctores.TC ~ CA01 , data, function(i) round(median(i),2)), 
  aes(label=Doctores.TC,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)

ggplot(data, aes(x=CA01, y=Libros.revisados.por.pares, fill=as.factor(CA01))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Libros revisados por pares")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15)) +
  geom_text(data = aggregate(Libros.revisados.por.pares ~ CA01 , data, function(i) round(median(i),2)), 
  aes(label=Libros.revisados.por.pares,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)

# Artes y humanidades
ggplot(data, aes(x=CA02, y=Producci.n.cient.fica, fill=as.factor(CA02))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Producción Científica")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15)) +
  geom_text(data = aggregate(Producci.n.cient.fica ~ CA02 , data, function(i) round(median(i),2)), 
  aes(label=Producci.n.cient.fica,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)

ggplot(data, aes(x=CA02, y=Formaci.n.posgrado, fill=as.factor(CA02))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Formación posgrado")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15)) +
  geom_text(data = aggregate(Formaci.n.posgrado ~ CA02 , data, function(i) round(median(i),2)), 
  aes(label=Formaci.n.posgrado,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)

ggplot(data, aes(x=CA02, y=Remuneraci.n.TC, fill=as.factor(CA02))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Remuneración TC")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15)) +
  geom_text(data = aggregate(Remuneraci.n.TC ~ CA02 , data, function(i) round(median(i),2)), 
  aes(label=Remuneraci.n.TC,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)

ggplot(data, aes(x=CA02, y=Doctores.TC, fill=as.factor(CA02))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Doctores TC")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15)) +
  geom_text(data = aggregate(Doctores.TC ~ CA02 , data, function(i) round(median(i),2)), 
  aes(label=Doctores.TC,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)

ggplot(data, aes(x=CA02, y=Libros.revisados.por.pares, fill=as.factor(CA02))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Libros revisados por pares")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15)) +
  geom_text(data = aggregate(Libros.revisados.por.pares ~ CA02 , data, function(i) round(median(i),2)), 
  aes(label=Libros.revisados.por.pares,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)

# Ciencias Sociales
ggplot(data, aes(x=CA03, y=Producci.n.cient.fica, fill=as.factor(CA03))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Producción Científica")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15)) +
  geom_text(data = aggregate(Producci.n.cient.fica ~ CA03 , data, function(i) round(median(i),2)), 
  aes(label=Producci.n.cient.fica,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)

ggplot(data, aes(x=CA03, y=Formaci.n.posgrado, fill=as.factor(CA03))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Formación posgrado")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15)) +
  geom_text(data = aggregate(Formaci.n.posgrado ~ CA03 , data, function(i) round(median(i),2)), 
  aes(label=Formaci.n.posgrado,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)

ggplot(data, aes(x=CA03, y=Remuneraci.n.TC, fill=as.factor(CA03))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Remuneración TC")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15)) +
  geom_text(data = aggregate(Remuneraci.n.TC ~ CA03 , data, function(i) round(median(i),2)), 
  aes(label=Remuneraci.n.TC,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)

ggplot(data, aes(x=CA03, y=Doctores.TC, fill=as.factor(CA03))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Doctores TC")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15)) +
  geom_text(data = aggregate(Doctores.TC ~ CA03 , data, function(i) round(median(i),2)), 
  aes(label=Doctores.TC,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)

ggplot(data, aes(x=CA03, y=Libros.revisados.por.pares, fill=as.factor(CA03))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Libros revisados por pares")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15)) +
  geom_text(data = aggregate(Libros.revisados.por.pares ~ CA03 , data, function(i) round(median(i),2)), 
  aes(label=Libros.revisados.por.pares,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)


# Ciencias
ggplot(data, aes(x=CA05, y=Producci.n.cient.fica, fill=as.factor(CA05))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Producción Científica")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15)) +
  geom_text(data = aggregate(Producci.n.cient.fica ~ CA05 , data, function(i) round(median(i),2)), 
  aes(label=Producci.n.cient.fica,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)

ggplot(data, aes(x=CA05, y=Formaci.n.posgrado, fill=as.factor(CA05))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Formación posgrado")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15)) +
  geom_text(data = aggregate(Formaci.n.posgrado ~ CA05 , data, function(i) round(median(i),2)), 
  aes(label=Formaci.n.posgrado,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)

ggplot(data, aes(x=CA05, y=Remuneraci.n.TC, fill=as.factor(CA05))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Remuneración TC")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15)) +
  geom_text(data = aggregate(Remuneraci.n.TC ~ CA05 , data, function(i) round(median(i),2)), 
  aes(label=Remuneraci.n.TC,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)

ggplot(data, aes(x=CA05, y=Doctores.TC, fill=as.factor(CA05))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Doctores TC")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15)) +
  geom_text(data = aggregate(Doctores.TC ~ CA05 , data, function(i) round(median(i),2)), 
  aes(label=Doctores.TC,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)

ggplot(data, aes(x=CA05, y=Libros.revisados.por.pares, fill=as.factor(CA05))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Libros revisados por pares")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15)) +
  geom_text(data = aggregate(Libros.revisados.por.pares ~ CA05 , data, function(i) round(median(i),2)), 
  aes(label=Libros.revisados.por.pares,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)

# Ingeniería
ggplot(data, aes(x=CA07, y=Producci.n.cient.fica, fill=as.factor(CA07))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Producción Científica")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15)) +
  geom_text(data = aggregate(Producci.n.cient.fica ~ CA07 , data, function(i) round(median(i),2)), 
  aes(label=Producci.n.cient.fica,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)

ggplot(data, aes(x=CA07, y=Formaci.n.posgrado, fill=as.factor(CA07))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Formación posgrado")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15)) +
  geom_text(data = aggregate(Formaci.n.posgrado ~ CA07 , data, function(i) round(median(i),2)), 
  aes(label=Formaci.n.posgrado,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)

ggplot(data, aes(x=CA07, y=Remuneraci.n.TC, fill=as.factor(CA07))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Remuneración TC")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15)) +
  geom_text(data = aggregate(Remuneraci.n.TC ~ CA07 , data, function(i) round(median(i),2)), 
  aes(label=Remuneraci.n.TC,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)

ggplot(data, aes(x=CA07, y=Doctores.TC, fill=as.factor(CA07))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Doctores TC")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15)) +
  geom_text(data = aggregate(Doctores.TC ~ CA07 , data, function(i) round(median(i),2)), 
  aes(label=Doctores.TC,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)

ggplot(data, aes(x=CA07, y=Libros.revisados.por.pares, fill=as.factor(CA07))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Libros revisados por pares")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15)) +
  geom_text(data = aggregate(Libros.revisados.por.pares ~ CA07 , data, function(i) round(median(i),2)), 
  aes(label=Libros.revisados.por.pares,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)

#Agricultura
ggplot(data, aes(x=CA08, y=Producci.n.cient.fica, fill=as.factor(CA08))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Producción Científica")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15)) +
  geom_text(data = aggregate(Producci.n.cient.fica ~ CA08 , data, function(i) round(median(i),2)), 
  aes(label=Producci.n.cient.fica,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)

ggplot(data, aes(x=CA08, y=Formaci.n.posgrado, fill=as.factor(CA08))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Formación posgrado")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15)) +
  geom_text(data = aggregate(Formaci.n.posgrado ~ CA08 , data, function(i) round(median(i),2)), 
  aes(label=Formaci.n.posgrado,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)

ggplot(data, aes(x=CA08, y=Remuneraci.n.TC, fill=as.factor(CA08))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Remuneración TC")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15)) +
  geom_text(data = aggregate(Remuneraci.n.TC ~ CA08 , data, function(i) round(median(i),2)), 
  aes(label=Remuneraci.n.TC,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)

ggplot(data, aes(x=CA08, y=Doctores.TC, fill=as.factor(CA08))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Doctores TC")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15)) +
  geom_text(data = aggregate(Doctores.TC ~ CA08 , data, function(i) round(median(i),2)), 
  aes(label=Doctores.TC,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)

ggplot(data, aes(x=CA08, y=Libros.revisados.por.pares, fill=as.factor(CA08))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Libros revisados por pares")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15)) +
  geom_text(data = aggregate(Libros.revisados.por.pares ~ CA08 , data, function(i) round(median(i),2)), 
  aes(label=Libros.revisados.por.pares,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)

# Salud
ggplot(data, aes(x=CA09, y=Producci.n.cient.fica, fill=as.factor(CA09))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Producción Científica")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15)) +
  geom_text(data = aggregate(Producci.n.cient.fica ~ CA09 , data, function(i) round(median(i),2)), 
  aes(label=Producci.n.cient.fica,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)

ggplot(data, aes(x=CA09, y=Formaci.n.posgrado, fill=as.factor(CA09))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Formación posgrado")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15)) +
  geom_text(data = aggregate(Formaci.n.posgrado ~ CA09 , data, function(i) round(median(i),2)), 
  aes(label=Formaci.n.posgrado,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)

ggplot(data, aes(x=CA09, y=Remuneraci.n.TC, fill=as.factor(CA09))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Remuneración TC")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15)) +
  geom_text(data = aggregate(Remuneraci.n.TC ~ CA09 , data, function(i) round(median(i),2)), 
  aes(label=Remuneraci.n.TC,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)

ggplot(data, aes(x=CA09, y=Doctores.TC, fill=as.factor(CA09))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Doctores TC")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15)) +
  geom_text(data = aggregate(Doctores.TC ~ CA09 , data, function(i) round(median(i),2)), 
  aes(label=Doctores.TC,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)

ggplot(data, aes(x=CA09, y=Libros.revisados.por.pares, fill=as.factor(CA09))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Libros revisados por pares")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15)) +
  geom_text(data = aggregate(Libros.revisados.por.pares ~ CA09 , data, function(i) round(median(i),2)), 
  aes(label=Libros.revisados.por.pares,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)


# Servicios
ggplot(data, aes(x=CA10, y=Producci.n.cient.fica, fill=as.factor(CA10))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Producción Científica")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15)) +
  geom_text(data = aggregate(Producci.n.cient.fica ~ CA10 , data, function(i) round(median(i),2)), 
  aes(label=Producci.n.cient.fica,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)

ggplot(data, aes(x=CA10, y=Formaci.n.posgrado, fill=as.factor(CA10))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Formación posgrado")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15)) +
  geom_text(data = aggregate(Formaci.n.posgrado ~ CA10 , data, function(i) round(median(i),2)), 
  aes(label=Formaci.n.posgrado,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)

ggplot(data, aes(x=CA10, y=Remuneraci.n.TC, fill=as.factor(CA10))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Remuneración TC")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15)) +
  geom_text(data = aggregate(Remuneraci.n.TC ~ CA10 , data, function(i) round(median(i),2)), 
  aes(label=Remuneraci.n.TC,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)

ggplot(data, aes(x=CA10, y=Doctores.TC, fill=as.factor(CA10))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Doctores TC")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15)) +
  geom_text(data = aggregate(Doctores.TC ~ CA10 , data, function(i) round(median(i),2)), 
  aes(label=Doctores.TC,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)

ggplot(data, aes(x=CA10, y=Libros.revisados.por.pares, fill=as.factor(CA10))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Libros revisados por pares")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15)) +
  geom_text(data = aggregate(Libros.revisados.por.pares ~ CA10 , data, function(i) round(median(i),2)), 
  aes(label=Libros.revisados.por.pares,family=c("serif")), position = position_dodge(width=0.8), colour="ghostwhite", size=7)





