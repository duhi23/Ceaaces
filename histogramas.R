library(ggplot2)

# Escalafon
ggplot(datos, aes(x=Escalaf.n, fill=as.factor(Escalaf.n))) + geom_histogram(binwidth=1)+
  scale_fill_manual(values=colores) + labs(x=" ") + labs(title="Escalafon") + labs(fill="NIVEL") +
  labs(y="CASOS") + theme(text=element_text(size=15), legend.position="none") + facet_grid(. ~ mantenimiento)

# Evaluacion
ggplot(datos, aes(x=Evaluaci.n, fill=as.factor(Evaluaci.n))) + geom_histogram(binwidth=1)+
  scale_fill_manual(values=colores) + labs(x=" ") + labs(title="Evaluacion") + labs(fill="NIVEL") +
  labs(y="CASOS") + theme(text=element_text(size=15), legend.position="none") + facet_grid(. ~ mantenimiento)

# Admision a estudio de pregrado
ggplot(datos, aes(x=Admisi.n.a.estudios.de.pregrado, fill=as.factor(Admisi.n.a.estudios.de.pregrado))) + geom_histogram(binwidth=1)+
  scale_fill_manual(values=colores) + labs(x=" ") + labs(title="Admision a estudio de pregrado") + labs(fill="NIVEL") +
  labs(y="CASOS") + theme(text=element_text(size=15), legend.position="none") + facet_grid(. ~ mantenimiento)

# Admision a estudio de posgrado
ggplot(datos, aes(x=Admisi.n.a.estudios.de.posgrado, fill=as.factor(Admisi.n.a.estudios.de.posgrado))) + geom_histogram(binwidth=1)+
  scale_fill_manual(values=colores) + labs(x=" ") + labs(title="Admision a estudio de posgrado") + labs(fill="NIVEL") +
  labs(y="CASOS") + theme(text=element_text(size=15), legend.position="none") + facet_grid(. ~ mantenimiento)

# Planificacion de la investigacion
ggplot(datos, aes(x=Planificaci.n.de.la.investigaci.n, fill=as.factor(Planificaci.n.de.la.investigaci.n))) + geom_histogram(binwidth=1)+
  scale_fill_manual(values=colores) + labs(x=" ") + labs(title="Planificacion de la investigacion") + labs(fill="NIVEL") +
  labs(y="CASOS") + theme(text=element_text(size=15), legend.position="none") + facet_grid(. ~ mantenimiento)

# Uso del seguimiento a graduados
ggplot(datos, aes(x=Uso.del.seguimiento.a.graduados, fill=as.factor(Uso.del.seguimiento.a.graduados))) + geom_histogram(binwidth=1)+
  scale_fill_manual(values=colores) + labs(x=" ") + labs(title="Uso del seguimiento a graduados") + labs(fill="NIVEL") +
  labs(y="CASOS") + theme(text=element_text(size=15), legend.position="none") + facet_grid(. ~ mantenimiento)

# Rendicion anual de cuentas
ggplot(datos, aes(x=Rendici.n.anual.de.cuentas, fill=as.factor(Rendici.n.anual.de.cuentas))) + geom_histogram(binwidth=1)+
  scale_fill_manual(values=colores) + labs(x=" ") + labs(title="Rendicion anual de cuentas") + labs(fill="NIVEL") +
  labs(y="CASOS") + theme(text=element_text(size=15), legend.position="none") + facet_grid(. ~ mantenimiento)

# Transparencia financiera
ggplot(datos, aes(x=Transparencia.financiera, fill=as.factor(Transparencia.financiera))) + geom_histogram(binwidth=1)+
  scale_fill_manual(values=colores) + labs(x=" ") + labs(title="Transparencia financiera") + labs(fill="NIVEL") +
  labs(y="CASOS") + theme(text=element_text(size=15), legend.position="none") + facet_grid(. ~ mantenimiento)

# Etica y responsabilidad
ggplot(datos, aes(x=X.tica.y.responsabilidad, fill=as.factor(X.tica.y.responsabilidad))) + geom_histogram(binwidth=1)+
  scale_fill_manual(values=colores) + labs(x=" ") + labs(title="Etica y responsabilidad") + labs(fill="NIVEL") +
  labs(y="CASOS") + theme(text=element_text(size=15), legend.position="none") + facet_grid(. ~ mantenimiento)

# Informacion para la evaluacion
ggplot(datos, aes(x=Informaci.n.para.la.evaluaci.n, fill=as.factor(Informaci.n.para.la.evaluaci.n))) + geom_histogram(binwidth=1)+
  scale_fill_manual(values=colores) + labs(x=" ") + labs(title="Informacion para la evaluacion") + labs(fill="NIVEL") +
  labs(y="CASOS") + theme(text=element_text(size=15), legend.position="none") + facet_grid(. ~ mantenimiento)

# Regimen Academico
ggplot(datos, aes(x=R.gimen.acad.mico, fill=as.factor(R.gimen.acad.mico))) + geom_histogram(binwidth=1)+
  scale_fill_manual(values=colores) + labs(x=" ") + labs(title="Regimen Academico") + labs(fill="NIVEL") +
  labs(y="CASOS") + theme(text=element_text(size=15), legend.position="none") + facet_grid(. ~ mantenimiento)

# Accion afirmativa
ggplot(datos, aes(x=Acci.n.afirmativa, fill=as.factor(Acci.n.afirmativa))) + geom_histogram(binwidth=1)+
  scale_fill_manual(values=colores) + labs(x=" ") + labs(title="Accion afirmativa") + labs(fill="NIVEL") +
  labs(y="CASOS") + theme(text=element_text(size=15), legend.position="none") + facet_grid(. ~ mantenimiento)

# Gestion de biblioteca
ggplot(datos, aes(x=Gesti.n.de.biblioteca, fill=as.factor(Gesti.n.de.biblioteca))) + geom_histogram(binwidth=1)+
  scale_fill_manual(values=colores) + labs(x=" ") + labs(title="Gestion de biblioteca") + labs(fill="NIVEL") +
  labs(y="CASOS") + theme(text=element_text(size=15), legend.position="none") + facet_grid(. ~ mantenimiento)

# Innovacion tecnologica 
ggplot(datos, aes(x=Innovaci.n.tecnol.gica, fill=as.factor(Innovaci.n.tecnol.gica))) + geom_histogram(binwidth=1)+
  scale_fill_manual(values=colores) + labs(x=" ") + labs(title="Innovacion tecnologica") + labs(fill="NIVEL") +
  labs(y="CASOS") + theme(text=element_text(size=15), legend.position="none") + facet_grid(. ~ mantenimiento)

# Espacios de bienestar
ggplot(datos, aes(x=Espacios.de.bienestar, fill=as.factor(Espacios.de.bienestar))) + geom_histogram(binwidth=1)+
  scale_fill_manual(values=colores) + labs(x=" ") + labs(title="Espacios de bienestar") + labs(fill="NIVEL") +
  labs(y="CASOS") + theme(text=element_text(size=15), legend.position="none") + facet_grid(. ~ mantenimiento)


colnames(datos)[c(13,14,22,23,24,28,31,32,33,35,36,37,40,43,48)]


datos[[13]]
