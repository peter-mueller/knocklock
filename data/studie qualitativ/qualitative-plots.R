data = read.csv(file("formatiert.csv", encoding="ISO-8859-1"), sep=";")
dataNonIT = read.csv(file("formatiert_nonit.csv", encoding="ISO-8859-1"), sep=";")
dataIT = read.csv(file("formatiert_it.csv", encoding="ISO-8859-1"), sep=";")

studie = sample(data)
studieNonIT = sample(dataNonIT)
studieIT = sample(dataIT)

summary(studie)
length(studie$Person)
BewertungEingabe = factor(studie$BewertungEingabe ,levels=c(1,2,3,4,5), labels=c("Sehr leicht","-",".","+","Sehr schwer"))
BewertungSicherheit = factor(studie$BewertungSicherheit ,levels=c(1,2,3,4,5), labels=c("Nicht sicher","-",".","+","Sehr sicher"))
ProblemeSpeichern = factor(studie$ProblemeSpeichern, levels=c(1,2,3,4,5), labels=c("Keine","-",".","+","Viele"))
ProblemeEntsperren= factor(studie$ProblemeEntsperren, levels=c(1,2,3,4,5), labels=c("Keine","-",".","+","Viele"))
BevorzugenVorPasswort= factor(studie$BevorzugenVorPasswort, levels=c(1,2,3,4,5), labels=c("Definitiv","-",".","+","Eher nicht"))

BewertungEingabeNonIt = factor(studieNonIT$BewertungEingabe ,levels=c(1,2,3,4,5), labels=c("Sehr leicht","-",".","+","Sehr schwer"))
BewertungSicherheitNonIt = factor(studieNonIT$BewertungSicherheit ,levels=c(1,2,3,4,5), labels=c("Nicht sicher","-",".","+","Sehr sicher"))
ProblemeSpeichernNonIt = factor(studieNonIT$ProblemeSpeichern, levels=c(1,2,3,4,5), labels=c("Keine","-",".","+","Viele"))
ProblemeEntsperrenNonIt= factor(studieNonIT$ProblemeEntsperren, levels=c(1,2,3,4,5), labels=c("Keine","-",".","+","Viele"))
BevorzugenVorPasswortNonIt= factor(studieNonIT$BevorzugenVorPasswort, levels=c(1,2,3,4,5), labels=c("Definitiv","-",".","+","Eher nicht"))

BewertungEingabeIT = factor(studieIT$BewertungEingabe ,levels=c(1,2,3,4,5), labels=c("Sehr leicht","-",".","+","Sehr schwer"))
BewertungSicherheitIT = factor(studieIT$BewertungSicherheit ,levels=c(1,2,3,4,5), labels=c("Nicht sicher","-",".","+","Sehr sicher"))
ProblemeSpeichernIT = factor(studieIT$ProblemeSpeichern, levels=c(1,2,3,4,5), labels=c("Keine","-",".","+","Viele"))
ProblemeEntsperrenIT= factor(studieIT$ProblemeEntsperren, levels=c(1,2,3,4,5), labels=c("Keine","-",".","+","Viele"))
BevorzugenVorPasswortIT= factor(studieIT$BevorzugenVorPasswort, levels=c(1,2,3,4,5), labels=c("Definitiv","-",".","+","Eher nicht"))


summary(BewertungEingabe )
BewertungSicherheit
#par(mfrow=c(3,2), cex=rep(3, 2))

t = table(BewertungEingabe)*100/length(BewertungEingabe)
t_nonit = table(BewertungEingabeNonIt)*100/length(BewertungEingabeNonIt)
t_it = table(BewertungEingabeIT)*100/length(BewertungEingabeIT)
nas = paste(sum(is.na(BewertungEingabe)), "nicht angegeben")
barplot(rbind(t, t_nonit, t_it), main="Wie bewertest du die Eingabe?", ylab = "Prozentualer Anteil", sub=nas, beside = TRUE, col=c("lightgreen", "lightcoral", "lightblue"))
legend("top", legend = c("Gesamt","Kein IT","IT"), fill=c("lightgreen", "lightcoral", "lightblue"))
#barplot(t, main="Wie bewertest du die Eingabe?", sub=nas )

t = table(BewertungSicherheit)*100/length(BewertungSicherheit)
t_nonit = table(BewertungSicherheitNonIt)*100/length(BewertungSicherheitNonIt)
t_it = table(BewertungSicherheitIT)*100/length(BewertungSicherheitIT)
nas = paste(sum(is.na(BewertungSicherheit)), "nicht angegeben")
barplot(rbind(t, t_nonit, t_it), main="Wie bewertest du die Sicherheit \n des KnockLocks?", ylab = "Prozentualer Anteil", sub=nas, beside = TRUE, col=c("lightgreen", "lightcoral", "lightblue"))
legend("top", legend = c("Gesamt","Kein IT","IT"), fill=c("lightgreen", "lightcoral", "lightblue"))
#barplot(t, main="Wie bewertest du die Sicherheit \n des KnockLocks?", sub=nas )

t = table(ProblemeSpeichern)*100/length(ProblemeSpeichern)
t_nonit = table(ProblemeSpeichernNonIt)*100/length(ProblemeSpeichernNonIt)
t_it = table(ProblemeSpeichernIT)*100/length(ProblemeSpeichernIT)
nas = paste(sum(is.na(ProblemeSpeichern)), "nicht angegeben")
barplot(rbind(t, t_nonit, t_it), main="Gab es Probleme beim Speichern \n deines Musters?", ylab = "Prozentualer Anteil", sub=nas, beside = TRUE, col=c("lightgreen", "lightcoral", "lightblue"))
legend("top", legend = c("Gesamt","Kein IT","IT"), fill=c("lightgreen", "lightcoral", "lightblue"))
#barplot(t, main="Gab es Probleme beim Speichern \n deines Musters?", sub=nas )

t = table(BevorzugenVorPasswort)*100/length(BevorzugenVorPasswort)
t_nonit = table(BevorzugenVorPasswortNonIt)*100/length(BevorzugenVorPasswortNonIt)
t_it = table(BevorzugenVorPasswortIT)*100/length(BevorzugenVorPasswortIT)
nas = paste(sum(is.na(BevorzugenVorPasswort)), "nicht angegeben")
barplot(rbind(t, t_nonit, t_it), main="Würdest du den KnockLock gegenüber \n einem Passwort bevorzugen?", ylab = "Prozentualer Anteil", sub=nas, beside = TRUE, col=c("lightgreen", "lightcoral", "lightblue"))
legend("top", legend = c("Gesamt","Kein IT","IT"), fill=c("lightgreen", "lightcoral", "lightblue"))
#barplot(t, main="Würdest du den KnockLock gegenüber \n einem Passwort bevorzugen?", sub=nas )

t = table(ProblemeEntsperren)*100/length(ProblemeEntsperren)
t_nonit = table(ProblemeEntsperrenNonIt)*100/length(ProblemeEntsperrenNonIt)
t_it = table(ProblemeEntsperrenIT)*100/length(ProblemeEntsperrenIT)
nas = paste(sum(is.na(ProblemeEntsperren)), "nicht angegeben")
barplot(rbind(t, t_nonit, t_it), main="Gab es Probleme beim Entsperren?", ylab = "Prozentualer Anteil", sub=nas, beside = TRUE, col=c("lightgreen", "lightcoral", "lightblue"))
legend("top", legend = c("Gesamt","Kein IT","IT"), fill=c("lightgreen", "lightcoral", "lightblue"))
#barplot(t, main="Gab es Probleme beim Entsperren?", sub=nas )

# "Welche T?tigkeit ?bst du aus? (Beruf bzw. Fachrichtung bei Studium)"
#"Wie alt bist du?"

par(mfrow=c(1,1))
nas = paste(sum(is.na(studie$Alter)), "nicht angegeben")
hist(studie$Alter, main="Wie alt bist du?", sub=nas)

# Fehlerarten

Bestanden = 1114
Zeitfehler = 189
Anzahlfehler = 466
Abweichungsfehler = 188


datatable = matrix( 
  c(Bestanden,0,0,0, 0,Zeitfehler,Anzahlfehler,Abweichungsfehler), 
  ncol=2
)
barplot(datatable,
        ylab="Anzahl",
        names.arg = c("Erfolgreich", "Fehler"),
        main="Anzahl Erfolgreiche Authentifizierung\n und Fehlerarten",
        col=c("lightgreen", "lightyellow", "lightcoral", "lightblue")
)
text(x=1.9,y=100,labels = "Eingabeverz.-fehler")     
text(x=1.9,y=500,labels = "Anzahlfehler")   
text(x=1.9,y=750,labels = "Tempofehler")  
