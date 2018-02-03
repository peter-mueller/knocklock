data = read.csv("formatiert.csv", sep=";")

studie = sample(data)
summary(studie)
BewertungEingabe = factor(studie$BewertungEingabe ,levels=c(1,2,3,4,5), labels=c("Sehr leicht","-",".","+","Sehr schwer"))
BewertungSicherheit = factor(studie$BewertungSicherheit ,levels=c(1,2,3,4,5), labels=c("Nicht sicher","-",".","+","Sehr sicher"))
ProblemeSpeichern = factor(studie$ProblemeSpeichern, levels=c(1,2,3,4,5), labels=c("Keine","-",".","+","Viele"))

ProblemeEntsperren= factor(studie$ProblemeEntsperren, levels=c(1,2,3,4,5), labels=c("Keine","-",".","+","Viele"))
BevorzugenVorPasswort= factor(studie$BevorzugenVorPasswort, levels=c(1,2,3,4,5), labels=c("Definitiv","-",".","+","Eher nicht"))

summary(BewertungEingabe )
BewertungSicherheit
par(mfrow=c(3,2))


t = table(BewertungEingabe,useNA)
nas = paste(sum(is.na(BewertungEingabe)), "nicht angegeben")
barplot(t, main="Wie bewertest du die Eingabe?", sub=nas )

t = table(BewertungSicherheit,useNA)
nas = paste(sum(is.na(BewertungSicherheit)), "nicht angegeben")
barplot(t, main="Wie bewertest du die Sicherheit \n des KnockLocks?", sub=nas)

t = table(ProblemeSpeichern,useNA)
nas = paste(sum(is.na(ProblemeSpeichern)), "nicht angegeben")
barplot(t, main="Gab es Probleme beim Speichern \n deines Musters?", sub=nas)

t = table(BevorzugenVorPasswort)
nas = paste(sum(is.na(BevorzugenVorPasswort)), "nicht angegeben")
barplot(t, main="Würdest du den KnockLock gegenüber \n einem Passwort bevorzugen?", sub=nas)

t = table(ProblemeEntsperren)
nas = paste(sum(is.na(ProblemeEntsperren)), "nicht angegeben")
barplot(t, main="Gab es Probleme beim Entsperren?", sub=nas)

# "Welche Tätigkeit übst du aus? (Beruf bzw. Fachrichtung bei Studium)"
#"Wie alt bist du?"

par(mfrow=c(1,1))
nas = paste(sum(is.na(studie$Alter)), "nicht angegeben")
hist(studie$Alter, main="Wie alt bist du?", sub=nas)
