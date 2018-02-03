getwd()

par(mfrow=c(1,1))
orginalData = read.csv("all-data.csv", header=FALSE)

reference =  c(0,500,833,1000,1500,2500,3000)

# Abweichung zu Referenz Plot 1
r = data.frame(
	id = 1:nrow(orginalData),
	reference = rep(reference,each= nrow(orginalData)),
	input = unlist(orginalData)
);
plot(input ~ reference  ,r, 
     ylab="Eingabe",
     xlab="Zeit" )

# Abweichung zu Referenz Plot 2
r$error =r$input - r$reference
d = t(orginalData)
matplot(reference,d, type="l",
	xlab="Zeitpunkt im Referenzmuster",
	ylab="Zeitpunkt Nutzereingabe"
)
lines(reference,reference, 
	lwd=3, lty="dashed"
)

# Plot und Test auf Normalverteilung
par(mfrow=c(2,3))
for (i in 2:7) {
hist(d[i,] - reference[i],breaks=seq(-2000,2000,by=100),
	main=i,
	xlab=paste("Fehler zum Referenzwert ",i),
	xlim=c(-2000,2000)
)
}

#Keine Normalverteilung
par(mfrow=c(2,3))
dataset = t(d);
for (col in 2:7) {
qqnorm(dataset[,col], main = bquote("Timeslot " ~ .(col)))
qqline(dataset[,col])
}

# Verlauf und Abweichung über die Zeit
dcorr = d
summary(t(dcorr))

par(mfrow=c(1,1))
matplot(reference, dcorr - reference,lty=1 ,type="b",pch=1,
	main="Fehler zum Referenzwert",
	ylab="Fehler",
	xlab="Zeit",
	ylim=c(-2000, 2000)
)

summary(t(dcorr-reference))

#Varianz und Standardabweichung
1/apply(abs(dcorr-reference), 1, sd)
apply(abs(dcorr-reference), 1, var)

# Skalierung auf letzten Wert
for (row in 1:ncol(dcorr)) {
  linecorr = dcorr[,row] / (dcorr[7,row]/3000)
  if(all(linecorr <= reference*1.3 & linecorr >= reference*0.7))
	  dcorr[, row] = linecorr
  else
    dcorr[, row] = NA
}
dcorr = t(na.omit(t(dcorr)))

matplot(reference, dcorr - reference, pch=1,lty=1 ,type="b",
	main="Skalierung auf den Referenzwert am Ende",
	ylab="Fehler",
	xlab="Zeit",
	ylim=c(-2000, 2000)
)

summary(t(dcorr-reference))
#Varianz und Standardabweichung nach Skalierung
1/apply(abs(dcorr-reference), 1, sd)
apply(abs(dcorr-reference), 1, var)

#Nach Skalierung sollte es eine Normalverteilung sein
par(mfrow=c(2,3))
dataset = t(dcorr);
for (col in 2:6) {
  qqnorm(dataset[,col], main = bquote("Timeslot " ~ .(col)))
  qqline(dataset[,col])
}

###### Default Grenzen für KnockLock ableiten 
par(mfrow=c(1,1))
deviation = apply(dcorr - reference,1, sd)
deviation
#plot(reference,deviation)

bounds = cbind(+deviation, -deviation)
bounds
boxplot(t(bounds) , xlab="Values", ylab="Bounds")

sprintf("Maximale Abweichung bei normalisierten Werten (95%% Quantil): %f",2 * mean(deviation[2:6]))

nonScaledDev = apply((t(orginalData) - reference) * 2, 1, sd)
model <- lm(nonScaledDev ~ reference - 1)
plot(reference, nonScaledDev)
abline(model, col="red")
sprintf("Model zum Std-Fehler/Zeit für Skalierung: |meas_last - ref_last| <= ref_last * %f",model$coefficients[1])

###### 2. Studie

Versuche = 1957
Bestanden = 1114
Fehler = 843
Zeitfehler = 189
Anzahlfehler = 466
Abweichungsfehler = 188

barplot(c(Bestanden, Fehler, Anzahlfehler, Zeitfehler, Abweichungsfehler), 
        space = c(0.1,0.1,1,0.1,0.1),
        names.arg = c("Erfolg.", "Fehler", "Anzahl", "Zeit", "Abweich." ),
        legend.text = 
        ylab="Anzahl",
        cex.names=0.8,
        col = c("green3", "red", "gray", "gray", "gray")
        )
        