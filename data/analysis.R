getwd()

d = read.csv("last-try-per-person.csv", header=FALSE)

reference =  c(0,500,833,1000,1500,2500,3000)
d$reference = rep(reference, nrow(d))



r = data.frame(
	id = 1:nrow(d),
	reference = rep(reference,each= nrow(d)),
	input = unlist(d)
);
d = t(d)
r
plot(input ~ reference  ,r )


r$error =r$input - r$reference

r[r$reference == 1000 & r$error > 500, ]

matplot(reference,d, type="b",
	xlab="Zeitpunkt im Referenzmuster",
	ylab="Zeitpunkt Nutzereingabe"
)
lines(reference,reference, 
	lwd=3, lty="dashed"
)


par(mfrow=c(2,4))
for (i in 1:7) {
hist(d[i,] - reference[i],breaks=seq(-2000,2000,by=100),
	main=i,
	xlab=paste("Fehler zum Referenzwert ",i),
	xlim=c(-2000,2000),
	ylim=c(0,50),
)
}
par(mfrow=c(1,1))


d[,d[4,] > 1500]
dcorr = d[,d[4,] <= 1500]
summary(t(dcorr))


par(mfrow=c(3,1))
dcorr = d[,d[7,] <= 2800 & d[7,] <= 2800]

matplot(reference, dcorr - reference, pch=1,lty=1 ,type="b",
	main="Fehler zum Referenzwert"
)

1/apply(abs(dcorr-reference), 1, sd)
apply(abs(dcorr-reference), 1, var)


# Korrektur auf letzten wert
for (row in 1:ncol(dcorr)) {
	dcorr[, row] = dcorr[,row] / (dcorr[7,row]/3000)
}
dcorr
matplot(reference, dcorr - reference, pch=1, col=1,lty=1 ,type="b",
	main="Skalierung auf den Referenzwert am Ende"
)
apply(dcorr, 2,  sd)

deviation = apply(dcorr- reference,1, sd)
deviation
plot(reference,deviation)

as.vector(dcorr-reference)

bounds = 2*apply(deviation, 1,sd)
bounds
model = lm(bounds ~ 0+reference )
model

lines(reference,bounds)
lines(reference,reference * 0.22)

lines(reference,-bounds)
lines(reference,-reference * 0.22)


matplot(reference, deviation, )

model
