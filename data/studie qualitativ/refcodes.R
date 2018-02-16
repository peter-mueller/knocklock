data = read.csv(file("refcodes.csv", encoding="ISO-8859-1"), sep=";", header = FALSE)

lengths <- array(0,dim=c(1,734))
for (i in 1:734) {
    lengths[i] = length(na.omit(t(data[i,])))
}

hist(lengths,main="Verteilung der Anzahl an Schläge" , xlab="Anzahl", ylab="Häufigkeit")
summary(t(lengths))
