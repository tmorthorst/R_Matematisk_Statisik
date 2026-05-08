#opgave 35
#Specifikt vil vi interessere os for variablen Hustype, der har de to mulige 
#værdier Hus og Rækkehus. interessere os for den
#generelle andel af huse ud af alle salg (dvs. både huse og rækkehuse) – vi kan kalde
#denne andel for p.


#1. Udregn et estimat ˆp for andelen af huse. i Salgspriser


Salgspriser <- read.csv("Data/Salgspriser.csv")
View(Salgspriser)
proportions(table( Salgspriser$Hustype))

rækkehuse <- subset(Salgspriser, Hustype =="Rækkehus")
Huse <- subset(Salgspriser, Hustype =="Hus")
p <- length(Huse$Hustype)/length(Salgspriser$Hustype)
p


