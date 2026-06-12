#3 Plot residualerne mod de prædikterede værdier samt mod hver af de kvantitative
#forklarende variable. Kommenter på plottene.

huspriserOR <- read.csv("Data/huspriser_OR.csv")
View(huspriserOR)
multi_regr <- lm(Pris ~ ., data = huspriserOR)
summary(multi_regr)

# Residualer mod prædikterede værdier
plot(multi_regr$fitted.values, multi_regr$residuals, pch = 19)
abline(h = 0, col = 'red')
png("residuals_vs_fitted.png", width = 800, height = 600)
plot(multi_regr$fitted.values, multi_regr$residuals, pch = 19)
abline(h = 0, col = 'red')
dev.off()

# Residualplot mod hver enkelt kvantitativ x-variabel
#størrelseHus
plot(huspriserOR$StørrelseHus, multi_regr$residuals,pch=19, ylab = "Residualer", xlab = "Størrelse på huset", main = "Residualer mod Størrelse på huset");abline(h=0, col="red")
png("residuals_vs_størrelseHus.png", width = 800, height = 600)
plot(huspriserOR$StørrelseHus, multi_regr$residuals,pch=19, ylab = "Residualer", xlab = "Størrelse på huset", main = "Residualer mod Størrelse på huset");abline(h=0, col="red")
dev.off()

#StørrelseGrund

plot(huspriserOR$StørrelseGrund, multi_regr$residuals, pch= 19, ylab ="Residualer", xlab = "størrelse på grunden", main = "Residualer mod størrelse på grunden");abline(h=0, col ="red")
png("residuals_vs_størrelseGrund.png", width = 800, height = 600)
plot(huspriserOR$StørrelseGrund, multi_regr$residuals, pch= 19, ylab ="Residualer", xlab = "størrelse på grunden", main = "Residualer mod størrelse på grunden");abline(h=0, col ="red")
dev.off()

#Soveværelser

plot(huspriserOR$Soveværelser, multi_regr$residuals, pch = 19, ylab = "Residualer", xlab = "Antal soveværelser", main = "Residualer mod antal soveværelser");abline(h=0, col="red")
png("residuals_vs_soveværelser.png", width = 800, height = 600)
plot(huspriserOR$Soveværelser, multi_regr$residuals, pch = 19, ylab = "Residualer", xlab = "Antal soveværelser", main = "Residualer mod antal soveværelser");abline(h=0, col="red")
dev.off()


#Badeværelser

plot(huspriserOR$Badeværelser, multi_regr$residuals, pch = 19, ylab = "Residualer", xlab = "Antal badeværelser", main = "Residualer mod antal badeværelser");abline(h=0, col="red")
png("residuals_vs_badeværelser.png", width = 800, height = 600)
plot(huspriserOR$Badeværelser, multi_regr$residuals, pch = 19, ylab = "Residualer", xlab = "Antal badeværelser", main = "Residualer mod antal badeværelser");abline(h=0, col="red")
dev.off()

#Alder
plot(huspriserOR$Alder, multi_regr$residuals, pch = 19, ylab = "Residualer", xlab = "Alder på huset", main = "Residualer mod alder på huset");abline(h=0, col="red")
png("residuals_vs_alder.png", width = 800, height = 600)
plot(huspriserOR$Alder, multi_regr$residuals, pch = 19, ylab = "Residualer", xlab = "Alder på huset", main = "Residualer mod alder på huset");abline(h=0, col="red")
dev.off()


#4. Lav et histogram over residualerne inddelt efter hver værdi af den kategoriske
#variabel Garage. Ser det ud til, at residualerne i disse to grupper har ca.
#samme middelværdi nær 0 og har cirka samme variation?
#Hint: Du kan evt. ogs˚a finde middelværdi og standardafvigelse, for hver af de
#to grupper.

par(mfrow=c(1,2))
hist(multi_regr$residuals[huspriserOR$Garage == 1], main = "Garage = 1 (Ja)", xlab = "Residualer", ylab = "Frekvens", col = "lightblue")
hist(multi_regr$residuals[huspriserOR$Garage == 0], main = "Garage = 0 (Nej)", xlab = "Residualer", ylab = "Frekvens", col = "lightcoral")
png("residuals_histogram_garage.png", width = 800, height = 600)
par(mfrow=c(1,2))
hist(multi_regr$residuals[huspriserOR$Garage == 1], main = "Garage = 1 (Ja)", xlab = "Residualer", ylab = "Frekvens", col = "lightblue")
hist(multi_regr$residuals[huspriserOR$Garage == 0], main = "Garage = 0 (Nej)", xlab = "Residualer", ylab = "Frekvens", col = "lightcoral")
dev.off()

mean(multi_regr$residuals[huspriserOR$Garage == 1])
mean(multi_regr$residuals[huspriserOR$Garage == 0])

sd(multi_regr$residuals[huspriserOR$Garage == 1])
sd(multi_regr$residuals[huspriserOR$Garage == 0])

#5 Angiv de estimerede parametre samt forklaringsgraden. Hvad siger modellen
#om sammenhængen mellem huspris og husstørrelse? Hvad siger modellen om
#sammenhængen mellem huspris og hvorvidt, huset har en garage?

summary(multi_regr)


#6.	Er prisen signifikant afhængig af hver af de forklarende variable? Argumenter ved hjælp af et eller flere relevante tests.

library(car)
Anova(multi_regr, type = "III")

#7 7. Reducer modellen, s˚a den kun indeholder signifikante forklarende variable.
#Med signifikant menes der p˚a et 5% signifikansniveau (behold skæringen/intercept,
#selvom det skulle være insignifikant). Hvilke variable beholdes i modellen? Angiv
#estimerede parametre og forklaringsgraden igen.
#Bemærk: Det er ikke nødvendigt at vise output, hver gang du reducerer modellen.
#Men forklar proceduren ved modelreduktionen.

#Først fjerner jeg den mindst signifikante variabel, som er "størrelseGrund".

reduced_multi_regr_1 <- lm(Pris ~. - StørrelseGrund, data = huspriserOR)
Anova(reduced_multi_regr_1, type = 'III')

reduced_multi_regr_2 <- lm(Pris ~. - StørrelseGrund - Alder, data = huspriserOR)
Anova(reduced_multi_regr_2, type = 'III')

reduced_multi_regr_3 <- lm(Pris ~. - StørrelseGrund - Alder - Garage, data = huspriserOR)
Anova(reduced_multi_regr_3, type = 'III')

reduced_multi_regr_4 <- lm(Pris ~. - StørrelseGrund - Alder - Garage - Soveværelser, data = huspriserOR)
Anova(reduced_multi_regr_4, type = 'III')

reduced_multi_regr_4$coefficients
summary(reduced_multi_regr_4)

#8. I den reducerede model, hvad er forskellen i prædikteret pris for et hus på 2460 squarefeet med 2 badev.relser, 3 soveværelser og med garage, sammenlignet med et hus p˚a 2460 squarefeet med 2 badev.relser, 4 sovev.relser og uden
# garage?



#opgave 63 
#vi arbejder videre med datasættet bilpriserNY, som er bilpriser hvor vi fjerner en outlier,
#og samtidig introducerer logpris

#1 fit en multipel lineær regressionsmodel, hvor logpris er respons variablen
#og resterende er forklarende. Hvad siger modellen om sammenh.ngen mellem logpris og sæslgers region?

Bilpriser <- read.csv("Data/Bilpriser.csv")

View(Bilpriser)
regr <- lm(Pris ~., data = Bilpriser)
plot(regr)

BilpriserNy <- Bilpriser[-829,]
BilpriserNy$LogPris <- log(BilpriserNy$Pris)
regr_ny <- lm(LogPris ~.-Pris, data = BilpriserNy)

summary(regr_ny)

ordning <- c("Sjælland","Fyn","Jylland","København")
BilpriserNy$Region <- factor(BilpriserNy$Region, levels = ordning)
regr_ny_ordning <- lm(LogPris ~.-Pris, data = BilpriserNy)
summary(regr_ny_ordning)


#3 3. Vi arbejder videre med modellen fra delspm. 2. Lav følgende dele af en modelkontrol:
#QQ-plot af residualerne, plot residualerne mod de prædikterede
#værdier, plot residualerne mod variablene Km og Alder, og lav histogrammer
#af residualerne inddelt efter hvorvidt sælger var privat eller ej. Er modelantagelserne
#rimelige?

#qqplot:
qqnorm(regr_ny_ordning$residuals, main = "QQ-plot af residualer"); qqline(regr_ny_ordning$residuals, col = "red")
png("qqplot_residuals_opgave63_3.png", width = 800, height = 600)
qqnorm(regr_ny_ordning$residuals, main = "QQ-plot af residualer"); qqline(regr_ny_ordning$residuals, col = "red")
dev.off()

#Residualer mod prædikterede værdier
plot(regr_ny_ordning$fitted.values, regr_ny_ordning$residual,s, pch = 19, xlab = "Prædikterede værdier", ylab = "Residualer", main = "Residualer mod prædikterede værdier"); abline(h=0, col="red")
png("residuals_vs_fitted_opgave63_3.png", width = 800, height = 600)
plot(regr_ny_ordning$fitted.values, regr_ny_ordning$residuals, pch = 19, xlab = "Prædikterede værdier", ylab = "Residualer", main = "Residualer mod prædikterede værdier"); abline(h=0, col="red")
dev.off()

#Residualer mod Km
plot(BilpriserNy$Km, regr_ny_ordning$residuals, pch = 19, xlab = "Km", ylab = "Residualer", main = "Residualer mod Km"); abline(h=0, col="red")
png("residuals_vs_km_opgave63_3.png", width = 800, height = 600)
plot(BilpriserNy$Km, regr_ny_ordning$residuals, pch = 19, xlab = "Km", ylab = "Residualer", main = "Residualer mod Km"); abline(h=0, col="red")
dev.off()

#Residualer mod Alder
plot(BilpriserNy$Alder, regr_ny_ordning$residuals, pch = 19, xlab = "Alder", ylab = "Residualer", main = "Residualer mod Alder"); abline(h=0, col="red")
png("residuals_vs_alder_opgave63_3.png", width = 800, height = 600)
plot(BilpriserNy$Alder, regr_ny_ordning$residuals, pch = 19, xlab = "Alder", ylab = "Residualer", main = "Residualer mod Alder"); abline(h=0, col="red")
dev.off()

View(BilpriserNy)
#Histogrammer af residualer inddelt efter privat sælger eller ej
par(mfrow=c(1,2))
hist(regr_ny_ordning$residuals[BilpriserNy$Sælger == "ja"], main = "Privat sælger", xlab = "Residualer", ylab = "Frekvens", col = "lightblue", breaks = 20)
hist(regr_ny_ordning$residuals[BilpriserNy$Sælger == "nej"], main = "Ikke privat sælger", xlab = "Residualer", ylab = "Frekvens", col = "lightcoral", breaks = 20)
png("residuals_histogram_privatsælger_opgave63_3.png", width = 800, height = 600)
par(mfrow=c(1,2))
hist(regr_ny_ordning$residuals[BilpriserNy$Sælger == "ja"], main = "Privat sælger", xlab = "Residualer", ylab = "Frekvens", col = "lightblue", breaks = 20)
hist(regr_ny_ordning$residuals[BilpriserNy$Sælger == "nej"], main = "Ikke privat sælger", xlab = "Residualer", ylab = "Frekvens", col = "lightcoral", breaks = 20)
dev.off()


#4 De resterende modelkontrol plots er rimelige, og vi arbejder derfor videre med
#modellen. Opstil et test for om logprisen afhænger signifikant af variablen
#Region. Hvad konkluderes p˚a et 5% signifikansniveau?
library(car)
Anova(regr_ny_ordning, type = "III")


#5. I følge modellen, hvordan adskiller Km-afhængigheden af logprisen sig for
#diesel i forhold til ikke-diesel biler?

summary(regr_ny_ordning)
#For at undersøge dette, kan vi inkludere en interaktion mellem Km og Diesel i modellen:
regr_ny_interaction <- lm(LogPris ~. -Pris + Km:Diesel, data = BilpriserNy)
summary(regr_ny_interaction)


#6. Fit en lineær regression som i delspm. 2 men hvor du nu inkluderer en vekselvirkning
#mellem Km og Diesel. Angiv parameterestimatet for vekselvirkningen
#og fortolk denne størrelse. Find desuden et 95% konfidensinterval for
#parameteren.

regr_veksel_km_diesel <- lm(LogPris ~. -Pris - Km - Diesel + Km * Diesel, data = BilpriserNy)
summary(regr_veksel_km_diesel)
confint(regr_veksel_km_diesel, "Km:Dieselnej", level=0.95)

#7. Hvad er den prædikterede logpris af en brugt diesel-bil, som har kørt 123 000
#Km, er 7˚ar gammel, og som sælges af en privatperson p˚a Fyn? Angiv desuden
#et 95% prædiktionsinterval for logprisen af en s˚adan bil.

predict(regr_veksel_km_diesel, newdata = data.frame(Km = 123,Diesel = "ja", Alder = 7, Sælger = "ja", Region ="Fyn",Pris = 0), interval = "prediction", level = 0.95)
View(BilpriserNy)


#opgave 64
#Datas.ttet chiledata stammer fra en meningsm˚aling, der blev foretaget i Chile
#i for˚aret 1988 inden en folkeafstemning i oktober 1988, hvor der skulle stemmes
#om, hvorvidt Augusto Pinochet kunne forts.tte sit styre i yderligere 8 ˚ar. Folkeafstemningen,
#der gav en sejr til nej-siden p˚a 56% (mod 44% ja-stemmer), blev p˚a
#baggrund af massivt pres fra det internationale samfund respekteret af Pinochet og
#blev derfor enden p˚a over 16 ˚ars diktaturstyre. Vi vil kigge på de to variable

#Sex: køn, F = female og M = male
#Vote: angiver den adspurgte forventede stemme, Y = yes, N = no, U = undecided, A = abstain

#1 opstil en antalstabel, og angiv relevant nulhypotese.
ChileData <- read.csv("Data/chiledata.csv")
View(ChileData)

tabel <- table(ChileData$sex, ChileData$vote)

test <- chisq.test(tabel)
test$expected
test$observed

tabel_med_summer <- addmargins(tabel)
tabel_med_summer


#3 Udregn Pearson’s teststørrelse og find en p-værdi ved at approksimere med
#χ2-fordelingen (evt. ved brug af R).

test
test$residuals
#5. Find også en p-værdi ved permutation. F˚ar du samme konklusion som ovenfor?

perm_test <- chisq.test(tabel, simulate.p.value = TRUE, B= 10000)
perm_test
