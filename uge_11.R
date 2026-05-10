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
