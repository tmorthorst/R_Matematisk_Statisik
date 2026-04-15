#opgaver til uge 9
#opgave 51
#I datasæt Olympics2012 er der information om højde (inches) og vægt (pounds) for 42 atleter
#1 Tegn et scatterplot af vægten (y-aksen) mod højden (x-aksen) og beskriv hvad du ser

library(resampledata3)
View(head(Olympics2012))

plot(Olympics2012$Height, Olympics2012$Weight, xlab = "Height (inches)", ylab = "Weight (pounds)", 
main = "Scatterplot of Height vs. Weight", pch = 19)
png("scatterplot_olympics.png", width= 800, height = 600)
plot(Olympics2012$Height, Olympics2012$Weight, xlab = "Height (inches)", ylab = "Weight (pounds)", 
main = "Scatterplot of Height vs. Weight", pch = 19)
dev.off()

#Vi kan se at der er en positiv sammenhæng mellem højde og vægt. Jo højere du er, desto mere vejer du.
# Vi ser også at der findes to outliers, som vejer meget mere ift. deres højde end resten af datasættet.

#2. Find formlen for regressionslinjen og indtegn den p˚a dit scatterplot.

regr_olympics <- lm(Weight ~ Height, Olympics2012)
regr_olympics

regr_olympics$coefficients
plot(Olympics2012$Height, Olympics2012$Weight, xlab = "Height (inches)", ylab = "Weight (pounds)", 
main = "Scatterplot of Height vs. Weight", pch = 19)
abline(regr_olympics, col = "red",lwd=2)
png("scatterplot_olympics_regression.png", width= 800, height = 600)
plot(Olympics2012$Height, Olympics2012$Weight, xlab = "Height (inches)", ylab = "Weight (pounds)",
main = "Scatterplot of Height vs. Weight", pch = 19)
abline(regr_olympics, col = "red",lwd=2)
dev.off()


#3 3. To atleter vejer betydeligt mere end de andre (en vægtløfter og en kuglestøder).
#Fjern disse to outliers og find regressionslinjen igen. Virker det til, at outliers har en betydning?

summary(Olympics2012)
IQR_weight <- IQR(Olympics2012$Weight)
Q3_weight <- quantile(Olympics2012$Weight, 0.75)
outlier <- Q3_weight + 1.5 * IQR_weight
Olympics2012_no_outliers <- subset(Olympics2012, Weight < outlier)
View(Olympics2012_no_outliers)

regr_olympics_no_outliers <- lm(Weight ~ Height, Olympics2012_no_outliers)
regr_olympics_no_outliers$coefficients

#Vi finder at regressionslinjen uden outliers er: Weight = - 163.21 + 4.5 * Height
#Vi kan se at både hældningen og skæringen med y-aksen er ændret betydeligt,
#hvilket tyder på at outliers har en betydelig indflydelse på regressionslinjen.


#4. Visualiser dette ved at indtegne den nye linje på samme scatterplot som ovenfor.
plot(Olympics2012$Height, Olympics2012$Weight, xlab = "Height (inches)", ylab = "Weight (pounds)",
main = "Scatterplot of Height vs. Weight", pch = 19)
abline(regr_olympics_no_outliers, col = "blue",lwd=2)
abline(regr_olympics, col = "red",lwd=2)
legend("topleft", legend = c("With Outliers", "Without Outliers"), col = c("red", "blue"), lwd = 2)
png("scatterplot_olympics_both_regressions.png", width= 800, height = 600)
plot(Olympics2012$Height, Olympics2012$Weight, xlab = "Height (inches)", ylab = "Weight (pounds)",
main = "Scatterplot of Height vs. Weight", pch = 19)
abline(regr_olympics_no_outliers, col = "blue",lwd=2)
abline(regr_olympics, col = "red",lwd=2)
legend("topleft", legend = c("With Outliers", "Without Outliers"), col = c("red", "blue"), lwd = 2)
dev.off()


#5 Hvad er den prædikterede vægt for en atlet, som er 73.3 inches høj (baseret på regressionslinjen fundet i delspm. 3.)?

prædikteret_vægt <- regr_olympics_no_outliers$coefficients[1] + regr_olympics_no_outliers$coefficients[2] * 73.3
prædikteret_vægt


#opgave 52
# I datasættet BilPriser har vi data fra 1125 salg af brugte Peugeot 206 biler. Vi har variablene:
# Km målt i 1000 km og pris målt i 1000 kr.

#1 Hvad er naturligt at bruge som responsvariabel og som forklarende variabel i
#denne situation? Tegn et scatterplot med dit valg af responsvariabel op ad y aksen


Bilpriser <- read.csv("Data/BilPriser.csv")
View(head(Bilpriser))
plot(Bilpriser$Km, Bilpriser$Pris, xlab = "Km (1000 km)", ylab = "Pris (1000 kr.)", main = "Scatterplot af km vs pris", pch = 19)
png("scatterplot_bilpriser.png", width= 800, height = 600)
plot(Bilpriser$Km, Bilpriser$Pris, xlab = "Km (1000 km)", ylab = "Pris (1000 kr.)", main = "Scatterplot af km vs pris", pch = 19)
dev.off()

#2 find formlen for regressionslinjen og indtegn den på dit scatterplot
regr_bilpriser <- lm(Pris ~ Km, Bilpriser)
regr_bilpriser$coefficients
plot(Bilpriser$Km, Bilpriser$Pris, xlab = "Km (1000 km)", ylab = "Pris (1000 kr.)", main = "Scatterplot af km vs pris", pch = 19)
abline(regr_bilpriser, col = "red",lwd=2)
png("scatterplot_bilpriser_regression.png", width= 800, height = 600)
plot(Bilpriser$Km, Bilpriser$Pris, xlab = "Km (1000 km)", ylab = "Pris (1000 kr.)", main = "Scatterplot af km vs pris", pch = 19)
abline(regr_bilpriser, col = "red",lwd=2)
dev.off()

#3 3. Vælg nu variablen log(Pris) som responsvariabel (log er her den naturlige
#logaritme). Tegn igen et scatterplot, find regressionslinjen og indtegn denne.

Bilpriser$log_Pris <- log(Bilpriser$Pris)
View(head(Bilpriser))
regr_bilpriser_log <- lm(log_Pris ~ Km, Bilpriser)
regr_bilpriser_log$coefficients
plot(Bilpriser$Km, Bilpriser$log_Pris, xlab = "Km (1000 km)", ylab = "log(Pris) (log(1000 kr.))", main = "Scatterplot af km vs log(Pris)", pch = 19)
abline(regr_bilpriser_log, col = "blue",lwd=2)
png("scatterplot_bilpriser_log_regression.png", width= 800, height = 600)
plot(Bilpriser$Km, Bilpriser$log_Pris, xlab = "Km (1000 km)", ylab = "log(Pris) (log(1000 kr.))", main = "Scatterplot af km vs log(Pris)", pch = 19)
abline(regr_bilpriser_log, col = "blue",lwd=2)
dev.off()
