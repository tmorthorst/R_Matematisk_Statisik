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

#opgave 53
#Vi skal i gang med modelkontrol. Vi har lige kigget på opgave 52, hvor der var en outlier (829) som vi nu vil fjerne.

View(Bilpriser)
Bilpriser_ny <- Bilpriser[-829,]

#1 Opskriv modelantagelserne for en simpel lineær regressionsmodel for de to
#modeller, hvor vi har henholdsvis Pris og log(Pris) som responsvariabel.
#besvaret i word

#2 2)	Fit begge modeller og plot deres residualer mod den forklarende variabel Km.

regr_bilpriser_ny <- lm(Pris ~ Km, Bilpriser_ny)
regr_bilpriser_log_ny <- lm(log_Pris ~ Km, Bilpriser_ny)

#For Pris modellen
plot(Bilpriser_ny$Km, regr_bilpriser_ny$residuals, xlab = "Km (1000 km)", ylab = "Residuals", main = "Residuals vs Km for Pris model", pch = 19)
abline(h=0, col = "red", lwd = 2)
png("residuals_bilpris_model.png", width= 800, height = 600)
plot(Bilpriser_ny$Km, regr_bilpriser_ny$residuals, xlab = "Km (1000 km)", ylab = "Residuals", main = "Residuals vs Km for Pris model", pch = 19)
abline(h=0, col = "red", lwd = 2)
dev.off()

# for log(pris) modellen

plot(Bilpriser_ny$Km, regr_bilpriser_log_ny$residuals, xlab = "Km (1000 km)", ylab = "Residuals", main = "Residuals vs Km for log(Pris) model", pch = 19)
abline(h=0, col = "blue", lwd = 2)
png("residuals_bilpris_log_model.png", width= 800, height = 600)
plot(Bilpriser_ny$Km, regr_bilpriser_log_ny$residuals, xlab = "Km (1000 km)", ylab = "Residuals", main = "Residuals vs Km for log(Pris) model", pch = 19)
abline(h=0, col = "blue", lwd = 2)
dev.off()


#4 Lav et (normalfordelings) qq-plot for begge modeller. Hvad kan siges om normalfordelingsantagelsen?
qqnorm(regr_bilpriser_ny$residuals, main = "QQ-plot for Pris model")
qqline(regr_bilpriser_ny$residuals, col = "red", lwd = 2)
png("qqplot_bilpris_model.png", width= 800, height = 600)
qqnorm(regr_bilpriser_ny$residuals, main = "QQ-plot for Pris model")
qqline(regr_bilpriser_ny$residuals, col = "red", lwd = 2)
dev.off()


qqnorm(regr_bilpriser_log_ny$residuals, main = "QQ-plot for log(Pris) model")
qqline(regr_bilpriser_log_ny$residuals, col = "blue", lwd = 2)
png("qqplot_bilpris_log_model.png", width= 800, height = 600)
qqnorm(regr_bilpriser_log_ny$residuals, main = "QQ-plot for log(Pris) model")
qqline(regr_bilpriser_log_ny$residuals, col = "blue", lwd = 2)
dev.off()


#opgave 54

#vi arbejder videre med Bilpriser_ny hvor vi har fjernet outlieren
#Vi kører kun med log(pris) modellen, da det var den model der så bedst ud i modelkontrollen
#1 Fit modellen og angiv den estimerede skæring ˆα samt den estimerede hældning
#ˆ β.
regr_bilpriser_log_ny <- lm(log_Pris ~ Km, Bilpriser_ny)
regr_bilpriser_log_ny$coefficients



#3 Vi vil teste om hældningen for beta i virkeligheden er 0:coefficients
# H0: β = 0 (der er ingen sammenhæng mellem Km og log(Pris))
# H1: β ≠ 0 (der er en sammenhæng mellem Km og log(Pris))
#Opskriv en relevant teststørrelse og angiv dens fordeling under nulhypotesen.

#4 Find den observerede værdi af teststørrelsen samt en tilhørende p-værdi vha.
#summary-funktionen i R. Lav en konklusion p˚a 5% signifikansniveau.

summary(regr_bilpriser_log_ny)

#5 Find samme størrelser ved at kode i hånden: I skal finde S og ss_x og bruge disse til at finde den observerede værdi af teststørrelsen. Find også p-værdien ved at bruge pt-funktionen i R.


n <- length(Bilpriser_ny$Km)
y <- Bilpriser_ny$log_Pris
x <- Bilpriser_ny$Km
x_bar <- mean(x)
beta_hat <- regr_bilpriser_log_ny$coefficients[2]
y_hat <- fitted(regr_bilpriser_log_ny)

#udregner S:
S <- sqrt(1/(n-2)*sum((y-y_hat)^2))
#udregner ss_x:
ss_x <- sum((x-x_bar)^2)

#jeg udregner nu SE_beta_hat:
SE_beta_hat <- S/sqrt(ss_x)
#udregner nu teststørrelsen:
T <- (beta_hat - 0)/SE_beta_hat
T
# 4. Beregn p-værdien (Dobbeltsidet test i t-fordelingen)
# Vi bruger lower.tail = FALSE for at finde P(T >= |t_obs|)

p_value <- 2 * pt(abs(T), df = n-2, lower.tail = FALSE)

data.frame(S, ss_x, SE_beta_hat, T, p_value)


#6 Find vha. confint-kommandoen i R et 95% konfidensinterval for populationsparameteren
#β. Hvad siger dette om effekten af Km-antallet for brugte biler?

confint(regr_bilpriser_log_ny, level = 0.95)
# med 95% sikkerhed falder den forventede log(pris) med mellem 0,0037 og 0,0042 når bilens kørte km stiger med 1000

#opgave 55
#vi kigger på olympics og fjerner outliersne
#1 fit en lineær regressionsmodel med vægt som responsvariabel og højde som forklarende variabel., 

View(Olympics2012_no_outliers)
regr_olympics_no_outliers <- lm(Weight ~ Height, Olympics2012_no_outliers)
regr_olympics_no_outliers
plot(Olympics2012_no_outliers$Height, Olympics2012_no_outliers$Weight, xlab = "Height (inches)", ylab = "Weight (pounds)", main = "Scatterplot of Height vs. Weight (No Outliers)", pch = 19)
abline(regr_olympics_no_outliers, col = "blue",lwd=2)


#Udfør en modelkontrol ved at lave relevante plots. Er modellens antagelser
#rimeligvis opfyldte?
plot(Olympics2012_no_outliers$Height, regr_olympics_no_outliers$residuals, xlab = "Height (inches)", ylab = "Residuals", main = "Residuals vs Height", pch = 19)
abline(h=0, col = "red", lwd = 2)
png("residuals_olympics_no_outliers.png", width= 800, height = 600)
plot(Olympics2012_no_outliers$Height, regr_olympics_no_outliers$residuals, xlab = "Height (inches)", ylab = "Residuals", main = "Residuals vs Height", pch = 19)
abline(h=0, col = "red", lwd = 2)
dev.off()

qqnorm(regr_olympics_no_outliers$residuals, main = "QQ-plot for Olympics model (No Outliers)")
qqline(regr_olympics_no_outliers$residuals, col = "blue", lwd = 2)
png("qqplot_olympics_no_outliers.png", width= 800, height = 600)
qqnorm(regr_olympics_no_outliers$residuals, main = "QQ-plot for Olympics model (No Outliers)")
qqline(regr_olympics_no_outliers$residuals, col = "blue", lwd = 2)
dev.off()


#3 3. Find vha. R en passende observeret teststørrelse samt en p-værdi for test af hypotesen
#H0 : β = 0 mod HA : β ̸= 0. #Hvad konkluderer du på et 5% signifikansniveau? 

summary(regr_olympics_no_outliers)


#Find vha. R et 95% konfidensinterval for populationsparameteren β. Hvad
#siger dette om sammenhængen mellem højde og vægt af OL-atleter?

confint(regr_olympics_no_outliers, level = 0.95)
