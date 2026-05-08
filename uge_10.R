#opgave 56

#Vi bruger stadig datasæt med atleter fra ol2022, hvor vægten er responsvariablen og højden er forklaringsvariablen. 
#0 Fit den lineære regressionsmodel for datasættet hvor du fjerner de to tungeste atleter.

library(resampledata3)
View(Olympics2012)
plot(Olympics2012$Height, Olympics2012$Weight)

which(Olympics2012$Weight > 250)
Olympics2012_no_outliers <- Olympics2012[-c(9,32),]
plot(Olympics2012_no_outliers$Height, Olympics2012_no_outliers$Weight)
View(Olympics2012_no_outliers)
regr_olympics_no_outliers <- lm(Weight ~ Height, data = Olympics2012_no_outliers)
regr_olympics_no_outliers$coefficients

# 1. Hvad er den prædikterede vægt for en atlet, som er 69.4 inches høj?

regr_olympics_no_outliers$coefficients[1] + regr_olympics_no_outliers$coefficients[2] * 69.4

# 149,16

#2. Find et 95% konfidensinterval for den forventede vægt af en atlet, som er 69.4
#inches høj. Hvad siger intervallet om vægten?
new_obs <- data.frame(Height = 69.4)
predict(regr_olympics_no_outliers, newdata = new_obs, interval = "confidence", level = 0.95)


#3. 3. Find et 95% prædiktionsinterval for vægten af ovenst˚aende atlet, som er
#69.4 inches høj. Hvordan ser intervallet ud i forhold til konfidensintervalet
#fra delspm. 2? Forklar hvorfor.

predict(regr_olympics_no_outliers, newdata = new_obs, interval = 'prediction', level = 0.95)


#44. Find nu både 95% konfidensintervaller og 95% prædiktionintervaller for vægten
#af atleter med højde p˚a hhv. 54, 60 og 66 inches. Hvad kan siges om den
#relative størrelse af disse intervaller, n˚ar der varieres p˚a den forklarende variabel?
#Forklar dette.

obs_54 <- data.frame(Height = 54)
obs_60 <- data.frame(Height = 60)
obs_66 <- data.frame(Height = 66)

konf_54 <- predict(regr_olympics_no_outliers, newdata = obs_54, interval = "confidence", level = 0.95)
konf_60 <- predict(regr_olympics_no_outliers, newdata = obs_60, interval = "confidence", level = 0.95)
konf_66 <- predict(regr_olympics_no_outliers, newdata = obs_66, interval = "confidence", level = 0.95)

pred_54 <- predict(regr_olympics_no_outliers, newdata = obs_54, interval = "prediction", level = 0.95)
pred_60 <- predict(regr_olympics_no_outliers, newdata = obs_60, interval = "prediction", level = 0.95)
pred_66 <- predict(regr_olympics_no_outliers, newdata = obs_66, interval = "prediction", level = 0.95)

data.frame(konf_54, pred_54)
data.frame(konf_60, pred_60)
data.frame(konf_66, pred_66)

#jo tættere på middelværdien x_bar = 68.42 inches, jo mindre er både konfidensintervallet og prædiktionsintervallet.
# Det skyldes at der er mindre usikkerhed omkring den forventede vægt for atleter med højde tæt på middelværdien, 
#da der er flere observationer i det område. For atleter med højde længere væk fra middelværdien, 
#er der færre observationer og dermed større usikkerhed, hvilket resulterer i bredere intervaller.


#opgave 57
#Datasættet simdata bruges. n  = 1031 observationer og responsvariablene er uafhængige.
#1 1.	Datasættet er konstrueret sådan, at netop ´en af modelantagelserne ikke holder.
#Identificer hvilken antagelse der er tale om ved at lave modelkontrol.

#først plotter vi residualerne mod middelværdien som lineær funktion
simdata <- read.csv("Data/simdata.csv")
View(simdata)
regr_simdata <- lm(y~x, data = simdata)
regr_simdata 

plot(simdata$x, regr_simdata$residuals, xlab = "x", ylab = "Residuals", main = "Residuals vs x", pch= 19)
abline(h = 0, col = "red")
png("residuals_plot.png", width = 800, height = 600)
plot(simdata$x, regr_simdata$residuals, xlab = "x", ylab = "Residuals", main = "Residuals vs x",pch = 19)
abline(h = 0, col = "red")
dev.off()

#ingen systematik

# jeg laver nu et qq plot for at undersøge normalfordelingen af residualerne
qqnorm(regr_simdata$residuals, main = "QQ Plot of Residuals")
qqline(regr_simdata$residuals, col = "red")
png("qqplot_residuals.png", width = 800, height = 600)
qqnorm(regr_simdata$residuals, main = "QQ Plot of Residuals")
qqline(regr_simdata$residuals, col = "red")
dev.off()



#På trods af at modelantagelserne ikke er helt opfyldt, arbejder vi lige nu videre med modellen.

#2.	 Hvad er den prædikterede værdi af responsvariablen, hvis den forklarende
#variabel tager værdien x = 44?

regr_simdata$coefficients[1] + regr_simdata$coefficients[2] * 44


#3. Find både et 95% konfidensinterval samt et 95% prædiktionsonterval for værdien
#af responsvariablen, n˚ar den forklarende variabel tager værdien x = 44.

new_obs_sim <- data.frame(x = 44)
predict(regr_simdata, newdata = new_obs_sim, interval = "confidence", level = 0.95)
predict(regr_simdata, newdata = new_obs_sim, interval = "prediction", level = 0.95)


#opgave 58
# vi kigger på datasættet Hollywood. total: omsætning (i 1000 $) (respons) og weekend: omsætning (i 1000 $) (forklarende)

#1 fit en lineær regressionsmodel for datasættet og lav modelkontrol for at undersøge om modelantagelserne er opfyldt.
Hollywood <- read.csv("Data/Hollywood.csv")
View(Hollywood)
regr_hollywood <- lm(total~ weekend, data = Hollywood)
regr_hollywood$coefficients

#jeg plotter residualerne mod weekend for at undersøge om systematik
plot(Hollywood$weekend, regr_hollywood$residuals, xlab= "weekend i 1000 $", ylab = "Residuals", main = "Residuals vs Weekend", pch = 19)
abline(h=0, col = "red")
png("hollywood_residuals_plot.png", width = 800, height = 600)
plot(Hollywood$weekend, regr_hollywood$residuals, xlab= "weekend i 1000 $", ylab = "Residuals", main = "Residuals vs Weekend", pch = 19)
abline(h=0, col = "red")
dev.off()

#der er ingen systematik, linearitetsantagelsen er godkendt, da værdierne er tilfældig fordelt omrking residual = 0.
#Der er ikke konstant variansen er lille for små værdier af weekend og større for større værdier af weekend
#og jo flere penge der tjenes i weekenden jo mere underestimerer modellen total omsætningen

#jeg laver nu et qq plot for at undersøge normalfordelingen af residualerne
qqnorm(regr_hollywood$residuals, main = "QQ Plot of Residuals", pch = 19)
qqline(regr_hollywood$residuals, col = "red")
png("hollywood_qqplot.png", width = 800, height = 600)
qqnorm(regr_hollywood$residuals, main = "QQ Plot of Residuals", pch = 19)
qqline(regr_hollywood$residuals, col = "red")
dev.off()

#følger ikke en normalfordeling, da halerne er tungere end normalfordelingen.

#Da datasættet heller ikke har synderligt mange observationer, må vi benytte os af
#resamplingmetoder for at kunne udtale os fornuftigt om sammenhængen.

N <- 10^4 
n <- nrow(Hollywood)
boot_beta <- replicate(N, {
    index <- sample(1:n, replace = TRUE)
    coef(lm(total ~ weekend, data = Hollywood[index, ]))[2]
})

# Konfidensinterval for beta:
quantile(boot_beta, probs = c(0.025, 0.975))

# 3. Find den prædikterede totale omsætning for en film, som omsætter for 10 000$ pr. biograf i åbningsweekenden. 
#Brug igen bootstrap til at finde et 95% percentil interval for den forventede værdi af en sådan film.

N <- 10^4
n <- nrow(Hollywood)
x_new <- 10
EY.boot <- numeric(N)
alpha_boot <- numeric(N)
beta_boot <- numeric(N)

for (i in 1:N) {
    index <- sample(n, replace = TRUE)
    Hollywood_boot <- Hollywood[index, ]
    regr_boot <- lm(total ~ weekend, data = Hollywood_boot)
    alpha_boot[i] <- regr_boot$coefficients[1]
    beta_boot[i] <- regr_boot$coefficients[2]
    EY.boot[i] <- regr_boot$coefficients[1] + regr_boot$coefficients[2] * x_new
}

quantile(EY.boot, probs = c(0.025, 0.975))

#4. Selv hvis vi ikke er sikre på, om middelværdien af responsvariablen afhænger
#lineært af den forklarende variabel, kan vi faktisk teste for uafhængighed:
#Benyt et permutationstest til at teste, om fordelingen af den totale filmomsætning
#afhænger af omsætningen i åbningsweekenden.

y_permuteret <- sample(Hollywood$total)
cor(Hollywood$weekend, y_permuteret)
n <- nrow(Hollywood)
#eller:

xvariabel <- Hollywood$weekend
yvariabel <- Hollywood$total
N <- 10^5 -1
n <- length(yvariabel)
cor.obs <- cor(xvariabel, yvariabel)
cor.perm <- numeric(N)
for (i in 1:N) {
    index <- sample(n, replace = FALSE)
    yvariabel.perm <-yvariabel[index]
    cor.perm[i] <- cor(xvariabel, yvariabel.perm)
}

p_value <- 2*min((1+sum(cor.perm>=cor.obs))/(N+1),
                 (1+sum(cor.perm<=cor.obs))/(N+1))
p_value


#opgave 59
#Vi kigger på datasættet FL_crimes. vi har variablene: Kriminalitet (forbrydelser pr. 1000 indbyggere), 
#uddannelse (procentdel af voksenbefolkningenn med mindst studentereksamen) og urbanisering (procentandel der bor i byen)
#vi ladder kriminalitet som responsvariabel
#1 Fit en lineær regressionsmodel af kriminalitet og uddannelse. Skriv prædiktionsligningen op.

FL_crime <- read.csv("Data/FL_crime.csv")
View(FL_crime)
regr_crime <- lm(Kriminalitet ~Uddannelse, data = FL_crime)
regr_crime$coefficients
plot(FL_crime$Uddannelse, FL_crime$Kriminalitet, xlab = "Uddannelse (%)", ylab = "Kriminalitet (forbrydelser pr. 1000 indbyggere)", main = "Kriminalitet vs Uddannelse", pch = 19)

#y = -50.85 + 1.49*x


#2. Udvid nu modellen ved at lave en lineær regression med både Uddannelse
#og Urbanisering som forklarende variable. Forklar sammenhængen mellem
#kriminalitetsraten og hver af de forklarende variable. Sammenligning med din
#konklusion fra delspm. 1.

regr_crime2 <- lm(Kriminalitet ~ Uddannelse + Urbanisering, data = FL_crime)
regr_crime2$coefficients


#3. Angiv t-teststørrelser samt p-værdier for test om signifikans af Uddannelse
#og Urbanisering. Husk at opskrive nulhypotesen og den alternative hypotese
#for de to tests. Hvad kan konkluderes?

summary(regr_crime2)

#4. Find et 95% konfidensinterval for parameteren β_2, der beskriver effekten af
#Urbanisering på kriminalitetsraten

confint(regr_crime2, level = 0.95)

#5. I R, find designmatricen X og regn (X′X)−1.
#Hint: Husk at X′ er den transponerede af X. Designmatricen kan opn˚as fra
#regressionsobjektet ved R-funktionen model.matrix. Man kan transponere en
#matrix i R vha. funktionen t(). Hvis I først har gemt X som X og X′ som
#Xtran, s˚a kan I finde den inverse ved at skrive
#invMatrix <- solve(Xtran%*%X)

X <- model.matrix(regr_crime2)
X
Xtran <- t(X)
invMatrix <- solve(Xtran %*% X)
invMatrix


#6.	Skriv teststørrelsen op og angiv dens fordeling under nulhypotesen H0. Brug
#dette samt delspm. 5 til at finde både observeret teststørrelse samt p-værdi.
#Det kan frit bruges at S = 20.82 (denne størrelse indgår i formlen for teststørrelsen).

#vi bruger formlen T = beta_hat_i / (S* sqrt(c_i))

beta_hat_1 <- regr_crime2$coefficients[2]
beta_hat_1 
S <- 20.82
T <- beta_hat_1 / (S * sqrt(invMatrix[2, 2]))
T
t_obs <- beta_hat_1 / (S * sqrt(0.0005151709))

#finder antallet af frihedsgrader:
n <- nrow(FL_crime)
k <- 2
df <- n - (k+1)
df
#finder p værdien:
p_value <- 2 * pt(abs(T), df = df, lower.tail = FALSE)
p_value


#Opgave 60
#Man kan også vare interesseret i at lave tests for generelle linearkombinationer af
#modellens parametre. Derfor er det vigtigt at kunne udtale sig om fordelingen af disse.
#Vi fortsætter med datasættet FL crime fra opgave 59, hvor β1 angiver parameteren
#hørende til variablen Uddannelse og β2 er parameteren horende til Urbanisering.
#Notationen ′ nedenfor betyder transponering af vektoren/matricen.

#1. Skriv linearkombinationen 3 ˆ β1 − 2 ˆ β2 som et produkt af to vektorer, hvor den
#ene er søjlevektoren ˆβ = (ˆα ˆ β1 ˆ β2)′.

var(3* regr_crime2$coefficients[1] - 2 *regr_crime2$coefficients[2])

S <- 20.82
v <- c(0, 3, -2)
var_v <- S^2 * t(v) %*% invMatrix %*% v
var_v


#opgave 61
#vi kigger på datasættet huspriser_OR, der indeholder data over 200 hussalg i Oregon.
# vi har variablene: pris (i 1000 $), størrelse (i square feet) og grund (i square feet), 
#antal soveværelser og antal badeværelser. og Alder i år.
#Helt naturligt vælges pris som responsvariabel og de andre som forklarende variable.

#1 fit en lineær regressionsmodel for datasættet og rapporter modellens forklaringsgrad.
huspriser <- read.csv("Data/huspriser_OR.csv")
View(huspriser)
regr_huspriser <- lm(Pris ~ . -Garage, data = huspriser)
summary(regr_huspriser)
regr_huspriser$coefficients

#2. fit en model med pris som responsvariabel og kun størrelse, badeværelser og soveværelser som forklarende variable.
regr_huspriser2 <- lm(Pris~StørrelseHus + Badeværelser + Soveværelser, data = huspriser)
summary(regr_huspriser2)


#3. Lav et passende valgt test for, om Pris er uafhængigt af både StørrelseGrund
#og Alder. Husk at opskrive både nul- og alternativ hypotese, observeret teststørrelse
#samt p-vardi. Hvad kan konkluderes?

#vi laver en F test. 
regr_huspriser_full <- lm(Pris ~ StørrelseHus + StørrelseGrund + Badeværelser + Soveværelser + Alder, data = huspriser)
regr_huspriser_reduced <- lm(Pris ~ . -StørrelseGrund - Alder - Garage, data = huspriser)
anova(regr_huspriser_reduced, regr_huspriser_full)
regr_huspriser_full$coefficients
regr_huspriser_reduced$coefficients
