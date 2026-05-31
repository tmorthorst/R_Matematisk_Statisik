#uge 12 
#opgave 65
tabel_HS <- rbind(c(1440,500),c(46,281))

rownames(tabel_HS) <- c("Alkohol_ja","Alkohol_nej")
colnames(tabel_HS) <- c("Cigaret_ja", "Cigaret_nej")
tabel_HS

#1 udregn de forventede værdier under nulhypotesen:
# H0: Cigaret og alkoholforbrug er uafhængige
#Ha: Cigaret og alkoholforbrug er afhængige
test_tabel_HS <- chisq.test(tabel_HS, correct = FALSE)
test_tabel_HS$expected

#2. Udregn Pearson’s teststørrelse og find en p-værdi for testet ved at approksimere
#med χ2-fordelingen.

test_tabel_HS
#3. Gør nu det samme men med kontinuitetskorrektion. B˚ade (korrigerede) teststørrelse
#samt p-værdi skal altså angives.

test_tabel_HS_korr <- chisq.test(tabel_HS, correct = TRUE)
test_tabel_HS_korr

#5. Test desuden hypotesen ved Fisher’s eksakte test.
test_tabel_HS_fisher <- fisher.test(tabel_HS)
test_tabel_HS_fisher
# manuelt:
a <- 1440 #første indgang
r1 <- 1440 + 500 # rækkesum
c1 <- 1440 + 46 # kolonnesum
n <- 1440 + 500 + 46 + 281 # totalsum

pval <- 2* min(sum(dhyper(0:a, r1, n-r1, c1)), 
sum(dhyper(a:r1, r1, n-r1, c1)))
pval

#opgave 66
# opgave med 3 dataindsamlinger. Afgør om uafhængighedstest eller homogenitetstest er relevant.
# 1 dataindsamling om patienter med alkohol i blod og intet, dag eller nat:
tabel_patient <- rbind(c(68, 191), c(51,66))
rownames(tabel_patient) <- c("Dag","Nat")
colnames(tabel_patient) <- c("Alkohol_ja", "Alkohol_nej")
tabel_patient
tabel_patient_test <- chisq.test(tabel_patient, correct = FALSE)
tabel_patient_test

#2 dataindsamling af 5 kommunetyper, hvor 200 er blevet udvalgt i hver kommune og undersøgt hvor mange biler de her i husstanden.

tabel_biler <- rbind(c(52,104,44), c(44,104,52), c(60,92,48), c(88,90,22), c(104,78, 18))
rownames(tabel_biler) <- c("Land", "Oplands","Provinsby", "Storby", "Hovedstads")
colnames(tabel_biler) <- c("0 biler", "1 bil", "2+ biler")
tabel_biler
tabel_biler_test <- chisq.test(tabel_biler, correct = FALSE)
tabel_biler_test

#3 dataindsamling af kinesiske børns opfattelser af reklamer og varemærker på tv. 725 drenge og 755 piger.
#anvender homogenitetstest.
tabel_reklamer <- rbind(c(180,260,137,96, 52), c(210, 266, 145, 85, 49))
rownames(tabel_reklamer) <- c("Drenge", "Piger")
colnames(tabel_reklamer) <- c("Meget positiv", "Positiv", "Neutral", "Negativ", "Meget negativ")
tabel_reklamer
tabel_reklamer_test <- chisq.test(tabel_reklamer, correct = FALSE)
tabel_reklamer_test


#Opgave 67
#lav homogenitetstest på compulsive buyers:
Compulsive_buyers <- read.csv("Data/compulsiveBuyers.csv")
View(Compulsive_buyers)
tabel_compulsive <- table(Compulsive_buyers$Gender, Compulsive_buyers$Compulsive)
tabel_compulsive
tabel_compulsive_test <- chisq.test(tabel_compulsive, correct = FALSE)
tabel_compulsive_test

#Opgave 68
#Politiken udgav artikel om at der ingen forskel burde være 
#1 opstil nulhypotese svarende til DBU's logik

#2 find de forventede værdier under nulhypotesen

dbu_tabel <- rbind(c(56,51,32,10))
rownames(dbu_tabel) <- c("Antal spillere")
colnames(dbu_tabel) <- c("1", "2", "3", "4")
dbu_tabel
dbu_test <- chisq.test(dbu_tabel, correct = FALSE)
dbu_test$expected

#3 3. Udfør et goodness of fit test af hypotesen fra delspm 1. Angiv teststørrelse
#amt p-værdi. Har DBU ret i, at der er en underrepræsentation af spillere født
#ent p˚a ˚aret? Eller kan du ikke forkaste deres logik/h˚ab om, at spillerne er
#igeligt fordelt ud over ˚aret?
dbu_test_gof<-chisq.test(dbu_tabel, p=c(0.25,0.25,0.25,0.25))
dbu_test_gof


#opgave 69
#datasættet Cuckoos
#1. Giv en passende numerisk og grafisk oversigt af data. Kunne det tyde på, at
#der er forskel på længden af gøgeæg på tværs af de forskellige fuglearter, der
#står for udrugningen?

library(resampledata3)
View(Cuckoos)
#numerisk visning:
tapply(Cuckoos$Egg, Cuckoos$Bird, summary)
#visuel visning:Bird
boxplot(Eggs ~ Bird, data = Cuckoos, main = "Længde af gøgeæg på tværs af fuglearter", xlab = "Fugleart", ylab = "Længde af gøgeæg")
png("Cuckoos_boxplot.png", width = 800, height = 600)
boxplot(Eggs ~ Bird, data = Cuckoos, main = "Længde af gøgeæg på tværs af fuglearter", xlab = "Fugleart", ylab = "Længde af gøgeæg")
dev.off()

#2. Lav et ANOVA-test for at undersøge, om middellængen af gøgeæggene afhænger
#af hvilken fugleart, der udruger ægget. Husk at skrive nulhypotese samt
#alternativ hypotese op. Hvad konkluderes p˚a et 5% signifikansniveau?

Cuckoos$Bird <- as.factor(Cuckoos$Bird)
View(Cuckoos)
Cuckoos_model <- lm(Eggs ~ Bird, data = Cuckoos)
anova(Cuckoos_model)

#3.	Vurder om antagelserne for et ANOVA-test er opfyldt. Stoler du på dine konklusioner?
# jeg laver residualplot: 
par(mfrow=c(1,2))
plot(Cuckoos_model, which = c(1,2))
png("Cuckoos_residualplots.png", width = 800, height = 600)
par(mfrow=c(1,2))
plot(Cuckoos_model, which = c(1,2))
dev.off()


#opgave 70
# vi vil undersøge om der er sammenhæng mellem hvor glad man er og hvor mange venner man har
#1 opstil relevant nulhypotese og alternativ hypotese

#2. Lav for hver af de tre grupper relevante plots til at vurdere, om antallet af
#venner kan antages at følge en normalfordeling. Vurder desuden, om der can
#antages at være konstant varians p˚a tværs af grupperne. Er antagelserne til
#et ANOVA-test opfyldt?

happy_friends <- read.csv("Data/gss_happy_numfriends.csv", header = TRUE, stringsAsFactors = TRUE)
View(happy_friends)
happy_friends$HAPPY <- as.factor(happy_friends$HAPPY)
#først viser jeg de tre gruppers qq plot side om side
par(mfrow=c(1,3))
#gruppe 1 Very happy
model_very_happy <- lm(NUMFREND ~ 1, data = happy_friends, subset = HAPPY == "Very happy")
plot(model_very_happy, which = 2, main = "QQ-plot for Very happy")
#gruppe 2 Pretty happy
model_pretty_happy <- lm(NUMFREND ~ 1, data = happy_friends, subset = HAPPY == "Pretty happy")
plot(model_pretty_happy, which = 2, main = "QQ-plot for Pretty happy")
#gruppe 3 Not too happy
model_not_too_happy <- lm(NUMFREND ~ 1, data = happy_friends, subset = HAPPY == "Not too happy")
plot(model_not_too_happy, which = 2, main = "QQ-plot for Not too happy")
png("happy_friends_qqplots.png", width = 1200, height = 400)
par(mfrow=c(1,3))
model_very_happy <- lm(NUMFREND ~ 1, data = happy_friends, subset = HAPPY == "Very happy")
plot(model_very_happy, which = 2, main = "QQ-plot for Very happy")
#gruppe 2 Pretty happy
model_pretty_happy <- lm(NUMFREND ~ 1, data = happy_friends, subset = HAPPY == "Pretty happy")
plot(model_pretty_happy, which = 2, main = "QQ-plot for Pretty happy")
#gruppe 3 Not too happy
model_not_too_happy <- lm(NUMFREND ~ 1, data = happy_friends, subset = HAPPY == "Not too happy")
plot(model_not_too_happy, which = 2, main = "QQ-plot for Not too happy")
dev.off()

#undersøger konstant varians ved at lave tapply
tapply(happy_friends$NUMFREND, happy_friends$HAPPY, sd)


#3. Udført et permutationstest til at vurdere, om der er en sammenhæng mellem,
#hvor glad man er, og hvor mange venner man har. Hvad kan du konkludere
#p˚a et 5% signifikansniveau?

#finder først teststørrelsen for det observerede data:
F_obs <- anova(lm(NUMFREND ~HAPPY, data = happy_friends))$F[1]
#opsætter permutation
B <- 10^5 -1
n <- nrow(happy_friends)
F_obs_star <- numeric(B)
for(i in 1:B){
    #bland indeks tilfældigt uden tilbagelægning
    index <- sample(n, replace = FALSE)
    #beregn teststørrelsen for det blandede data
    F_obs_star[i] <- anova(lm(NUMFREND[index]~ HAPPY, data = happy_friends))$F[1]
}
#beregn p-værdi
pval <- (1+ sum(F_obs_star >= F_obs))/(B+1)
pval

#opgave 71

#indtaster data fra opgaven:
n_g <- c(41,166,215)
mean_g <- c(12.1,6.4,6.2)
sd_g <- c(21.3, 10.2, 14.0)

#beregner hjælpestørrelser
G <- length(n_g)    #Antal grupper (3)
n <- sum(n_g)   #Total antal observationer (41+166+215)
var_g <- sd_g^2     #Varians for hver gruppe

#2 beregner samlet gennemsnit:
mean_tot <- sum(n_g * mean_g)/n

#3 beregner kvadratsummer (SS):
#SSTR (variation mellem grupper)
SSTR <- sum(n_g * (mean_g - mean_tot)^2)

#SSE (variation inden for grupper)
SSE <- sum((n_g - 1) * var_g)

#4. beregner mean squares (MS):
MSTR <- SSTR/(G-1)
MSE <- SSE/(n-G)
#beregn teststørrelse F:
F_obs <- MSTR/MSE
p_value <- pf(F_obs, df1 = G-1, df2 = n-G, lower.tail = FALSE)

#vis resultater:
c(F_obs = F_obs, p_value = p_value)
