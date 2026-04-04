# ============================================================
# UGE 7: MATEMATISK STATISTIK
# ============================================================

# --- OPGAVE 37 ----------------------------------------------



#opgave 37
#I denne opgave kigger vi igen på datasættet Salgspriser.
#1 Tegn et histogram og et qqplot. (sammenligning med normalfordelingen) for
#de observerede værdier af salgsprisen.

Salgspriser <- read.csv("DataMac/Salgspriser.csv")

hist(Salgspriser$Salgspris, breaks = 20, prob = T, main = "Histogram af Salgspriser")
png("histogram.png", width= 800, height = 600)
hist(Salgspriser$Salgspris, breaks = 20, prob = TRUE, main = "Histogram af Salgspriser")
dev.off()

#tegn qqplot 
qqnorm(Salgspriser$Salgspris, main = "QQ-plot af Salgspriser")
qqline(Salgspriser$Salgspris, col = "red")
png("qqplot.png", width= 800, height = 600)
qqnorm(Salgspriser$Salgspris, main = "QQ-plot af Salgspriser")
qqline(Salgspriser$Salgspris, col = "red")
dev.off()


#2 2. Fremstil et approksimativt 95% konfidensinterval for middelværdien af salgsprisen.

alpha <- 0.05
n<- length(Salgspriser$Salgspris)
X_bar <- mean(Salgspriser$Salgspris)
s <- sd(Salgspriser$Salgspris)
z_fraktil <- qnorm(1-alpha/2)

L <- X_bar - z_fraktil * (s/sqrt(n))
U <- X_bar + z_fraktil * (s/sqrt(n))
paste("95% konfidensinterval for middelværdien af salgsprisen:", L, "til", U)


#3. Fremstil et 95% bootstrap konfidensinterval for middelværdien af salgsprisen.
#kør bootstrap simuleringer for at finde konfidensintervallet
N <- 10^5
mean_boot <- numeric(N)
for (i in 1:N) {
    bootsample <- sample(Salgspriser$Salgspris, size = n, replace = TRUE)
    mean_boot[i] <- (mean(bootsample)-X_bar)/ (s/sqrt(n))

}
#find kvantilerne for bootstrap fordelingen
q1 <- quantile(mean_boot, probs = alpha/2)
q2 <- quantile(mean_boot, probs = 1-alpha/2)
#beregn konfidensintervallet
L <- X_bar - q2 * (s/sqrt(n))
U <- X_bar - q1 * (s/sqrt(n))
paste("95% bootstrap konfidensinterval for middelværdien af salgsprisen:", L, "til", U)


#opgave 41
#Amerikansk stikprøve på 583 viser gennemsnitlig 43,5 timer arbejde om ugen. 
#Standardafvigelsen er 15,3 timer, og det ugentlige antal arbejdstimer er normalfordelt.
#En standard uge er defineret til 40 timer. Test 5% signifikansniveau om middelværdien
#af amerikanernes arbejdsuge adskiller sig fra 40 timer. 

#
n <- 583 
alpha <- 0.05
X_bar <- 43.5
s <- 15.3
mu_0 <- 40
t_obs <- (X_bar - mu_0) / (s/sqrt(n))
p_værdi <- 2* (1-pt(abs(t_obs), df = n-1))
paste("Teststatistik:", t_obs)
paste("P-værdi:", p_værdi)



#opgave 42
#gennemsnitlig værdi af aktier på NASDAQ 2.jan 2017 = 30.29 USD. 
#Vores datasæt af Nasdaq indeholder 50 tilfædlige udvalgte aktier som ved åbningsdag 01.12/17
#havde en gennemsnitlig værdi på 23.29 USD. 

#1. Tegn histogram og qqplot af de 50 aktieværdier. beskriv fordelingen og afgør om den er normalfordelt.

library(resampledata3)
View(Nasdaq)
hist(Nasdaq$Open, breaks= 20, prob = T, main = "Histogram af NASDAQ aktieværdier")
png("histogram_nasdaq.png", width= 800, height = 600)
hist(Nasdaq$Open, breaks= 20, prob = T, main = "Histogram af NASDAQ aktieværdier")
dev.off()

#tegn qqplot 
qqnorm(Nasdaq$Open, main = "QQ-plot af NASDAQ aktieværdier")
qqline(Nasdaq$Open, col = "red")
png("qqplot_nasdaq.png", width= 800, height = 600)
qqnorm(Nasdaq$Open, main = "QQ-plot af NASDAQ aktieværdier")
qqline(Nasdaq$Open, col = "red")
dev.off()


#2 benyt et bootstrap t test til på et 5% signifikansniveau at teste om middelværdien 
#af aktieværdierne d. 1. december adskiller sig fra 30.29 USD.

n <- length(Nasdaq$Open)
alpha <- 0.05
X_bar <- mean(Nasdaq$Open)
s <- sd(Nasdaq$Open)
mu_0 <- 30.29
t_obs <- (X_bar - mu_0) / (s/sqrt(n))
N <- 10^5
T_star <- numeric(N)
for (i in 1:N) {
    bootsample <- sample(Nasdaq$Open, size = n, replace = TRUE)
    X_star <- mean(bootsample)
    s_star <- sd(bootsample)
    T_star[i] <- (X_star - X_bar)/ (s_star * sqrt(n))
}

#beregner p værdi
p_værdi <- 2* min( (sum(T_star>=t_obs)+1)/(N+1), (sum(T_star<=t_obs)+1)/(N+1) ) 
p_værdi


#Prøv ogs˚a at benytte et t–test i stedet. Sammenlign med resultatet fra spørgsm˚al 2

t_test <- t.test(Nasdaq$Open, mu = mu_0)
t_test



#opgave 43
# i 1987 var 39.1% af sexarbejdere i Bamako, Mali, smittet med HIV.
# Der testes 130, hvoraf 48% testes positiv. Er dette tilstrækkeligt 
#bevis for at HIV smitteprocenten har ændret sig.
# benyt eksakt test og z-test med 5% signifikansniveau. Sammenlign resultaterne.

#eksakt test
n <- 130
alpha <- 0.05
p0 <- 0.391
x_obs <- round(0.48*n)
p_værdi_eksakt <- 2* sum(dbinom(x_obs:n, size = n, prob = p0))
p_værdi_eksakt

#z-test
zobs <- (x_obs- n*p0)/sqrt(n*p0*(1-p0))
2*dnorm(zobs)
