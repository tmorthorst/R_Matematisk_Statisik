#eksamen 2025
#opgave 1.1
tabel_meningsmåling <-rbind(c(257,283,314),c(247,316,292))
rownames(tabel_meningsmåling)<-c("Decemeber2024","Marts2025")
colnames(tabel_meningsmåling)<-c("Venstre for reg.","Regering","Højre for reg")
tabel_meningsmåling
test_meningsmåling_homo <-chisq.test(tabel_meningsmåling,correct = FALSE)
test_meningsmåling_homo
#opgave 1.2

samlet <- rbind(c(504,509,606))
rownames(samlet)<- "Samlet meningsmåling"
colnames(samlet)<- c("Venstre for reg.","Regering","Højre for reg")
samlet

chisq.test(samlet, p=c(0.209,0.509,0.282))

#opgave 2.1
bilpriser <-read.csv("../Data/Bilpriser2025.csv")
View(bilpriser)

X_bar <- mean(bilpriser$Pris)
X_bar
S<-sd(bilpriser$Pris)
n<-nrow(bilpriser)
t_obs <- qt(0.975, df = n-1)
#KI:
Lki <- X_bar -t_obs*S/sqrt(n)
Uki <- X_bar + t_obs*S/sqrt(n)
c(Lki,Uki)
paste0("[",round(Lki,5)," ; ",round(Uki,5),"]")

#opgave 2.2
par(mfrow=c(1,1))
qqnorm(bilpriser2025$Pris)
qqline(bilpriser2025$Pris,col='red',lwd=2)
#eller tegne histogram

#opgave 2.3
#bootstrap konfidensinterval:
alpha <-0.05
Pris <- bilpriser2025$Pris
X_bar <- mean(Pris)
S <-sd(Pris)
n <- length(Pris)
N <- 10^5-1
T_star <- numeric(N)

for(i in 1:N){
  x <- sample(Pris,size = n, replace = T)
  T_star[i] <-(mean(x)-X_bar)/ (sd(x)/sqrt(n))
}

q1 <-quantile(T_star,c(alpha/2),names =FALSE)
q2 <-quantile(T_star,c(1-alpha/2),names =FALSE)
#udregn KI:
X_bar -q2*S/sqrt(n);X_bar-q1*S/sqrt(n)

#opgave 3
lin_reg_logpris_km <- lm(LogPris~Km, data = bilpriser)
lin_reg_logpris_km
summary(lin_reg_logpris_km)

par(mfrow=c(1,2))
qqnorm(lin_reg_logpris_km$residuals,pch=19)
qqline(lin_reg_logpris_km$residuals, col='red')
plot(bilpriser$Km, lin_reg_logpris_km$residuals, pch=19)
abline(h=0,col='red')

par(mfrow=c(1,1))
#opgave 3.2
multi_km_brændstof <- lm(LogPris~ Km*Brændstof, data = bilpriser2025)
summary(multi_km_brændstof)
multi_km_brændstof_uden_veksel <- lm(LogPris~Km+Brændstof,data = bilpriser2025)
anova(multi_km_brændstof_uden_veksel,multi_km_brændstof)

#opgave 3.3
ny_obs <- data.frame(Km =150, Brændstof = "diesel")
predict(multi_km_brændstof_uden_veksel, newdata= ny_obs, interval = "confidence", level =0.95)

#prædiktionsinterval:
predict(multi_km_brændstof_uden_veksel,newdata=ny_obs, interval = "prediction",level=0.95)


#opgave 4.3
obs_data <- read.csv("../Data/obsData.csv")
View(obs_data)
observationer <- obs_data$observationer
mle<-1/mean((observationer-1)^2/observationer)
n <- length(observationer)
mle_selv <- (1/n*sum(((observationer-1)^2)/observationer))^-1
mle
mle_selv

beta_tilde <- (1/n*sum(observationer^2)-1)^-1
beta_tilde

#bootstrap algoritme
N<- 10^5-1
mle.boot <- numeric(N)
moment.boot <- numeric(N)
for(i in 1:N){
  bootsample <- sample(observationer, size = n, replace = TRUE)
  mle.boot[i]<- 1/mean((bootsample-1)^2/bootsample)
  moment.boot[i] <- (1/n*sum(bootsample^2)-1)^(-1)
}
#bias og varians for mle:
bias_mle <- mean(mle.boot)-mle
var_mle <- var(mle.boot)
bias_mle;var_mle
#bias og variasns for moment:
bias_moment <- mean(moment.boot)-beta_tilde
var_moment <- var(moment.boot)
bias_moment;var_moment

MSE_mle <- var_mle +bias_mle^2 
MSE_mle
MSE_moment <- var_moment + bias_moment^2
MSE_moment

#opgave 4.4
alpha <-0.05
mle<-1/mean((observationer-1)^2/observationer)
n<-length(observationer)
q<-qnorm(1-alpha/2)
#konfidensintervallet:
Lki <- mle - q*mle*sqrt(2/n)
Uki <-mle +q*mle*sqrt(2/n)
Lki;Uki
