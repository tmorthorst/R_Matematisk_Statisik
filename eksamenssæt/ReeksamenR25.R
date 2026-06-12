
#opgave 1.1
# lav anova test
interkontinental <- read.csv("Data/Interkontinental.csv")
View(interkontinental)
sorteret_interkontinental <- interkontinental[order(interkontinental$Destination),]
View(sorteret_interkontinental)
lm_model <- lm(Pris ~ Destination, data = sorteret_interkontinental)
lm_model
anova(lm_model)

#tests
tapply(interkontinental$Pris, interkontinental$Destination, sd)


lm_model_Bangkok <- lm(Pris ~ 1, data = sorteret_interkontinental, subset = Destination =="Bangkok")
par(mfrow=c(1,5))
plot(lm_model_Bangkok, which =2, main = "QQ-plot for Bangkok")
lm_model_Beijing <- lm(Pris ~ 1, data = sorteret_interkontinental, subset = Destination =="Beijing")
plot(lm_model_Beijing, which = 2, main = "QQ-plot for Beijing")
lm_model_Dubai <- lm(Pris ~ 1, data = sorteret_interkontinental, subset = Destination =="Dubai")
plot(lm_model_Dubai, which = 2, main = "QQ-plot for Dubai")
lm_model_Los_Angeles <- lm(Pris ~ 1, data = sorteret_interkontinental, subset = Destination =="Los Angeles")
plot(lm_model_Los_Angeles, which = 2, main = "QQ-plot for Los Angeles")
lm_model_New_York <- lm(Pris ~ 1, data = sorteret_interkontinental, subset = Destination =="New York")
plot(lm_model_New_York, which = 2, main = "QQ-plot for New York")


#opgave 1.2 udregn gennemsnit og lav permutationstest
#gennemsnit:
tapply(sorteret_interkontinental$Pris, sorteret_interkontinental$Destination, mean)

F_obs <- anova(lm_model)$F[1]
B <- 10^4-1
n <- nrow(sorteret_interkontinental)
F_obs_star <- numeric(B)
for (i in 1:B){
  index <- sample(n, replace = FALSE)
  F_obs_star[i] <- anova(lm(sorteret_interkontinental$Pris[index] ~ sorteret_interkontinental$Destination))$F[1]
}
p_val <- (1 + sum(F_obs_star >= F_obs))/(B+1)
p_val

#opgave 2.1 opstil multipel lin reggresion, hvor passager er respons og 
#pris, destination og afgange samt vekselvirkning pris og destination er forklarende
multi_passager <- lm(Passagerer ~ Afgange + Pris * Destination, data = sorteret_interkontinental)
multi_passager
summary(multi_passager)

#opgave 2.2 Argumenter for at ovenstående vekselvirkning ikke har signifikans betydning

multi_passagerer_uden_veksel <- lm(Passagerer ~., data = sorteret_interkontinental)
summary(multi_passagerer_uden_veksel)
anova(multi_passagerer_uden_veksel,multi_passager)

summary(multi_passagerer_uden_veksel)

multi_passagerer_uden_veksel_pris <- lm(Passagerer ~.-Pris, data = sorteret_interkontinental)
summary(multi_passagerer_uden_veksel_pris)

#opgave 2.3 Beregn konfidensintervaller for alle slutmodellens parametre

confint(multi_passagerer_uden_veksel_pris, level = 0.95)

#opgave 3.1
x_bar1 <-56.98
S1<-6.62
n <- 20
t_værdi <- qt(0.975, df= 20-1)
KI <- x_bar1 + c(-1,1)* t_værdi*S1/sqrt(n)
KI

x_bar2 <-58.38
s2 <- 6.41
n <-20
t_værdi <- qt(0.975,df =20-1)
ki2 <- x_bar2 + c(-1,1)*t_værdi *s2/sqrt(n) 
ki2

#opgave 3.2
D_bar <- 56.98-58.38
S_D <- 0.97
n <- 20
mu_0 <- 0

t_obs <- (D_bar-mu_0)/(S_D/sqrt(n))
t_obs
p_værdi <- 2*(1-pt(abs(t_obs),df=n-1))
p_værdi

#opgave 4.3
n<-50
observationer<-c(0.164, 0.072, 0.115, 0.350, 0.160, 0.104, 0.389, 0.274,
                 0.114, 0.116, 1.216, 0.463, 0.050, 0.147, 0.144, 0.088,
                 0.110, 0.242, 0.159, 0.103, 0.141, 0.111, 0.131, 0.129,
                 0.104, 0.073, 0.062, 0.079, 0.067, 0.132, 0.177, 0.068,
                 0.198, 0.245, 0.095, 0.112, 0.133, 0.391, 0.548, 0.508,
                 0.133, 0.090, 0.111, 0.160, 0.071, 0.050, 0.082, 0.597,
                 0.202, 0.069)
alpha_hat <- 1/(3*n)*sum(1/observationer)
alpha_hat

#bootstrap

N <- 10^5-1
mle.boot <-numeric(N)
for(i in 1:N){
  bootsample <- sample(observationer, size = n, replace = TRUE)
  mle.boot[i]<- 1/(3*n)*sum(1/bootsample)
}
var(mle.boot)
#percentil interval:
quantile(mle.boot, c(0.025,0.975))

#4.4
log_likelihood <- function(alpha){
  -n*log(2)-3*n*log(alpha)-4*sum(log(observationer))-1/alpha *sum(1/observationer)
}
l_alpha_hat <- log_likelihood(alpha_hat)
l_alpha <- log_likelihood(3)

#beregner -2logQ(x)
minus2logQ<-2*l_alpha_hat-2*l_alpha
minus2logQ
#beregner p-værdi
pchisq(minus2logQ,df =1, lower.tail = FALSE)
