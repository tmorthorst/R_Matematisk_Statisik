#opgave 1.1
tabel <- rbind(c(17738,4457),c(17795,2530))
rownames(tabel) <- c("2021(før)","2022 (efter)")
colnames(tabel)<- c("Afholdt barsel","Ikke afholdt barsel")
prop.table(tabel, 1)
test_homo <- chisq.test(tabel,correct = FALSE)
test_homo

#opgave 1.2
x1 <- tabel[2,1] # antal succeser (afholdt barsel)
n1 <- sum(tabel[2,]) # samlet antal i gruppen
p1_tilde <- (x1+1)/(n1+2)

x2 <- tabel[1,1] 
n2 <- sum(tabel[1,])
p2_tilde <- (x2+1)/(n2+2)
alpha<-0.05
q <- qnorm(1-0.05/2)
Lki<- p1_tilde-p2_tilde - q*sqrt((p1_tilde*(1-p1_tilde)/n1)+(p2_tilde*(1-p2_tilde))/n2)
Uki <-p1_tilde-p2_tilde + q*sqrt((p1_tilde*(1-p1_tilde)/n1)+(p2_tilde*(1-p2_tilde))/n2)
Lki;Uki


#opgave 2.1

husstande <- read.csv("Data/Husstande.csv")
View(husstande)

gns_eje <- mean(husstande$LogOmkostning[husstande$Boligform=="eje"])
gns_leje <-mean(husstande$LogOmkostning[husstande$Boligform=="leje"])
gns_eje - gns_leje

husstande$Boligform <- as.factor(husstande$Boligform)
model <- lm(LogOmkostning ~Boligform, data = husstande)
anova(model)

#permutationstest 
F_obs <- anova(model)$F[1]
F_obs
#opsætter permutation:
N <- 10^5 -1
n<- nrow(husstande)
F_obs_star <- numeric(N)

for(i in 1:N){
  # Bland indeks tilfældigt uden tilbagelægning
  index <- sample(n, replace=FALSE)
  #beregner F-størrelse for det blendede data
  F_obs_star[i]<- anova(lm(LogOmkostning[index]~Boligform, data = husstande))$F[1]
}
#beregner p-værdi:
p_værdi <- (1+sum(F_obs_star>=F_obs))/(N+1)
p_værdi

#opgave 3
model_omk_indk <-lm(LogOmkostning~LogIndkomst, data = husstande)
summary(model_omk_indk)

confint(model_omk_indk,level=0.95)
#3.2
par(mfrow=c(1,2))
qqnorm(model_omk_indk$residuals,main = "qqplot for normalfordelingen af residualerne")
qqline(model_omk_indk$residuals, col = 2)

plot(model_omk_indk$fitted.values, model_omk_indk$residuals, xlab = "fitted values", ylab ="residualer", main = "fitted værdier mod residualer")
abline(h=0,col='red', lwd=2)

#opgave 3.3
model_omk_indk_sove <- lm(LogOmkostning~LogIndkomst + Soveværelser, data = husstande)
summary(model_omk_indk_sove)

#opgave 4
tapply(husstande$LogOmkostning,husstande$Urbanisering,mean)
tapply(husstande$LogOmkostning,husstande$Urbanisering,sd)

type1 <- subset(husstande, Urbanisering=='type1')$LogOmkostning
type2 <- subset(husstande, Urbanisering =='type2')$LogOmkostning
type3 <- subset(husstande, Urbanisering =='type3')$LogOmkostning
type4 <- subset(husstande, Urbanisering =='type4')$LogOmkostning
type5 <- subset(husstande, Urbanisering =='type5')$LogOmkostning

mean(type1);sd(type1)
mean(type2);sd(type2)
mean(type3);sd(type3)
mean(type4);sd(type4)
mean(type5);sd(type5)

model_omk_urb <- lm(LogOmkostning~Urbanisering, data = husstande)
husstande$Urbanisering <- as.factor(husstande$Urbanisering)
model_omk_urb <- lm(LogOmkostning~Urbanisering, data = husstande)
anova(model_omk_urb)


#opgave 5
observationer <- c(1.75, 1.51, 0.97, 0.32, 1.54, 1.14, 0.49, 0.26, 0.43,
                   0.39, 0.15, 0.28, 2.01, 0.37, 0.76, 0.53, 1.10, 0.65,
                   0.70, 0.81, 0.59, 1.15, 1.33, 1.49, 0.63, 2.14, 1.19,
                   1.25, 1.31, 0.64, 0.94, 1.42, 0.07, 0.65, 0.53, 0.82,
                   0.81, 0.41, 2.31, 0.55, 1.29, 1.18, 1.23, 0.91, 0.97,
                   1.83, 1.12)
n<-length(observationer)
MLE <- 1/n * sum(observationer^2)
MLE

N <- 10^5-4
mle.boot <-numeric(N)
for(i in 1:N){
  bootsample <- sample(observationer, size = n, replace = TRUE)
  mle.boot[i]<- 1/n*sum(bootsample^2)
}
mle_varians <- var(mle.boot)
mle_varians

 LogL <- function(beta){
   resultat<-n*log(2)-n*log(beta)+sum(log(observationer))-1/beta*sum(observationer^2)
   return(resultat)
 }
 minus2Q <- 2*LogL(MLE)-2*LogL(1)
minus2Q
p_værdi <- pchisq(minus2Q,df=1, lower.tail = F) 
p_værdi
