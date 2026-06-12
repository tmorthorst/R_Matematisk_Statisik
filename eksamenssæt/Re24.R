#opgave 1.1

mu0 <-0
xbar <-0.171
n<-353
s <- 1.803
tobs <- (xbar-mu0)/(s/sqrt(n))
p_værdi <- 2*(1-pt(abs(tobs),df=n-1))
p_værdi
tobs

n_Fisker <- 271
xbar_Fisker <- -0.030
S_Fisker <- 0.299
tobs_Fisker <- (xbar_Fisker-mu0)/(S_Fisker/sqrt(n_Fisker))
p_værdi_Fisker <-2*(1-pt(abs(tobs_Fisker),df = n_Fisker -1))
p_værdi_Fisker
tobs_Fisker

#opgave 1.2
xbar_novo <- 0.171
n_novo <- 353
s_novo <- 1.803
xbar_fisker <- -0.030
n_fisker <- 271
s_fisker <- 0.299

T_obs <- (xbar_novo-xbar_fisker)/sqrt(s_novo^2/n_novo+s_fisker^2/n_fisker)

frihedsgrader<- (s_novo/n_novo+s_fisker/n_fisker)^2 /(((s_novo^2/n_novo)^2)/(n_novo-1)+((s_fisker^2/n_fisker)^2 )/(n_fisker-1))

p_værdi <- 2*(1-pt(abs(T_obs),df=frihedsgrader))
T_obs
frihedsgrader
p_værdi

#opgave 2.1
løndata<-read.csv("Data/Løndata.csv")
View(løndata)

#fitter multi lin reg, med logløn som respons:
Multi_logløn <- lm(LogLøn ~., data = løndata)
summary(Multi_logløn)

#modelkontrol
##qqplot for normalfordelingen af residualer:
par(mfrow=c(1,2))
qqnorm(Multi_logløn$residuals, main = "qqplot for normalfordelingen af residualerne")
qqline(Multi_logløn$residuals, col = 'red', lwd=2)

#residualer vs fitted værdier:
plot(Multi_logløn$fitted.values, Multi_logløn$residuals, pch=19, main = "residualer vs fitted værdier", xlab= "fittede værdier",ylab ="residualer")
abline(h=0,col =2)

#residualplot mod de enkelte forklarende kvantitative x-variable.
#uddannelse:
plot(løndata$Uddannelse, Multi_logløn$residuals,pch=19,xlab = "års uddannelse", ylab = "residualer", main = "residualer mod års uddannelse")
abline(h=0,col=2)
#erfaring:
plot(løndata$Erfaring, Multi_logløn$residuals, pch=19, xlab = "års erfaring", ylab = "residualer", main = "residualer mod års erfaring")
abline(h=0,col = 2)

#opgave 2.2
løndata<-read.csv("Data/Løndata.csv")
Multi_logløn <- lm(LogLøn ~., data = løndata)
summary(Multi_logløn)

Multi_logløn_R1 <-lm(LogLøn~.-IkkeHvid, data = løndata)
summary(Multi_logløn_R1)

#opgave 2.3
#konfidensinterval for parameteren beta3:
confint(Multi_logløn_R1, level =0.95)

#opgave 3
tabel_livskvalitet <- rbind(c(11,115,195),c(18,125,151))
rownames(tabel_livskvalitet)<- c("18-29","30-39")
colnames(tabel_livskvalitet) <- c("Lav", "Middel","Høj")
tabel_livskvalitet
test_homo <- chisq.test(tabel_livskvalitet, correct = FALSE)
test_homo
test_homo$residuals
test_homo$expected

livskval_ung <- (1*11+2*115+3*195)/321
livskval_gammel <- (18*1+125*2+151*3)/294
livskval_ung;livskval_gammel

#opgave 4.3
obsdata<- read.csv("Data/obsData24re.csv")
View(obsdata)
observationer <- obsdata$observationer
n <- length(observationer)
mle <- -n/sum(log(observationer))-1
mle
moment <- (1-2*mean(observationer))/(mean(observationer)-1)
moment
#bootstrap percentil:
N<-10^5 -4
mle.boot <- numeric(N)
moment.boot <- numeric(N)
for(i in 1:N){
  bootsample <- sample(observationer, size = n, replace = TRUE)
  mle.boot[i]<- -n/sum(log(bootsample))-1
  moment.boot[i] <- (1-2*mean(bootsample))/(mean(bootsample)-1)
}
quantile(mle.boot,c(0.025,0.975))
quantile(moment.boot,c(0.025,0.975))

#opgave 4.4
#bootstrap for bias og varians
#benytter forrige bootstrap.
MLE_bias <- mean(mle.boot)-mle
MLE_bias
MLE_varians <- var(mle.boot)
MLE_varians
moment_bias <- mean(moment.boot)-moment
moment_varians <- var(moment.boot)
moment_bias;moment_varians

mle_MSE <- MLE_varians + MLE_bias^2
mle_moment <-moment_varians + moment_bias^2
mle_MSE;mle_moment
