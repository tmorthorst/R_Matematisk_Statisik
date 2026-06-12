#opgave 1.1
tabel<- rbind(c(667,648),c(163,79))
rownames(tabel)<-c("Positiv","Negativ")
colnames(tabel)<-c("Test A","Test B")
prop.table(tabel, 2)

#Test A
x_tilde <- tabel[1,1] +2 #antal succes + 2 
n_tilde <- sum(tabel[,1])+4 #antal i test A + 4
p_tilde <- x_tilde/n_tilde
alpha <-0.05
q<- qnorm(1-alpha/2)

Lac <- p_tilde - q*sqrt(p_tilde*(1-p_tilde)/n_tilde)
Uac <- p_tilde + q*sqrt(p_tilde*(1-p_tilde)/n_tilde)
Lac;Uac

#opgave 1.2
#Test A
x1tilde <- tabel[1,1] #antal succes
n1tilde <- sum(tabel[,1]) #antal i gruppen test A
p1tilde <- (x1tilde+1)/(n1tilde +2)
#Test B
x2tilde <- tabel[1,2] #antal succes
n2tilde <- sum(tabel[,2]) #antal i gruppen test B
p2tilde <- (x2tilde +1)/(n2tilde+2)
alpha <-0.05
q<- qnorm(1-alpha/2)

L<-(p1tilde-p2tilde)-q*sqrt((p1tilde*(1-p1tilde))/n1tilde+(p2tilde*(1-p2tilde))/n2tilde)
U<-(p1tilde-p2tilde)+q*sqrt((p1tilde*(1-p1tilde))/n1tilde+(p2tilde*(1-p2tilde))/n2tilde)
L;U

#opgave 2.1
xbar <- 13.2
ybar <- 7.3
s1 <- 8
s2 <- 6
n1 <-16
n2 <- 68
T_obs <- (xbar-ybar)/sqrt((s1^2/n1)+(s2^2/n2))
T_obs
frihedsgrader <-(s1^2/n1+s2^2/n2)^2/((s1^2/n1)^2/(n1-1)+(s2^2/n2)^2/(n2-1))
frihedsgrader
p_værdi <- 2*(1-pt(abs(T_obs),df=frihedsgrader))
p_værdi

#opgave 3.1
laks <- read.csv("Data/laks.csv")
View(laks)
model_PCB_DDT <- lm(PCB~DDT, data =laks)
summary(model_PCB_DDT)

#opgave 3.2
ny_obs <- data.frame(DDT<-35)
predict(model_PCB_DDT, newdata=ny_obs,interval ="prediction",level =0.95)

qqnorm(model_PCB_DDT$residuals, main="qqplot af residualer")
qqline(model_PCB_DDT$residuals, col='red', lwd=2)

plot(model_PCB_DDT$fitted.values,model_PCB_DDT$residuals, main = "residualer vs. fitted værdier", xlab = "fitted værdier", ylab="residualer")
abline(h=0,col='red',lwd=2)



#opgave 4
observationer <- c(3, 10, 3, 1, 5, 5, 6, 2, 2, 4, 6, 9, 7, 1, 4,
                   1, 4, 7, 2, 3, 10, 6, 1, 4, 5, 2, 1, 5, 1, 2)
xbar <- mean(observationer)
MLE <- (xbar-1)/(xbar+1)
MLE

#bootstrap bias
N<-10^5-1
n<- length(observationer)
mle.boot <- numeric(N)
for(i in 1:N){
  bootsample <- sample(observationer, size=1, replace =TRUE)
  mle.boot[i]<-(mean(bootsample)-1)/(mean(bootsample)+1)
}
bias<- mean(mle.boot)-MLE
var_mle <- var(mle.boot)
bias;var_mle
