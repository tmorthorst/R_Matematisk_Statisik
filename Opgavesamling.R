stikprøve <- c(3,5,8,15,20,21,24)
mean(stikprøve)

median(stikprøve)

log_stikprøve <-log(stikprøve)
mean(log_stikprøve)
median(log_stikprøve)

log(mean(stikprøve))
log(median(stikprøve))


library(resampledata3)

view(Spruce)

#1. Benyt summary til at beregne numeriske opsummeringer af variablen 
#for højdeændringer kaldet Ht.change.

summary(Spruce$Ht.change)

hist(Spruce$Ht.change,prob =TRUE,breaks = 8)

qqnorm(Spruce$Ht.change, main='QQ-plot over Height change')
qqline(Spruce$Ht.change)

boxplot(Di.change~Fertilizer,Spruce, ylab = "Ændring i diameter",
        main ='Boxplot Spruce data')

tapply(Spruce$Di.change,Spruce$Fertilizer, mean)
tapply(Spruce$Di.change,Spruce$Fertilizer, median)

plot(Ht.change~Di.change,Spruce,pch=19,main ="Scatterplot: Hvordan afhænger
     højdeændringer af diameterændringerne")

qexp(0.05,4,lower.tail =T)
log(0.95)/-4


x <- 0:20
pbinom(x,20,0.3)


x<- rnorm(1000)

hist(x,breaks=10, prob = TRUE)
qqnorm(x)
qqline(x)


tider <- c(26,21,31,22,16)
# 2. Find alle måder at vælge 3 personer ud af 5 (giver en matrix med 10 kolonner)
# Vi bruger indeks 1-5 for at holde styr på, hvem der er hvem
kombinationer <- combn(1:5,3)
kombinationer

resultater <- apply(kombinationer,2, function(indeks){
  kaffe_gruppe<-tider[indeks] #de 3 udvalgte
  kontrol_gruppe <- tider[-indeks] #de 2 resterende (minus-indeks fjerner dem)
  forskel <- mean(kaffe_gruppe)-mean(kontrol_gruppe)
  return(forskel)
})
print(resultater)



# 1. Indtast dine data igen
tider <- c(26, 21, 31, 22, 16)

# 2. Beregn den observerede forskel (den vi fandt i spg. 1)
# (De første 3 fik kaffe, de sidste 2 fik ikke)
obs_forskel <- mean(tider[1:3]) - mean(tider[4:5])

# 3. Klargør simulationen
M <- 10^4 - 1  # Vi skal lave 9999 tilfældige træk
perm_forskelle <- numeric(M) # En tom liste til at gemme resultaterne

# 4. Kør løkken M gange
for(i in 1:M) {
  # Bland tiderne tilfældigt (permutation)
  blandet_tider <- sample(tider)
  
  # Beregn forskellen på den nye, tilfældige opdeling
  # (Gennemsnit af de første 3 minus gennemsnit af de sidste 2)
  perm_forskelle[i] <- mean(blandet_tider[1:3]) - mean(blandet_tider[4:5])
}

# 5. Beregn P-værdien
# Vi tæller, hvor mange gange den "falske" forskel var lige så stor eller større end den rigtige.
# Vi lægger 1 til i både tæller og nævner for at inkludere vores egen observation (obs_forskel).
p_vaerdi <- (sum(perm_forskelle >= obs_forskel) + 1) / (M + 1)

# Vis resultatet
p_vaerdi


View(Salgspriser)

gns_hus <- mean(Salgspriser$Kvadratmeterpris[Salgspriser$Hustype=="Hus"], na.rm=TRUE)
gns_rækkehus <-mean(Salgspriser$Kvadratmeterpris[Salgspriser$Hustype=="Rækkehus"],na.rm=T)
gns_forskel <-gns_hus - gns_rækkehus

N<-10000
simulerede_forskelle <- replicate(N, {
  # Bland (shuffle) hustyperne tilfældigt
  blandet_type <- sample(Salgspriser$Hustype)
  # Beregn gennemsnit for de nye tilfældige grupper
  ny_hus <- mean(Salgspriser$Kvadratmeterpris[blandet_type == "Hus"], na.rm = TRUE)
  ny_raekke <- mean(Salgspriser$Kvadratmeterpris[blandet_type == "Rækkehus"], na.rm = TRUE)
  # Returnér forskellen
  ny_hus - ny_raekke
})

# 3. Beregn P-værdi (Tosidet test)
# Hvor stor en andel af de simulerede forskelle er mere ekstreme end den observerede?
p_vaerdi <- mean(abs(simulerede_forskelle) >= abs(obs_forskel))
p_vaerdi
simulerede_forskelle


#opgave 17
n <-200
N <-10^5

betastik <- rbeta(200,2,3)

hist(betastik,breaks=20,prob=T)
mean(betastik)
sd(betastik)

mean.boot <- numeric(N)
for (i in 1:N){
  bootsample <-sample(betastik, size = 200, replace = T)
  mean.boot[i]<-mean(bootsample)
}
hist(mean.boot, prob =T, breaks = 20)
mean(mean.boot)
sd(mean.boot)


mean.sim <-numeric(N)
for (i in 1:N) {
  simsample <-rbeta(200, 2,3)
  mean.sim[i]<-mean(simsample)
}
hist(mean.sim,breaks = 20, prob = T)
mean(mean.sim)
sd(mean.sim)


betastik <- rbeta(50,2,3)

hist(betastik,breaks=20,prob=T, main = "histogram med n = 50")
mean(betastik)
sd(betastik)

mean.boot <- numeric(N)
for (i in 1:N){
  bootsample <-sample(betastik, size = 50, replace = T)
  mean.boot[i]<-mean(bootsample)
}
hist(mean.boot, prob =T, breaks = 20, main ="hist med n = 50 bootstrap")
mean(mean.boot)
sd(mean.boot)


mean.sim <-numeric(N)
for (i in 1:N) {
  simsample <-rbeta(50, 2,3)
  mean.sim[i]<-mean(simsample)
}
hist(mean.sim,breaks = 20, prob = T, main = "hist med n = 50 og mange forskellige udtræk")
mean(mean.sim)
sd(mean.sim)



#Opgave 18
bilpriser <- read.csv("Data/Bilpriser.csv")
View(bilpriser)
hist(bilpriser$Pris,breaks=25,prob=T)
summary(bilpriser)

#gennemsnitspris:
mean(bilpriser$Pris)
#gennemsnit af diesel og ikke diesel:
gns<-tapply(bilpriser$Pris,bilpriser$Diesel,mean)
gns
#difference:
diff(gns)


#b Find bootstrapfordeling og beregn gennemsnit og tegn histogram:
#dernæst find empirisk middelværdi og lav et 95% bootstrap percentilinterval
boot_sample <- sample(bilpriser$Pris, 1125, replace=T)
mean(boot_sample)
hist(boot_sample, main= "histogram over bootstrapfordeling af pris",breaks = 25,prob = T)

N <- 10^5 


mean_boot_bil <- numeric(N)
for(i in 1:N){
  boot_sample <- sample(bilpriser$Pris,1125,replace=T)
  mean_boot_bil[i]<-mean(boot_sample)
}
mean(mean_boot_bil)
# 95% bootstrap percentilinterval (fra 2,5% til 97,5%)
quantile(mean_boot_bil, probs=c(0.025,0.975))


#Find bootstrap for fordelingen mellem den gennemsnitlige pris for diesel og benzin.
#Dernæst den empiriske.

N <- 10000 

# Bruger nu "ja" og "nej" med småt, så det matcher dit datasæt
pris_diesel <- subset(bilpriser, Diesel == "ja")$Pris
pris_benzin <- subset(bilpriser, Diesel == "nej")$Pris

nDiesel <- length(pris_diesel)
nBenzin <- length(pris_benzin)

meandiff.boot <- numeric(N)

for(i in 1:N){
  bootsampleDiesel <- sample(pris_diesel, size = nDiesel, replace = TRUE)
  bootsampleBenzin <- sample(pris_benzin, size = nBenzin, replace = TRUE)
  meandiff.boot[i] <- mean(bootsampleDiesel) - mean(bootsampleBenzin)
}

# Tegner histogrammet
hist(meandiff.boot, breaks = 25, prob = TRUE,
     main = "Bootstrapfordeling af prisforskel", xlab = "Prisforskel")

#finder empirisk gennemsnit:
mean(meandiff.boot)

quantile(meandiff.boot,probs=c(0.025,0.975))
