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


#Opgave 19

#Opret en ny variabel til datasættet, reactiontime, og kald den forskel.
#dernæst lav bootstrap og find 95% percentil interval.

Reaction_Time <- read.csv("Data/ReactionTime.csv")
View(Reaction_Time)

Reaction_Time$Forskel <- Reaction_Time$Ja - Reaction_Time$Nej

View(Reaction_Time)
length(Reaction_Time$Forskel)

N<-10^5

meandiff.boot <- numeric(N)

for( i in 1:N){
  boot_sample <- sample(Reaction_Time$Forskel, size = length(Reaction_Time$Forskel), replace = TRUE)
  meandiff.boot[i] <- mean(boot_sample)
}

#empiriske gennemsnit:
mean(meandiff.boot)

#95% percentilinterval:
quantile(meandiff.boot, probs = c(0.025,0.975))



#opgave 20
#brug datasæt MathAnxiety
#Fremstil et boxplot, der viser matematikangsten for hver af de to grupper af elever.

library(resampledata3)
View(MathAnxiety)

boxplot(AMAS~Grade, MathAnxiety, ylab = "graden af mat angst", 
main = "boxplot over graden af angst for matematik inddelt i primary og secondary school ")

#udregn forholdet mellem den gennemsnitlige matematikangst for de to grupper.

angst_primary <- subset(MathAnxiety, Grade == "Primary")$AMAS
angst_secondary <- subset(MathAnxiety, Grade =="Secondary")$AMAS
gns_primary <- mean(angst_primary)
gns_secondary <- mean(angst_secondary)

gns_secondary/gns_primary

#Lav nu bootstrapfordelingen og tegn histogram
meandiff.boot <-numeric(N)

for(i in 1:N){
  bootsampleprimary <- sample(angst_primary, size = length(angst_primary),replace =TRUE)
  bootsamplesecondary <- sample(angst_secondary, size = length(angst_secondary),replace = TRUE)
  meandiff.boot[i] <- mean(bootsamplesecondary)/mean(bootsampleprimary)
}
mean(meandiff.boot)

hist(meandiff.boot, breaks = 30, probability = TRUE, main = "Histogram af forholdet", xlab ="forholdet")

#find ud af om det er biased og beregn den relative bias
rigtig_middelværdi <- gns_secondary/gns_primary

bias_boot <- mean(meandiff.boot)- rigtig_middelværdi
standardfejl<-sd(meandiff.boot)
bias_boot/standardfejl



#opgave 21
#Beregn andel af biler der blev solgt med en bilforhandler
View(bilpriser)
proportions(table(bilpriser$Sælger))
mean(bilpriser$Sælger=="ja")
#beregn andel biler solgt med en bilforhandler for hver af de to typer af biler
proportions(table(Diesel= bilpriser$Diesel,Bilforhandler=bilpriser$Sælger), margin =1)


#beregn den relative andel 
diesel_biler <- subset(bilpriser, Diesel =="ja")
benzin_biler <- subset(bilpriser, Diesel =="nej")

benzin_biler

andel_diesel
andel_benzin


#andele:
andel_diesel <- mean(diesel_biler$Sælger=="ja")
andel_benzin <- mean(benzin_biler$Sælger =="ja")
#udregn forhold:
relativ_andel <- andel_diesel/andel_benzin
relativ_andel


#opgave b: find bootstrapfordlingen for andelen af biler solgt med forhandler
#tegn et histogram og beregn 95% bootstrap percentilinterval.
N <- 10^5 
boot_andeler <- numeric(N)

for(i in 1:N){
  boot_sample <- sample(bilpriser$Sælger, replace =TRUE)
  #udregner andelen
  boot_andeler[i]<- mean(boot_sample =="ja")
}
#tegner histogram:
hist(boot_andeler,breaks=30, prob =T)

#95% percentilinterval og vist i histogram:
kvantiler <- quantile(boot_andeler, probs=c(0.025,0.975))
kvantiler
abline(v= kvantiler, col ="red")

# Find bootstrapfordelingen for relative andel. Tegn et
#histogram over fordelingen. Beregn bootstrapestimater for bias og standardfejl
#for forholdsestimatoren. 

diesel_biler <- subset(bilpriser, Diesel =="ja")
benzin_biler <- subset(bilpriser, Diesel =="nej")

relativ_andel <- numeric(N)

andel_diesel
for(i in 1:N){
  boot_sample_diesel <- sample(diesel_biler$Sælger =="ja", replace =TRUE)
  boot_sample_benzin <- sample(benzin_biler$Sælger =="ja", replace =TRUE)
  relativ_andel[i] <- mean(boot_sample_diesel)/ mean(boot_sample_benzin)
}
mean(relativ_andel)

hist(relativ_andel,breaks=20, prob = T)

#andele:
andel_diesel <- mean(diesel_biler$Sælger=="ja")
andel_benzin <- mean(benzin_biler$Sælger =="ja")
#udregn forhold:
relativ_andel_rigtig <- andel_diesel/andel_benzin

bias <- mean(relativ_andel)-relativ_andel_rigtig
bias
standardfejl<-sd(relativ_andel)
bias/standardfejl
