#uge 6
#opgave 28
#Vi tænker os en stikprøve bestående af n = 55 normalfordelte observationer fra en
#population med kendt varians på σ^2 = 4 og ukendt middelværdi μ. I stikprøven er
#gennemsnitsværdien x = 12.3.

#1 fremstil et 95% konfidensinterval for μ

alpha <- 0.05
n <- 55
barx <- 12.3
sigma <- sqrt(4)
# Da variansen er kendt, bruger vi qnorm() i stedet for qt()
z_fraktil <- qnorm(1-alpha/2)

barx-z_fraktil*sigma/sqrt(n);barx+z_fraktil*sigma/sqrt(n)

#opgave 3:
#Hvis vi fastholder n på 55 og i stedet ændrer på signifikansniveauet α (som
#for var på 5%): Hvad skal α vare for at længden af konfidensintervallet bliver
#halveret sammenlignet med intervallet, der blev fundet i Spørgsmål 1?
  
# Find den mål-fraktil som er halvdelen af 95% fraktilen
z_target <- qnorm(0.975) / 2  # Giver ca. 0.98

# Find arealet til højre for denne fraktil (som er alpha/2)
alpha_half <- 1 - pnorm(z_target)

# Find det samlede alpha
alpha_new <- 2 * alpha_half
alpha_new # Giver 0.3270862



#opgave 29
#Vi har en stikprøve bestående af 50 medarbejdere fra en stor amerikansk virksomhed.
#Den gennemsnitlige årsløn blandt de 50 medarbejdere i stikprøven er på 33313 $, og standardafvigelsen 
#i stikprøven er på 4188 (også i $). Vi kan antage, at
#årslønnen for alle medarbejdere i virksomheden er normalfordelt.

#Fremstil et 98% konfidensinterval for middelværdien af årslønnen blandt alle virksomhedens
#medarbejdere.

#jeg bruger t-fordelingen

alpha <- 0.02
n <- 50
barx <- 33313
s <- 4188
#finder t-fraktilen
t_fraktil <- qt(1-alpha/2, df=n-1)
#beregner KI
barx-t_fraktil*s/sqrt(n);barx+t_fraktil*s/sqrt(n)




#opgave 30
#1. Tegn et histogram og et qqplot over hastighederne for hver af de to stikprøver.
#Virker det rimeligt at antage, at data er (omtrentlig) normalfordelt i de to grupper?
  

HastighedAdvarsel <- read.csv("Data/HastighedAdvarsel.csv")
View(HastighedAdvarsel)
skilt <- subset(HastighedAdvarsel, Advarselsskilt =="Skilt")$Hastighed
ikke_skilt <- subset(HastighedAdvarsel, Advarselsskilt =="Ikke skilt")$Hastighed

hist(skilt, main = "Hist over hastighed med skilte", prob = T, breaks = 25)
hist(ikke_skilt, main = "Hist over hastighed uden skilte", prob = T, breaks = 25)

qqnorm(skilt, main = "QQ-plot over hastighed med skilt")
qqline(skilt)
qqnorm(ikke_skilt, main = "QQ-plot over hastighed uden skilt")
qqline(ikke_skilt)


#2
#2. Fremstil et 95% konfidensinterval for forskellen på middelværdierne af hastighederne
#for de to grupper.

#Jeg bruger denne R-formel:
#t.test(data_gruppe1, data_gruppe2, paired = FALSE, conf.level = 0.95)

t.test(skilt,ikke_skilt, paired = FALSE, conf.level=0.95)


#opgave 31
#nu er det parret data:
ReactionTime <- read.csv("Data/ReactionTime.csv")
View(ReactionTime)
ReactionTime$Forskel <- ReactionTime$Ja - ReactionTime$Nej

t.test(ReactionTime$Forskel, conf.level = 0.95)
t.test(ReactionTime$Ja,ReactionTime$Nej,paired =TRUE, conf.level = 0.95)


#opgave 32 
#1.	Udregn et estimat for variansen blandt gruppenn af observationer fra "ikke_skilt".
ikke_skilt <- subset(HastighedAdvarsel, Advarselsskilt =="Ikke skilt")$Hastighed
var(ikke_skilt)

#2 2.5 Fremstil et 95% konfidensinterval for denne varians.

n <- length(ikke_skilt)
s2 <- var(ikke_skilt)
df <- n -1

#finder chi i anden fraktilerne:
chi_lower <- qchisq(0.025,df)
chi_upper <- qchisq(0.975,df)

#beregner grænserne
KI_nedre <- (df*s2)/chi_upper
KI_øvre <- (df*s2)/chi_lower
KI <- c(KI_nedre, KI_øvre)
KI

#opgave 33 
#fremstil 95% KI for beta hvor Xi \sim Gamma(3n, 1/n) og n = 100 og X\bar = 14.3
q1 <- qgamma(0.025,shape =300, scale = 1/100)
q2 <- qgamma(0.975,shape=300,scale = 1/100)
q1;q2
X_bar <- 14.3
X_bar/q2;X_bar/q1
