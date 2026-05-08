#Uge 8
### Opgave 44

#a Vi fandt et konfidensinterval for mu1 - mu2 på (-2,61; -2,07) i opg 30
# H0: mu1 = mu2 mod HA: mu1 != mu2
# Vil hypotesen blive forkastet p˚a et 5% signifikansniveau?
# Ja, da 0 ikke ligger i konfidensintervallet, kan vi forkaste nulhypotesen om at mu1 = mu2
#b
#lav et t test for test af nulhypotesen H0: mu1 = mu2 mod HA: mu1 != mu2
# Hvor mu1 betegner middelværdien for gruppen uden advarselsskilte
# og mu2 betegner middelværdien for gruppen med advarselsskilte. Benyt et 5% signifikansniveau.

HastighedAdvarsel <- read.csv("Data/HastighedAdvarsel.csv")
View(head(HastighedAdvarsel))

med_skilt <- HastighedAdvarsel$Hastighed[HastighedAdvarsel$Advarselsskilt == "Skilt"]
uden_skilt <- HastighedAdvarsel$Hastighed[HastighedAdvarsel$Advarselsskilt == "Ikke skilt"]

t.test(med_skilt, uden_skilt)



#opgave 45
#I en amerikansk virksomhed er 24 mænd og 26 kvinder blevet spurgt om deres løn.
#Kvinderne tjente i gennemsnit 31290 dollar med sd = 3670
#Mændenes gennemsnitlige løn var 35504 dollar med sd =3618
#Det kan antages at lønnen er normalfordelt i begge grupper.

#lav et t test på 5 % signifikansniveau for at teste om der er en forskel i løn mellem mænd og kvinder i virksomheden

# Jeg opstiller nulhypotesen: H0: X_bar = Y_bar mod HA: X_bar != Y_bar
# hvor X_bar er gennemsnitslønnen for kvinder og Y_bar er gennemsnitslønnen for mænd

# Jeg kan bruge t.test funktionen i R til at udføre t-testen
X_bar <- 31290
Y_bar <- 35504
sd_X <- 3670
sd_Y <- 3618
n_X <- 26
n_Y <- 24

#jeg beregner teststørrelsen: T
T <- (X_bar - Y_bar) / sqrt((sd_X^2 / n_X) + (sd_Y^2 / n_Y))
T
# Jeg beregner frihedsgraderne: df
df <- ((X_bar^2 / n_X + Y_bar^2 / n_Y)^2) / (((X_bar^2 / n_X)^2 / (n_X - 1)) + ((Y_bar^2 / n_Y)^2 / (n_Y - 1)))
df
# Jeg beregner p-værdien
p_value <- 2 * (1 - pt(abs(T), df))
p_value


#opgave 46
#Vi kigger på sættet ReactionTime, hvor 32 studerende har fået målt reaktionstid, mens de kører bil i to forskellige situationer
# situation 1: uden tlf (anført i søjlen: Nej) og situation 2: med tlf (anført i søjlen: Ja)
ReactionTime <- read.csv("Data/ReactionTime.csv")
View(head(ReactionTime))

#benyt et relevant t test til at teste om der er en forskel i middelværdien for reaktionstid.
#benyt et signifikansniveau på 5%

# Jeg opstiller nulhypotesen: H0: X_bar = Y_bar mod HA: X_bar != Y_bar
# hvor X_bar er gennemsnitsreaktionstiden uden tlf og Y_bar er gennemsnitsreaktionstiden med tlf.

#jeg laver t test for parret data, da det er de samme studerende der har fået målt reaktionstid i begge situationer

t.test(ReactionTime$Nej, ReactionTime$Ja, paired = TRUE)


#Opgave 47
#I datasættet CompulsiveBuyers er 800 mænd og 1501 kvinder blevet interviewet og vurderet om de har sygelig købstrang.
#Lav et relevant test for at undersøge (på et 5% signifikansniveau), om der er forskel på andelen af mand og 
#andelen af kvinder, der har en sygelig købetrang. Sørg for at fortolke resultatet.

CompulsiveBuyers <- read.csv("Data/CompulsiveBuyers.csv")
View(head(CompulsiveBuyers))

# Jeg opstiller nulhypotesen: H0: p_mænd = p_kvinder mod HA: p_mænd != p_kvinder
# hvor p_mænd er andelen af mænd med sygelig købstrang og p_kvinder er andelen af kvinder med sygelig købstrang.

# jeg benytter mig af et z-test for at teste forskellen i andele mellem mænd og kvinder

#1. indstaster data
nM <- 800
nK <- 1501
xM <- sum(CompulsiveBuyers$Gender == "Male" & CompulsiveBuyers$Compulsive == "yes")
xK <- sum(CompulsiveBuyers$Gender == "Female" & CompulsiveBuyers$Compulsive == "yes")
xK
#2. udregner estimater og den poolede andel
phatK <-xK / nK
phatM <- xM / nM
phatP <- (xK+xM)/(nK+nM)

#3. udregner teststørrelsen z_obs
z_obs <- (phatK - phatM) /sqrt(phatP *(1-phatP)*(1/nK+1/nM))

#4. udregner p-værdien (to-sidet test)
2*(1-pnorm(abs(z_obs)))

z_obs


#opgave 48
#opgave 1,2 og 3 er besvaret i word
#opgave 4: 4.	Udregn den faktiske værdi af −2 logQ(x) og brug denne til at finde p-værdien
#for likelihood ratio testet. Hvad kan man, på et 5% signifikansniveau, konkludere
#om sandsynligheden p for at stemme Biden?

n <- 1646
x <- 930
p0 <- 0.5
p_hat <- x/n
test_stat <- 2*(x*log(p_hat/p0)+(n-x)*log((1-p_hat)/(1-p0)))
test_stat
p_value <- 1- pchisq(test_stat, df =1)
p_value




#opgave 49
#opg 1 og 2 lavet i word
#opg 3 vi har information om 2167 forsikringsudbetalinger. Sådan data kan modeleres med paretofordeling.  
#Vi vil undersøge om den forventede udbetaling er 4 mio. kr. hvilket svarer til H0: theta = 2 mod Ha theta != 2
# Udregn først MLE og dernæst den faktiske værdi af -2 logQ(x) som er fundet i forrige opgaver.

forsikringsudbetalinger <- read.csv("Data/Forsikring.csv")
View(head(forsikringsudbetalinger))
n <- length(forsikringsudbetalinger$Forsikringssum)
x_i <- forsikringsudbetalinger$Forsikringssum
theta_hat <- n / (sum(log(x_i))-n*log(2))
theta_hat

test_stat <- 2*(n*log(theta_hat/2)+n*log(2)*(theta_hat-2)-(theta_hat-2)*sum(log(x_i)))
test_stat
# Brug sidstnævnte til at finde p-værdien for likelihood ratio testet

p_value <- 1- pchisq(test_stat, df = 1)
p_value
