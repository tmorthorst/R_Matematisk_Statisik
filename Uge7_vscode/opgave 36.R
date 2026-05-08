#Et amerikansk studie i staten New York har haft til form˚al at undersøge, om
#teenageres tv-sening har betydning for, om de senere udviser aggressiv adfærd.
#Til at starte med har man undersøgt 707 personers tv-sening. Af disse s˚a 88 tv
#mindre end en time om dagen, mens 619 s˚a tv mere end en time om dagen. I
#løbet af de næste 17 ˚ar fulgte man de udvalgte personer og bad dem selv og deres
#familier rapportere, om de udviste nogen form for aggressiv opførsel over for andre
#mennesker. Resultatet blev

#Aggresiv adfærd (JA) + mindre end en time tv: 5
#Aggresiv adfærd (NEJ) + mindre end en time tv: 83
#Aggresiv adfærd (JA) + mere end en time tv: 154
#Aggresiv adfærd (NEJ) + mere end en time tv: 465


#Vi vil interessere os for sandsynligheden for senere at udvise aggressiv adfærd i hver
#af de to grupper opdelt efter mængden af tv–sening.
#Fremstil et 95% konfidensinterval for forskellen på de to sandsynligheder.

alpha <- 0.05
q <- qnorm(1-alpha/2)
aggresiv_tv_mindre_JA <- 5
aggresiv_tv_mindre_NEJ <- 83
aggresiv_tv_mere_JA <- 154
aggresiv_tv_mere_NEJ <- 465
#justerede andele (Agresti-Coull)
p_tilde_mindre <- (aggresiv_tv_mere_JA + 1) / (aggresiv_tv_mere_JA + aggresiv_tv_mere_NEJ + 2)
p_tilde_mere <- (aggresiv_tv_mindre_JA + 1) / (aggresiv_tv_mindre_JA + aggresiv_tv_mindre_NEJ + 2)
#beregning af nedre og øvre grænse
L <- p_tilde_mindre - p_tilde_mere -q*sqrt(p_tilde_mindre*(1-p_tilde_mindre)/(aggresiv_tv_mere_JA + aggresiv_tv_mere_NEJ + 2) 
+ p_tilde_mere*(1-p_tilde_mere)/(aggresiv_tv_mindre_JA + aggresiv_tv_mindre_NEJ + 2))
U <- p_tilde_mindre - p_tilde_mere +q*sqrt(p_tilde_mindre*(1-p_tilde_mindre)/(aggresiv_tv_mere_JA + aggresiv_tv_mere_NEJ + 2)
 + p_tilde_mere*(1-p_tilde_mere)/(aggresiv_tv_mindre_JA + aggresiv_tv_mindre_NEJ + 2))
L; U 
