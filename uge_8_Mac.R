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

HastighedAdvarsel <- read.csv("Datamac/HastighedAdvarsel.csv")
View(head(HastighedAdvarsel))

med_skilt <- HastighedAdvarsel$Hastighed[HastighedAdvarsel$Advarselsskilt == "Skilt"]
uden_skilt <- HastighedAdvarsel$Hastighed[HastighedAdvarsel$Advarselsskilt == "Ikke skilt"]

t.test(med_skilt, uden_skilt)
