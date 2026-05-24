#uge 12 
#opgave 65
tabel_HS <- rbind(c(1440,500),c(46,281))

rownames(tabel_HS) <- c("Alkohol_ja","Alkohol_nej")
colnames(tabel_HS) <- c("Cigaret_ja", "Cigaret_nej")
tabel_HS

#1 udregn de forventede værdier under nulhypotesen:
# H0: Cigaret og alkoholforbrug er uafhængige
#Ha: Cigaret og alkoholforbrug er afhængige
test_tabel_HS <- chisq.test(tabel_HS)
test_tabel_HS$expected

#2. Udregn Pearson’s teststørrelse og find en p-værdi for testet ved at approksimere
#med χ2-fordelingen.

test_tabel_HS
