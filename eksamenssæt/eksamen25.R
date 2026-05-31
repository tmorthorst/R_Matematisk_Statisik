#eksamen 2025
#opgave 1.1
tabel_meningsmåling <-rbind(c(257,283,314),c(247,316,292))
rownames(tabel_meningsmåling)<-c("Decemeber2024","Marts2025")
colnames(tabel_meningsmåling)<-c("Venstre for reg.","Regering","Højre for reg")
tabel_meningsmåling
test_meningsmåling_homo <-chisq.test(tabel_meningsmåling,correct = FALSE)
test_meningsmåling_homo
#opgave 1.2

samlet <- rbind(c(504,509,606))
rownames(samlet)<- "Samlet meningsmåling"
colnames(samlet)<- c("Venstre for reg.","Regering","Højre for reg")
samlet

chisq.test(samlet, p=c(0.209,0.509,0.282))

