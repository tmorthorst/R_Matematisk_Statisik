#opgave 25
library(resampledata3)
#
#2. Benyt formlerne for momentestimatorerne udledt i spørgsmål 1 til at estimere
#α og β på baggrund af datasættet.¨

View(Service)
X_bar <- mean(Service$Times)
varians <- var(Service$Times)
X_bar
varians
#vi har alpha = (X_bar^2)/(varians)
#vi har beta = varians/X

alpha <- X_bar^2/varians
beta <- varians/X
alpha
beta
# Tegn et histogram over datasættets observationer, og indtegn tætheden for den
#fundne gammafordeling

hist(Service$Times,breaks=20,prob = T, main = "Histogram med Gamma tæthed", xlab ="ventetid")

curve(dgamma(x,shape = alpha, scale = beta), add = TRUE, col ="red")

      