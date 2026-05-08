#1 Udregn et estimat ˆp for andelen af huse i Salgspriser.

Salgspriser <- read.csv("Data/Salgspriser.csv")
p_hat <- mean(Salgspriser$Hustype == "Hus")
p_hat

#2 Fremstil et 95% score konfidensinterval for p.
alpha <- 0.05
n <- length(Salgspriser$Hustype)
q <- qnorm(1-alpha/2)
L <- (p_hat+q^2/(2*n)-q*sqrt(p_hat*(1-p_hat)/n+q^2/(4*n^2)))/(1+q^2/n)
U <- (p_hat+q^2/(2*n)+q*sqrt(p_hat*(1-p_hat)/n+q^2/(4*n^2)))/(1+q^2/n)
cat("Konfidensintervallet for andelen af huse i Salgspriser er: [", L, ", ", U, "]\n")

# 3. Fremstil et 95% Agresti–Coull konfindensinterval for p.
n_tilde <- n + q^2 
X <- sum(Salgspriser$Hustype == "Hus")
X_tilde <- X + q^2 /2
p_tilde <- X_tilde / n_tilde
L_AC <- p_tilde - q*sqrt(p_tilde*(1-p_tilde)/n_tilde)
U_AC <- p_tilde + q*sqrt(p_tilde*(1-p_tilde)/n_tilde)
cat("Agresti–Coull konfidensintervallet for andelen af huse i Salgspriser er: [", L_AC, ", ", U_AC, "]\n")

#4. Hvor stor skulle stikprøven mindst have været for at vi kunne være sikre p˚a,
#at Agresti–Coull intervallet havde en fejlmargen p˚a højst 2 procentpoint?

E <- 0.02
p_tilde <- 0.5
n_min <- (q^2 * p_tilde * (1 - p_tilde)) / E^2
n_min
n <- n_min - q^2 
n
