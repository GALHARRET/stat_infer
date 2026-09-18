##### EXERCICE 1 #####

p_A   <- 0.3   # vraie proportion
N_rep <- 5000   # nombre de répétitions
B <- 5000
alpha <- 0.05

resultats=data.frame(p_hat=rep(NA,N_rep),borne_inf=rep(NA,N_rep),borne_sup=rep(NA,N_rep))

n     <- 10    # taille d'échantillon

for(i in 1:N_rep){
    p_hat<-rbinom(1, size = n, prob = p_A) / n
    simu<-rbinom(B, size = n, prob = p_hat) / n
    ic<-quantile(simu,probs=c(alpha/2,1-alpha/2))
    resultats$p_hat[i]<-p_hat
    resultats$borne_inf[i]<-ic[[1]]
    resultats$borne_sup[i]<-ic[[2]]
}

resultats$contient <- resultats$borne_inf <= p_A & p_A <= resultats$borne_sup
taux <- mean(resultats$contient)

cat("Taux de couverture :", round(taux * 100, 1), "%\n")

n     <- 100    # taille d'échantillon

for(i in 1:N_rep){
    p_hat<-rbinom(1, size = n, prob = p_A) / n
    simu<-rbinom(B, size = n, prob = p_hat) / n
    ic<-quantile(simu,probs=c(alpha/2,1-alpha/2))
    resultats$p_hat[i]<-p_hat
    resultats$borne_inf[i]<-ic[[1]]
    resultats$borne_sup[i]<-ic[[2]]
}

resultats$contient <- resultats$borne_inf <= p_A & p_A <= resultats$borne_sup
taux <- mean(resultats$contient)

cat("Taux de couverture :", round(taux * 100, 1), "%\n")

n     <- 1000    # taille d'échantillon


for(i in 1:N_rep){
    p_hat<-rbinom(1, size = n, prob = p_A) / n
    simu<-rbinom(B, size = n, prob = p_hat) / n
    ic<-quantile(simu,probs=c(alpha/2,1-alpha/2))
    resultats$p_hat[i]<-p_hat
    resultats$borne_inf[i]<-ic[[1]]
    resultats$borne_sup[i]<-ic[[2]]
}

resultats$contient <- resultats$borne_inf <= p_A & p_A <= resultats$borne_sup
taux <- mean(resultats$contient)

cat("Taux de couverture :", round(taux * 100, 1), "%\n")


##### EXERCICE 2 #####

n      <- 50  # taille d'échantillon
B      <- 10000 # nombre de simulations

moy_est<-rep(NA,B)
# B échantillons → B proportions estimées
for(b in 1:B){
moy_est[b]<-mean(runif(n,min=200,max=250))
}

ggplot(data.frame(moy_est), aes(x = moy_est)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 50,
                 fill = "#3498db", color = "white", alpha = 0.7) +
  geom_density(color = "#e74c3c", linewidth = 1) +
  labs(
    x     = "Moyenne estimée",
    y     = "Densité",
    title = "Distribution de la moyenne empirique",
    subtitle = bquote(U(200, 250) ~ "," ~ n == .(n) ~ "," ~ B == .(B))
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )


n      <- 1000  # taille d'échantillon
B      <- 10000 # nombre de simulations

moy_est<-rep(NA,B)
# B échantillons → B proportions estimées
for(b in 1:B){
moy_est[b]<-mean(runif(n,min=200,max=250))
}

ggplot(data.frame(moy_est), aes(x = moy_est)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 50,
                 fill = "#3498db", color = "white", alpha = 0.7) +
  geom_density(color = "#e74c3c", linewidth = 1) +
  labs(
    x     = "Moyenne estimée",
    y     = "Densité",
    title = "Distribution de la moyenne empirique",
    subtitle = bquote(U(200, 250) ~ "," ~ n == .(n) ~ "," ~ B == .(B))
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )