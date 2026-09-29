library(ggplot2)

# Données fictives (x = poids de naissance, y = GMQ2)
df <- data.frame(
  x = c(1.2, 2.0, 2.9, 3.6, 4.5, 5.3, 6.1, 7.0),
  y = c(2.4, 2.0, 3.9, 3.2, 4.9, 4.1, 5.9, 5.3)
)

# Ajustement par les moindres carrés
mod   <- lm(y ~ x, data = df)
beta0 <- coef(mod)[1]
beta1 <- coef(mod)[2]
df$yhat <- fitted(mod)
df$e    <- residuals(mod)

# Carrés des résidus (côté = |e_i|)
df$xmin <- df$x
df$xmax <- df$x + abs(df$e)
df$ymin <- pmin(df$y, df$yhat)
df$ymax <- pmax(df$y, df$yhat)

lab_droite <- sprintf("Droite des moindres carrés : ŷ = %.2f + %.2f x  (β0 = %.2f, β1 = %.2f)",
                      beta0, beta1, beta0, beta1)

p <- ggplot(df) +
  # carrés des résidus
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "#E8A33D", alpha = 0.25) +
  # résidus
  geom_segment(aes(x = x, xend = x, y = yhat, yend = y),
               colour = "#C0392B", linetype = "dashed", linewidth = 0.8) +
  # droite des moindres carrés
  geom_abline(aes(intercept = beta0, slope = beta1, colour = "droite"), linewidth = 1.3) +
  # observations et valeurs ajustées
  geom_point(aes(x, y, colour = "obs"), size = 3) +
  geom_point(aes(x, yhat, colour = "ajust"), shape = 21, fill = "white", size = 2.5, stroke = 1.2) +
  scale_colour_manual(
    name = NULL,
    values = c(droite = "#1F5F99", obs = "#222222", ajust = "#1F5F99"),
    breaks = c("droite", "obs", "ajust"),
    labels = c(lab_droite, "Observations (xᵢ, yᵢ)", "Valeurs ajustées (xᵢ, ŷᵢ)")
  ) +
  # annotations résidu et carré
  annotate("segment", x = 1.7, y = 4.8, xend = 2.87, yend = 3.6,
           colour = "#C0392B", arrow = arrow(length = unit(2, "mm"))) +
  annotate("text", x = 0.6, y = 4.95, hjust = 0, colour = "#C0392B", size = 4,
           label = "résidu  eᵢ = yᵢ − ŷᵢ") +
  annotate("segment", x = 5.2, y = 2.8, xend = 4.85, yend = 4.5,
           colour = "#B9770E", arrow = arrow(length = unit(2, "mm"))) +
  annotate("text", x = 4.6, y = 2.6, hjust = 0, colour = "#B9770E", size = 4,
           label = "carré du résidu  eᵢ²") +
  # encadré de définition
  annotate("label", x = 0.45, y = 7.85, hjust = 0, vjust = 1, size = 3.8,
           label.padding = unit(3, "mm"), fill = "white",
           label = "La droite des moindres carrés est celle qui rend\nla somme des carrés des résidus la plus petite possible :\n\n\n\n") +
  annotate("text", x = 0.6, y = 6.75, hjust = 0, size = 4, parse = TRUE,
           label = "min[list(beta[0], beta[1])]~~sum(e[i]^2, i, '')==sum((y[i]-(beta[0]+beta[1]*x[i]))^2, i, '')") +
  annotate("text", x = 0.6, y = 6.05, hjust = 0, size = 4, parse = TRUE,
           label = "beta[1]==frac(cov(x,y), var(x))~~~~~~beta[0]==bar(y)-beta[1]*bar(x)") +
  coord_equal(xlim = c(0.3, 8), ylim = c(0.5, 8), expand = FALSE) +
  labs(title = "Principe de la droite des moindres carrés",
       x = "x (ex. poids de naissance PN)", y = "y (ex. GMQ2)") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15),
        legend.position = c(0.99, 0.01), legend.justification = c(1, 0),
        legend.background = element_rect(colour = "grey70"))

print(p)
ggsave("droite_moindres_carres.png", p, width = 9, height = 8.5, dpi = 150)
