
# Gestion de l'environnement ----------------------------------------------
library(tidyverse)
library(forcats)
library(MASS)
library(roxygen2)

# nettoyage ---------------------------------------------------------------
lintr::use_lintr(type = "tidyverse")
lintr::lint("script.R")
styler::style_file("script.R")
lintr::lint("script.R")


# appel des fonctions -----------------------------------------------------

source("functions.R", encoding = "UTF-8")

# Import de données -------------------------------------------------------
df <- arrow::read_parquet(
  "individu_reg.parquet",
  col_select = c("region", "aemm", "aged", "anai", "catl", "cs1", "cs2", "cs3",
                 "couple", "na38", "naf08", "pnai12", "sexe", "surf", "tp",
                 "trans", "ur")
)

# retraitement des données ------------------------------------------------

df <- df %>%
  mutate(aged = as.numeric(aged))

df$sexe <- df$sexe %>%
  as.character() %>%
  fct_recode(Homme = "1", Femme = "2")

df3 <- df %>%
  dplyr::select(surf, cs1, ur, couple, aged) %>%
  filter(surf != "Z")

df3[, 1] <- factor(df3$surf, ordered = TRUE)

df3[, "cs1"] <- factor(df3$cs1)

df3 %>%
  filter(couple == "2" & aged > 40 & aged < 60)

# stats desc --------------------------------------------------------------

summarise(group_by(df, aged), n())

df3 <- df %>%
  group_by(couple, trans) %>%
  summarise(x = n()) %>%
  group_by(couple) %>%
  mutate(y = 100 * x / sum(x))

calculer_stats(rnorm(10))
calculer_stats(rnorm(10), "ecart-type")
calculer_stats(rnorm(10), "variance")

calculer_stats(df %>% 
                           filter(sexe == "Homme") %>% 
                           pull(aged))

calculer_stats(df %>% 
                           filter(sexe == "Femme") %>% 
                           pull(aged))

# graphiques --------------------------------------------------------------
ggplot(df) +
  geom_histogram(aes(x = 5 * floor(as.numeric(aged) / 5)), stat = "count")

# part d'homme dans chaque cohort
p <- df %>%
  group_by(aged, sexe) %>%
  summarise(SH_sexe = n()) %>%
  group_by(aged) %>%
  mutate(SH_sexe = SH_sexe / sum(SH_sexe)) %>%
  filter(sexe == 1) %>%
  ggplot() +
  geom_bar(aes(x = aged, y = SH_sexe), stat = "identity") +
  geom_point(aes(x = aged, y = SH_sexe), stat = "identity", color = "red") +
  coord_cartesian(c(0, 100))


ggsave("p.png", p)

# modélisation ------------------------------------------------------------

polr(surf ~ cs1 + factor(ur), df3)
