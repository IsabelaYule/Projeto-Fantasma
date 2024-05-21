#-- Análise 5 ----
#Variação da nota de engajamento pelo personagem que conseguiu capturar o monstro

library(tidyverse)
library(data.table)
capturam <- tibble(banco_projetofantasma$caught_fred, 
                       banco_projetofantasma$caught_daphnie,
                       banco_projetofantasma$caught_velma,
                       banco_projetofantasma$caught_shaggy,
                       banco_projetofantasma$caught_scooby,
                       banco_projetofantasma$caught_other,
                       banco_projetofantasma$engagement)
colnames(capturam) <- c("Fred", "Daphnie", "Velma", "Salsicha",
                        "Scooby", "Outro", "Engajamento")
capturam[capturam==FALSE] <- NA

capturam$Fred <- capturam$Fred %>% str_replace_all('TRUE', "Fred")
capturam$Daphnie <- capturam$Daphnie %>% str_replace_all('TRUE', "Daphnie")
capturam$Velma <- capturam$Velma %>% str_replace_all('TRUE', "Velma")
capturam$Salsicha <- capturam$Salsicha %>% str_replace_all('TRUE', "Salsicha")
capturam$Scooby <- capturam$Scooby %>% str_replace_all('TRUE', "Scooby")
capturam$Outro <- capturam$Outro %>% str_replace_all('TRUE', "Outro")

df <- unite(capturam, "Personagens", c("Fred", "Daphnie", "Velma", "Salsicha", "Scooby", "Outro"), sep = "//", na.rm = T)
df <- separate_rows(df, "Personagens")%>%
  filter(Personagens %in% c("Fred", "Daphnie", "Velma", "Salsicha", "Scooby", "Outro"))%>%
  group_by(Personagens)%>%
  arrange(Personagens)

#__Gráfico Análise 5----

ggplot(df) +
  aes(x = Personagens , y = Engajamento) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Personagens", y = "Engajamento") +
  theme_estat()

#__Dados para um quadro resumo----

QuadroAnalise5 <- setDT(df)[, as.list(c(summary(Engajamento),
                                        sd=sd(Engajamento))), by=Personagens]
View(QuadroAnalise5)
