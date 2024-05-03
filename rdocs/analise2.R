#--Análise 2----

#Variação da nota de IMDB por temporada dos episódios

#tirando crossover, movie e serie da linha de temporadas

library(tidyverse)
df2 <- data.frame(banco_projetofantasma$level_0, banco_projetofantasma$season, banco_projetofantasma$imdb)
df2 <- df2%>%
  filter(banco_projetofantasma.season %in% c("1", "2", "3", "4"))
df2 <- df2%>%
  group_by(banco_projetofantasma.season)%>%
  arrange(banco_projetofantasma.season)
View(df2)

#__Gráfico Análise 2----

library(tidyverse)
ggplot(df2) +
  aes(x = banco_projetofantasma.season, y = banco_projetofantasma.imdb) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Temporadas", y = "Nota IMDB") +
  theme_estat()

ggsave("GraficoAnalise2.pdf", width = 158, height = 93, units = "mm")

#__Quadro Análise 2----

library(data.table)
QuadroAnalise2 <- setDT(banco_projetofantasma)[, as.list(c(summary(imdb),
                                                           sd = sd(imdb))), by = season]%>%
  filter(season %in% c("1", "2", "3", "4"))

