#-- Análise 1 ----

#criar nova coluna que mostre apenas os anos dos lançamentos

library(lubridate)
banco_projetofantasma$anos <- year(banco_projetofantasma$date_aired)

#separar as décadas


banco_projetofantasma$décadas <- cut(banco_projetofantasma$anos,
                                     breaks =  seq(1959, 2029, 10),
                                     labels = c('1960',
                                                '1970',
                                                '1980',
                                                '1990',
                                                '2000',
                                                '2010',
                                                '2020'))

#entender quantidade de lançamentos em cada década

tabela_1 <- table(banco_projetofantasma$décadas, banco_projetofantasma$format)
tabela_1 <- tabela_1[,c("Serie", "Movie", "CrossOver")]

as.data.frame(tabela_1)
dftabela1<- data.frame(rbind(tabela_1))
 

#__Gráfico Análise 1----

#construindo uma tabela com as quantidades de cada formato como uma nova coluna para construir um gráfico

library(tidyverse)
A <- tibble(
  Décadas = c('1960´s', '1970´s', '1980´s', '1990´s', '2000´s', '2010´s', '2020´s',
              '1960´s', '1970´s', '1980´s', '1990´s', '2000´s', '2010´s', '2020´s',
              '1960´s', '1970´s', '1980´s', '1990´s', '2000´s', '2010´s', '2020´s'),
  Quantidade = c(15, 137, 165, 10, 69, 125, 28,
                 0, 1, 3, 3, 14, 22, 3,
                 0, 3, 0, 1, 1, 3, 0),
  Formato = c('Série', 'Série', 'Série', 'Série','Série', 'Série', 'Série',
              'Filme', 'Filme', 'Filme', 'Filme', 'Filme', 'Filme', 'Filme',
              'CrossOver', 'CrossOver', 'CrossOver', 'CrossOver', 'CrossOver', 'CrossOver', 'CrossOver')
)

#contrução do gráfico

ggplot(A) +
  aes(x = Décadas, y = Quantidade, group = Formato, colour = Formato) +
  scale_fill_manual('Formato', values = c("#006606","#003366","#A11D21") )+
  geom_line(size = 1)+
  geom_point(size = 2) +
  labs(x = "Décadas", y = "Quantidade") +
  theme_estat()
ggsave("GraficoAnalise1.pdf", width = 158, height = 93, units = "mm")

#__Tabela Análise 1----

# adicionar o total de cada formato de lançamento 
# e adicionar o total de lançamentos por década


#cálculo do total de formatos de lançamento
"Total" <- list(sum(dftabela1$Serie),
                sum(dftabela1$Movie),
                sum(dftabela1$CrossOver))

#construção da tabela
T1 <- tibble(
  Décadas = c('1960', '1970', '1980', '1990', '2000', '2010', '2020', 'Total'),
  Serie = c(15, 137, 165, 10, 69, 125, 28, 549),
  Filme = c(0, 1, 3, 3, 14, 22, 3, 46),
  CrossOver = c(0, 3, 0, 1, 1, 3, 0, 8),
  Total = c(15, 141, 168, 14, 84, 150, 31, 603)
)


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

