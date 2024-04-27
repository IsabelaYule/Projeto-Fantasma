#-- Análise1 ----

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

A <- tibble(
  Décadas = c('1960', '1970', '1980', '1990', '2000', '2010', '2020',
              '1960', '1970', '1980', '1990', '2000', '2010', '2020',
              '1960', '1970', '1980', '1990', '2000', '2010', '2020'),
  Quantidade = c(15, 137, 165, 10, 69, 125, 28,
                 0, 1, 3, 3, 14, 22, 3,
                 0, 3, 0, 1, 1, 3, 0),
  Formato = c('Serie', 'Serie', 'Serie', 'Serie','Serie', 'Serie', 'Serie',
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
