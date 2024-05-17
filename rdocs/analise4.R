#--Análise 4----

#Relação das notas IMDB e engajamento

library(tidyverse)
nota_eng <- data.frame(banco_projetofantasma$imdb,
                       banco_projetofantasma$engagement)
colnames(nota_eng) <- c('Notas IMDB', 'Engajamento')
nota_eng <- nota_eng[order(nota_eng$`Notas IMDB`,
                           nota_eng$Engajamento),]

#__Gráfico Análise 4----

ggplot(nota_eng) +
  aes(x = `Notas IMDB`, y = Engajamento) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Notas IMDB",
    y = "Engajamento"
  ) +
  theme_estat()

ggsave("GraficoAnalise4.pdf", width = 158, height = 93, units = "mm")

#__Dados para fazer uma tabela----

nota_eng2 <- data.frame(banco_projetofantasma$imdb,
                       banco_projetofantasma$engagement)
colnames(nota_eng2) <- c('Notas IMDB', 'Engajamento')
nota_eng2 <- nota_eng2[order(nota_eng2$`Notas IMDB`,
                           nota_eng2$Engajamento),]

nota_eng2$`Notas IMDB` <- as.integer(nota_eng2$`Notas IMDB`)
nota_eng2 <- group_by(nota_eng2, `Notas IMDB`)
View(nota_eng2)

nota_eng2 <- na.omit(nota_eng2)

n_e2 <- nota_eng2%>%
  filter(`Notas IMDB` %in% 2)
mean(n_e2$Engajamento)

n_e3 <- nota_eng2%>%
  filter(`Notas IMDB`%in% 3)
mean(n_e3$Engajamento)

n_e4 <- nota_eng2%>%
  filter(`Notas IMDB`%in% 4)
mean(n_e4$Engajamento)

n_e5 <- nota_eng2%>%
  filter(`Notas IMDB`%in% 5)
mean(n_e5$Engajamento)

n_e6 <- nota_eng2%>%
  filter(`Notas IMDB`%in% 6)
mean(n_e6$Engajamento)

n_e7 <- nota_eng2%>%
  filter(`Notas IMDB`%in% 7)
mean(n_e7$Engajamento)

n_e8<- nota_eng2%>%
  filter(`Notas IMDB`%in%8)
mean(n_e8$Engajamento)


