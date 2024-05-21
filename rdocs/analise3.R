<<<<<<< HEAD
#--Análise 3----

#Top 3 terrenos mais frequentes pela ativação da armadilha

#Entender a frequência dos terrenos(fonte da tabela de terrenos mais frequentes)

library(tidyverse)
tab_terrenos <- table(banco_projetofantasma$setting_terrain)
tab_terrenos <- sort(tab_terrenos, decreasing = TRUE)
sum(tab_terrenos)

tab_terrenos <- as.data.frame(tab_terrenos)
as.numeric(tab_terrenos$Freq)
mutate(tab_terrenos$Freqrel <- c(round((tab_terrenos$Freq/603),4)))

View(tab_terrenos)

#Top 3 terrenos e a ativação da armadilha de primeira

terrenos2 <- table(banco_projetofantasma$setting_terrain,
                   banco_projetofantasma$trap_work_first)
terrenos2 <- as.data.frame(terrenos2)%>%
  filter(Var1 %in% c("Urban", "Rural", "Forest"))%>%
  group_by(Var1)%>%
  arrange(Var1)
terrenos2$Var1 <- recode(terrenos2$Var1, Forest='Floresta',
                         Urban='Urbano')
terrenos2$Var2 <- recode(terrenos2$Var2, 'TRUE'='Sim',
                         'FALSE'='Não')
colnames(terrenos2) <- c('Terrenos','Armadilha funcionou de primeira', 'Freq')
terrenos2 <- terrenos2[order(terrenos2$Terrenos, decreasing = T),]


terrenos2 <- terrenos2 %>%
  mutate(Terrenos = case_when(
    Terrenos %>% str_detect("Urbano") ~ "Urbano",
    Terrenos %>% str_detect("Rural") ~ "Rural",
    Terrenos %>% str_detect("Floresta") ~ "Floresta"
  )) %>%
  group_by(Terrenos, `Armadilha funcionou de primeira`) %>%
  summarise(freq = Freq ) %>%
  mutate(
    freq_relativa = round(freq/sum(freq)*100,1)
  ) 
terrenos2 <- terrenos2%>%
  mutate(
    freq_relativa = gsub("\\.", ",", freq_relativa) %>% paste("%", sep = ""),
    label = str_c(freq, "(", freq_relativa, ")") %>% str_squish()
  )

View(terrenos2)

#__Gráfico Análise 3----

ggplot(terrenos2) +
  aes(
    x = fct_reorder(Terrenos, freq, .desc = T), y = freq,
    fill = `Armadilha funcionou de primeira`, label = label
  ) +
  ylim(c(0, 75))+
  geom_col(position = position_dodge2(preserve = "single", padding =
                                        0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  labs(x = "Terrenos", y = "Frequência") +
  theme_estat()

ggsave("GraficoAnalise3.pdf", width = 158, height = 93, units = "mm")




=======
#--Análise 3----

#Top 3 terrenos mais frequentes pela ativação da armadilha

#Entender a frequência dos terrenos(fonte da tabela de terrenos mais frequentes)

library(tidyverse)
tab_terrenos <- table(banco_projetofantasma$setting_terrain)
tab_terrenos <- sort(tab_terrenos, decreasing = TRUE)
sum(tab_terrenos)

tab_terrenos <- as.data.frame(tab_terrenos)
as.numeric(tab_terrenos$Freq)
mutate(tab_terrenos$Freqrel <- c(round((tab_terrenos$Freq/603),4)))

View(tab_terrenos)

#Top 3 terrenos e a ativação da armadilha de primeira

terrenos2 <- table(banco_projetofantasma$setting_terrain,
                   banco_projetofantasma$trap_work_first)
terrenos2 <- as.data.frame(terrenos2)%>%
  filter(Var1 %in% c("Urban", "Rural", "Forest"))%>%
  group_by(Var1)%>%
  arrange(Var1)
terrenos2$Var1 <- recode(terrenos2$Var1, Forest='Floresta',
                         Urban='Urbano')
terrenos2$Var2 <- recode(terrenos2$Var2, 'TRUE'='Sim',
                         'FALSE'='Não')
colnames(terrenos2) <- c('Terrenos','Armadilha funcionou de primeira', 'Freq')
terrenos2 <- terrenos2[order(terrenos2$Terrenos, decreasing = T),]


terrenos2 <- terrenos2 %>%
  mutate(Terrenos = case_when(
    Terrenos %>% str_detect("Urbano") ~ "Urbano",
    Terrenos %>% str_detect("Rural") ~ "Rural",
    Terrenos %>% str_detect("Floresta") ~ "Floresta"
  )) %>%
  group_by(Terrenos, `Armadilha funcionou de primeira`) %>%
  summarise(freq = Freq ) %>%
  mutate(
    freq_relativa = round(freq/sum(freq)*100,1)
  ) 
terrenos2 <- terrenos2%>%
  mutate(
    freq_relativa = gsub("\\.", ",", freq_relativa) %>% paste("%", sep = ""),
    label = str_c(freq, "(", freq_relativa, ")") %>% str_squish()
  )

View(terrenos2)

#__Gráfico Análise 3----

ggplot(terrenos2) +
  aes(
    x = fct_reorder(Terrenos, freq, .desc = T), y = freq,
    fill = `Armadilha funcionou de primeira`, label = label
  ) +
  ylim(c(0, 75))+
  geom_col(position = position_dodge2(preserve = "single", padding =
                                        0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  labs(x = "Terrenos", y = "Frequência") +
  theme_estat()

ggsave("GraficoAnalise3.pdf", width = 158, height = 93, units = "mm")




>>>>>>> 1dec98d (.)
