
# Energias renováveis ----------------------------------------------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ---------------------------------------------------------------------------------------------------------
# Data: 11/11/22 ---------------------------------------------------------------------------------------------------------------------------
# Referência: https://ourworldindata.org/renewable-energy ----------------------------------------------------------------------------------

# Carregar pacotes -------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(cols4all)
library(hrbrthemes)
library(gridExtra)

# Carregar dados ---------------------------------------------------------------------------------------------------------------------------

energy <- read.csv("renewable-share-energy.csv")
view(energy)
names(energy)
energy_fonte <- read.csv("modern-renewable-prod.csv")
view(energy_fonte)
names(energy_fonte)

# Manipular dados --------------------------------------------------------------------------------------------------------------------------

energy <- energy %>%
  select(-Code) %>%
  rename(por_energ = Renewables....equivalent.primary.energy.) %>%
  view()

# Para os países com as maiores economias do mundo

energy3 <- energy %>%
    filter(Entity %in% c("China", "Japan", "Germany", 
                         "United Kingdom", "United States"),
           Year %in% c("2018", "2019", "2020", "2021")) %>%
  group_by(Entity) %>%
  summarise(media = mean(por_energ),
            sd = sd(por_energ), n = n(),
            se = sd/sqrt(n)) %>%
  view()

energy4 <- energy %>%
    filter(Entity %in% c("China", "Japan", "Germany", 
                         "United Kingdom", "United States")) %>%
  view()

# Gráficos ---------------------------------------------------------------------------------------------------------------------------------

c4a_gui()

g3 <- ggplot(energy3, aes(x = fct_reorder(Entity, media), 
                    y = media, fill = Entity)) +
  geom_col(width = 0.9) +
  geom_errorbar(aes(ymin = media - se, ymax = media + se),
                width = 0.3, size = 0.8) +
  scale_fill_manual(values = c("#1B9E77", "#D95F02", 
                                "#7570B3", "#E7298A", 
                                "#66A61E")) +
  scale_y_continuous(expand = expansion(mult = c(0,0.1))) +
  scale_x_discrete(labels = c("Estados Unidos", "Japão", 
                              "China", "Reino Unido", "Alemanha")) +
  labs(x = "Países", 
       y = "Porcentagem",
       title = "Energia de fontes renováveis\n entre os anos de 2018 e 2021") +
  theme_light() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"))
g3

g4 <- ggplot(energy4, aes(x = Year, y = por_energ, 
                    group = Entity, color = Entity)) +
  geom_line(size = 1.3) +
  scale_color_manual(values = c("#1B9E77", "#D95F02", 
                                 "#7570B3", "#E7298A", 
                                 "#66A61E"),
                     labels = c("China", "Alemanha", "Japão",
                                "Reino Unido", "Estados Unidos")) +
  labs(x = "Tempo (anos)", 
       y = "Porcentagem",
       title = "Energia de fontes renováveis entre os anos de 1965 e 2021",
       color = "Países") +
  theme_light() +
  theme(axis.text = element_text(color = "black"))
g4





