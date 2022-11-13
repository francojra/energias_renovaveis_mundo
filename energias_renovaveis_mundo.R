
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
                         "United Kingdom", "United States")) %>%
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
  scale_fill_manual(values = c("#E69F00", "#56B4E9", 
                                "#009E73", "#F0E442", 
                                "#0072B2")) +
  scale_y_continuous(expand = expansion(mult = c(0,0.1))) +
  coord_flip() +
  labs(x = "Países", 
       y = "Porcentagem",
       title = "Energia primária de fontes renováveis\n entre os anos de 2000 e 2021") +
  theme_light() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"))
g3

g4 <- ggplot(energy4, aes(x = Year, y = por_energ, 
                    group = Entity, color = Entity)) +
  geom_line(size = 1.3) +
  scale_color_manual(values = c("#2E91E5", "#E15F99", 
                                "#1CA71C", "#FB0D0D", 
                                "#DA16FF")) +
  scale_x_continuous(expand = expansion(mult = c(0.01,0.01))) +
  labs(x = "Tempo (anos)", 
       y = "Porcentagem",
       title = "Energia primária de fontes renováveis\n entre os anos de 2000 e 2021",
       color = "Países") +
  theme_light() +
  theme(axis.text = element_text(color = "black"))
g4
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  




