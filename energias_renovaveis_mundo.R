
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

# Para os países mais desenvolvidos da América Latina

energy1 <- energy %>%
  filter(Entity %in% c("Argentina", "Brazil", "Chile", 
                       "Peru", "Colombia", "Mexico")) %>%
  group_by(Entity) %>%
  summarise(media = mean(por_energ),
            sd = sd(por_energ), n = n(),
            se = sd/sqrt(n)) %>%
  view()

energy2 <- energy %>%
  filter(Entity %in% c("Argentina", "Brazil", "Chile", 
                       "Peru", "Colombia", "Mexico")) %>%
  view()

# Para os países com as maiores economias do mundo

energy3 <- energy %>%
    filter(Entity %in% c("China", "Japan", "Germany", "United Kingdom", "India", 
                       "France", "Italy", "United States", "Canada", "South Korea"),
           between(Year, 2000, 2021)) %>%
  group_by(Entity) %>%
  summarise(media = mean(por_energ),
            sd = sd(por_energ), n = n(),
            se = sd/sqrt(n)) %>%
  view()

energy4 <- energy %>%
    filter(Entity %in% c("China", "Japan", "Germany", "United Kingdom", "India", 
                       "France", "Italy", "United States", "Canada", "South Korea"),
           between(Year, 2000, 2021)) %>%
  view()

# Gráficos ---------------------------------------------------------------------------------------------------------------------------------

g1 <- ggplot(energy3, aes(x = fct_reorder(Entity, media), 
                    y = media, fill = Entity)) +
  geom_col(width = 0.9) +
  geom_errorbar(aes(ymin = media - se, ymax = media + se),
                width = 0.3, size = 0.8) +
  scale_fill_manual(values = c("#7F3C8D", "#11A579", 
                                "#3969AC", "#F2B701", 
                                "#E73F74", "#80BA5A", 
                                "#E68310", "#008695", 
                                "#CF1C90", "#A5AA99")) +
  scale_y_continuous(expand = expansion(mult = c(0,0.1))) +
  coord_flip() +
  labs(x = "Países", 
       y = "Porcentagem",
       title = "Energia primária de fontes renováveis\n entre os anos de 2000 e 2021") +
  theme_light() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"))
g1

g2 <- ggplot(energy4, aes(x = Year, y = por_energ, 
                    group = Entity, color = Entity)) +
  geom_line(size = 1.8) +
  scale_color_manual(values = c("#7F3C8D", "#11A579", 
                                "#3969AC", "#F2B701", 
                                "#E73F74", "#80BA5A", 
                                "#E68310", "#008695", 
                                "#CF1C90", "#A5AA99")) +
  scale_x_continuous(expand = expansion(mult = c(0,0))) +
  labs(x = "Tempo (anos)", 
       y = "Porcentagem",
       title = "Energia primária de fontes renováveis\n entre os anos de 2000 e 2021",
       color = "Países") +
  theme_light() +
  theme(axis.text = element_text(color = "black"))
g2

g3 <- ggplot(energy3, aes(x = fct_reorder(Entity, media), 
                    y = media, fill = Entity)) +
  geom_col(width = 0.9) +
  geom_errorbar(aes(ymin = media - se, ymax = media + se),
                width = 0.3, size = 0.8) +
  scale_fill_manual(values = c("#7F3C8D", "#11A579", 
                                "#3969AC", "#F2B701", 
                                "#E73F74", "#80BA5A", 
                                "#E68310", "#008695", 
                                "#CF1C90", "#A5AA99")) +
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
  geom_line(size = 1.8) +
  scale_color_manual(values = c("#7F3C8D", "#11A579", 
                                "#3969AC", "#F2B701", 
                                "#E73F74", "#80BA5A", 
                                "#E68310", "#008695", 
                                "#CF1C90", "#A5AA99")) +
  scale_x_continuous(expand = expansion(mult = c(0,0))) +
  labs(x = "Tempo (anos)", 
       y = "Porcentagem",
       title = "Energia primária de fontes renováveis\n entre os anos de 2000 e 2021",
       color = "Países") +
  theme_light() +
  theme(axis.text = element_text(color = "black"))
g4





