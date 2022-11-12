
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
  filter(Entity %in% c("Argentina", "Bolivia", "Brazil", "Chile",
                       "Colombia", "Costa Rica", "Cuba", 
                       "Dominican Republic", "Ecuador", "El Salvador",
                       "Guatemala", "Haiti", "Honduras", "Mexico",
                       "Nicaragua", "Panama", "Paraguay", "Peru",
                       "Uruguay", "Venezuela")) %>%
  group_by(Entity) %>%
  summarise(media = mean(por_energ),
            sd = sd(por_energ), n = n(),
            se = sd/sqrt(n)) %>%
  view()

energy2 <- energy %>%
  filter(Entity %in% c("Argentina", "Bolivia", "Brazil", "Chile",
                       "Colombia", "Costa Rica", "Cuba", 
                       "Dominican Republic", "Ecuador", "El Salvador",
                       "Guatemala", "Haiti", "Honduras", "Mexico",
                       "Nicaragua", "Panama", "Paraguay", "Peru",
                       "Uruguay", "Venezuela")) %>%
  view()

# Gráficos ---------------------------------------------------------------------------------------------------------------------------------

c4a_gui()
c4a("dark24", 20)

ggplot(energy1, aes(x = fct_reorder(Entity, media), 
                    y = media, fill = Entity)) +
  geom_col(width = 0.9) +
  geom_errorbar(aes(ymin = media - se, ymax = media + se),
                width = 0.3, size = 0.8) +
  scale_fill_manual(values = c("#2E91E5", "#E15F99", 
                                "#1CA71C", "#FB0D0D", 
                                "#DA16FF", "#222A2A", 
                                "#B68100", "#750D86", 
                                "#EB663B", "#511CFB", 
                                "#00A08B", "#FB00D1", 
                                "#FC0080", "#B2828D", 
                                "#6C7C32", "#778AAE", 
                                "#862A16", "#A777F1", 
                                "#620042", "#1616A7")) +
  scale_y_continuous(expand = expansion(mult = c(0,0.1))) +
  coord_flip() +
  labs(x = "Países", 
       y = "Porcentagem",
       title = "Energia primária de fontes renováveis\n entre os anos de 1965 e 2021") +
  theme_light() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"))

ggplot(energy2, aes(x = Year, y = por_energ, 
                    group = Entity, color = Entity)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = c("#2E91E5", "#E15F99", 
                                "#1CA71C", "#FB0D0D", 
                                "#DA16FF", "#222A2A", 
                                "#B68100", "#750D86", 
                                "#EB663B", "#511CFB", 
                                "#00A08B", "#FB00D1", 
                                "#FC0080", "#B2828D", 
                                "#6C7C32", "#778AAE", 
                                "#862A16", "#A777F1", 
                                "#620042", "#1616A7")) +
  labs(x = "Tempo (anos)", 
       y = "Porcentagem",
       title = "Energia primária de fontes renováveis\n entre os anos de 1965 e 2021") +
  theme_light() +
  theme(axis.text = element_text(color = "black"))








