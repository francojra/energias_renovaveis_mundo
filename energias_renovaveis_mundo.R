
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

# Para a América Latina

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



