
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
energy_fonte <- read.csv("modern-renewable-prod.csv")
view(energy_fonte)