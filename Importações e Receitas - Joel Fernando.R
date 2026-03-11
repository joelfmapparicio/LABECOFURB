#install.packages("ipeadatar")
#install.packages("tidyverse")
#install.packages("devtools")
#install_github("gomesleduardo/ipeadatar")
library(devtools)
library(ipeadatar)
library(tidyverse)
options(scipen = 999)


impotação2024 <- 
  ipeadata("IMPORTACAO") %>% 
  filter(uname == "States" & date == "2024-01-01")

receitacorrente2024 <- 
  ipeadata("RECORRE") %>% 
  filter(uname == "States" & date == "2024-01-01")

imp <- select(impotação2024, imp = value, tcode)
rec <- select(receitacorrente2024, rec = value, tcode)

# JUNTANDO AMBAS AS VARIÁVEIS POR ESTADO (tcode)
# DEIXANDO A BASE DE DADOS SOMENTE COM AS VARIÁVEIS

bd <- merge(imp, rec, by = "tcode") %>% 
  select(-tcode)

# ESTIMANDO O MODELO ECONOMÉTRICO

mod <- lm(imp ~ rec, bd)

# VENDO OS RESULTADOS DO MODELO ESTIMADO

summary(mod)


