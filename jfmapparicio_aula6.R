library(WDI) # CARREGAR BIBLIOTECA/PACOTE
#install.packages("WDI")

options(scipen = 999) # REMOVER A NOT. CIENT.
# DADOS EM PAINEL
dadospib <- WDI(country = 'all',
                indicator = 'EG.USE.ELEC.KH.PC')

paises <- c('BR', 'US')

dadoselectricbrus <- WDI(country = paises,
                         indicator = 'EG.USE.ELEC.KH.PC')

# CORTE TRANSVERSAL
dadosElectric2023 <- WDI(country = 'all',
                         indicator = 'EG.USE.ELEC.KH.PC',
                         start = 2023, end = 2023)

# SÉRIE TEMPORAL
dadosElectricBR <- WDI(country = 'BR',
                       indicator = 'EG.USE.ELEC.KH.PC') 

#GRÁFICOS 
#BIBLIOTECA ggplot2 (tidyverse)
#install.packages ("tidyverse")
library(tidyverse)


#DADOS EM PAINÉIS

grafpainel <- ggplot(dadosElectricBR,
                     mapping = aes(y = EG.USE.ELEC.KH.PC,
                                   x = year)) + 

  geom_point() 

print(grafpainel)

#CORTE TRANSVERSAL

grafcorte <-  ggplot(dadosElectric2023,
                     mapping = aes(y = EG.USE.ELEC.KH.PC,
                                   x = year)) + 
  
  geom_point()

print(grafcorte)

# SÉRIE TEMPORAL 

grafserie <-  ggplot(dadosElectricBR,
                     mapping = aes(y = EG.USE.ELEC.KH.PC,
                                   x = year)) + 
  geom_line()

print(grafserie)

