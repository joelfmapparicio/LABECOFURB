library(WDI) # CARREGAR BIBLIOTECA/PACOTE

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
