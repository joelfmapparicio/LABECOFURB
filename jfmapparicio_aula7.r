library(WDI)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

#install.packages("ggthemes")
#install.packages("rnaturalearthdata")
#install.packages("rnaturalearth")

# 1. Baixar os dados de exportação (% do PIB)
exportdata <- WDI(
  country = "all", 
  indicator = "NE.EXP.GNFS.ZS",
  start = 1990, end = 2022,
  extra = TRUE
) %>% filter(region != "Aggregates")

# ---------- GRÁFICO 1: Linha de países selecionados ----------
paises <- c("Brazil", "Germany", "China", "United States")

graf_linhas <- ggplot(filter(exportdata, country %in% paises), 
                      aes(x = year, y = NE.EXP.GNFS.ZS, color = country)) +
  geom_line(size = 1.2) +
  labs(
    title = "Exportações (% do PIB) - Comparativo entre Países",
    x = "Ano",
    y = "% do PIB",
    color = "País"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(graf_linhas)

# ---------- GRÁFICO 2: Boxplot por década ----------
exportdata <- exportdata %>%
  mutate(decada = paste0(floor(year / 10) * 10, "s"))

graf_boxplot <- ggplot(exportdata, aes(x = decada, y = NE.EXP.GNFS.ZS)) +
  geom_boxplot(fill = "#69b3a2", color = "#1f3c88") +
  labs(
    title = "Distribuição da Taxa de Exportações por Década",
    x = "Década",
    y = "% do PIB"
  ) +
  theme_minimal()

print(graf_boxplot)

# ---------- GRÁFICO 3: Mapa mundial com o último ano ----------
# Último ano com dados por país
latest_export <- exportdata %>%
  group_by(iso3c) %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  ungroup()

# Carrega o mapa mundial
world <- ne_countries(scale = "medium", returnclass = "sf")

# Junta os dados de exportação ao mapa
map_data <- left_join(world, latest_export, by = c("iso_a3" = "iso3c"))

graf_mapa <- ggplot(map_data) +
  geom_sf(aes(fill = NE.EXP.GNFS.ZS), color = NA) +
  scale_fill_viridis_c(option = "plasma", na.value = "gray90") +
  labs(
    title = "Exportações (% do PIB) - Último Ano Disponível",
    fill = "% do PIB"
  ) +
  theme_minimal()

print(graf_mapa)

