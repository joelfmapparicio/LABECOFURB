# Carregar pacotes necessários
library(tidyverse)
library(ggplot2)
library(forecast)
library(dynlm)

# Dados simulados de 1900 a 2025
set.seed(123) # Para reprodutibilidade
anos <- 1900:2025  # 126 elementos

# Corrigir preços
preco <- c(
  runif(61, 0.5, 2) + c(rep(0, 20), rep(3, 5), rep(0, 36)),  # 1900-1960: 61 anos
  runif(65, 1, 5) + seq(0, 10, length.out = 65) + rnorm(65, 0, 1)  # 1961-2025: 65 anos
)

# Corrigir quantidades
quantidade <- c(
  seq(5, 20, length.out = 61) + rnorm(61, 0, 1),  # 1900-1960: 61 anos
  seq(20, 60, length.out = 65) + rnorm(65, 0, 5)  # 1961-2025: 65 anos
)

# Criar dataframe
dados_cafe <- data.frame(Ano = anos, Preco = preco, Quantidade = quantidade)

# Gráfico 1: Série Temporal de Preços e Quantidade
grafico_serie <- ggplot(dados_cafe, aes(x = Ano)) +
  geom_line(aes(y = Preco, color = "Preço (USD/saca)")) +
  geom_line(aes(y = Quantidade, color = "Quantidade (M sacas)")) +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Quantidade (M sacas)")) +
  labs(title = "Evolução do Preço e Quantidade de Café (1900-2025)",
       y = "Preço (USD/saca)", color = "Legenda") +
  theme_minimal() +
  scale_color_manual(values = c("Preço (USD/saca)" = "blue", "Quantidade (M sacas)" = "red"))

# Gráfico 2: Dispersão com Ajuste de Tendência
grafico_dispersao <- ggplot(dados_cafe, aes(x = Preco, y = Quantidade)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Relação entre Preço e Quantidade de Café",
       x = "Preço (USD/saca)", y = "Quantidade (M sacas)") +
  theme_minimal()

# Gráfico 3: Heatmap de Correlação por Década
dados_cafe$Decada <- cut(dados_cafe$Ano, breaks = seq(1900, 2030, by = 10), right = FALSE)
correlacao <- dados_cafe %>%
  group_by(Decada) %>%
  summarise(Correlacao = cor(Preco, Quantidade)) %>%
  na.omit()
grafico_heatmap <- ggplot(correlacao, aes(x = Decada, y = 1, fill = Correlacao)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  labs(title = "Correlação Preço-Quantidade por Década", y = "") +
  theme_minimal() +
  theme(axis.text.y = element_blank())

# Calcular Elasticidade-Preço da Demanda
modelo <- dynlm(log(Quantidade) ~ log(Preco), data = dados_cafe)
elasticidade <- coef(modelo)["log(Preco)"]

# Exibir resultados
print(grafico_serie)
print(grafico_dispersao)
print(grafico_heatmap)
cat("Elasticidade-Preço da Demanda:", elasticidade, "\n")
summary(modelo)

# Salvar gráficos (opcional)
ggsave("serie_temporal.png", grafico_serie, width = 10, height = 6)
ggsave("dispersao.png", grafico_dispersao, width = 10, height = 6)
ggsave("heatmap.png", grafico_heatmap, width = 10, height = 6)