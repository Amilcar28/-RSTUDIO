library(ncdf4)
setwd("C:/Users/Amilcar/Documents/CRU/") #definir diretorio
getwd()
dir()

library(ncdf4)
library(SPEI)
library(raster)
library(tidyverse)
library(stars)
library(zoo)
library(reshape2)
library(tidyr)
library(zoo)


# Abrir arquivos NetCDF do CRU
pre_nc <- nc_open("cru_ts4.08.1901.2023.pre.dat.nc")
pet_nc <- nc_open("cru_ts4.08.1901.2023.pet.dat.nc")

# Extrair dimensões e variáveis
lats <- ncvar_get(pre_nc, "lat")  # Latitude
lons <- ncvar_get(pre_nc, "lon")  # Longitude
time <- ncvar_get(pre_nc, "time") # Tempo em meses desde 1900-01-01

# Variáveis: Precipitação e Evapotranspiração
precip <- ncvar_get(pre_nc, "pre")  # Precipitação (mm/month)
pet <- ncvar_get(pet_nc, "pet")     # Evapotranspiração (mm/day)

# Fechar arquivos para liberar memória
nc_close(pre_nc)
nc_close(pet_nc)

# Converter o tempo para formato de data
start_date <- as.Date("1900-01-01")
dates <- seq(start_date, by = "month", length.out = length(time))

# Calcular dias por mês
days_in_month <- as.numeric(format(dates, "%d"))

# Converter PET para mm/month
pet_monthly <- pet * days_in_month

# Limites geográficos da região sul de Angola
lat_sul <- which(lats >= -18 & lats <= -12)
lon_sul <- which(lons >= 10 & lons <= 23)

# Filtrar dados para a região
precip_sul <- precip[lon_sul, lat_sul, ]
pet_sul <- pet_monthly[lon_sul, lat_sul, ]

# Calcular média espacial para precipitação e PET
mean_precip_sul <- apply(precip_sul, 3, mean, na.rm = TRUE)
mean_pet_sul <- apply(pet_sul, 3, mean, na.rm = TRUE)

library(zoo)

# Criar séries temporais
ts_precip <- ts(mean_precip_sul, start = c(1980, 1), frequency = 12)
ts_pet <- ts(mean_pet_sul, start = c(1980, 1), frequency = 12)

# Calcular balanço hídrico
balanco_hidrico <- ts_precip - ts_pet

library(SPEI)

# Calcular SPI para 1, 3, 6, 12 e 24 meses
spi_1 <- spi(ts_precip, scale = 1)
spi_3 <- spi(ts_precip, scale = 3)
spi_6 <- spi(ts_precip, scale = 6)
spi_12 <- spi(ts_precip, scale = 12)
spi_24 <- spi(ts_precip, scale = 24)

# Calcular SPEI para 1, 3, 6, 12 e 24 meses
spei_1 <- spei(balanco_hidrico, scale = 1)
spei_3 <- spei(balanco_hidrico, scale = 3)
spei_6 <- spei(balanco_hidrico, scale = 6)
spei_12 <- spei(balanco_hidrico, scale = 12)
spei_24 <- spei(balanco_hidrico, scale = 24)

# Salvar SPI
write.csv(spi_1$fitted, "SPI_1_CRU.csv", row.names = FALSE)
write.csv(spi_3$fitted, "SPI_3_CRU.csv", row.names = FALSE)
write.csv(spi_6$fitted, "SPI_6_CRU.csv", row.names = FALSE)
write.csv(spi_12$fitted, "SPI_12_CRU.csv", row.names = FALSE)
write.csv(spi_24$fitted, "SPI_24_CRU.csv", row.names = FALSE)

# Salvar SPEI
write.csv(spei_1$fitted, "SPEI_1_CRU.csv", row.names = FALSE)
write.csv(spei_3$fitted, "SPEI_3_CRU.csv", row.names = FALSE)
write.csv(spei_6$fitted, "SPEI_6_CRU.csv", row.names = FALSE)
write.csv(spei_12$fitted, "SPEI_12_CRU.csv", row.names = FALSE)
write.csv(spei_24$fitted, "SPEI_24_CRU.csv", row.names = FALSE)

# Exemplo: Comparar SPI e SPEI de 12 meses
# Plotar SPI-12

# Plot para SPI-1
plot(spi_1$fitted, type = "l", col = "blue", ylim = c(-3, 3), 
     main = "SPI-1 (1 mês)", ylab = "Índice", xlab = "Tempo")

# Plot para SPI-3
plot(spi_3$fitted, type = "l", col = "green", ylim = c(-3, 3), 
     main = "SPI-3 (3 meses)", ylab = "Índice", xlab = "Tempo")

# Plot para SPI-6
plot(spi_6$fitted, type = "l", col = "orange", ylim = c(-3, 3), 
     main = "SPI-6 (6 meses)", ylab = "Índice", xlab = "Tempo")

# Plot para SPI-12
plot(spi_12$fitted, type = "l", col = "red", ylim = c(-3, 3), 
     main = "SPI-12 (12 meses)", ylab = "Índice", xlab = "Tempo")

# Plot para SPI-24
plot(spi_24$fitted, type = "l", col = "purple", ylim = c(-3, 3), 
     main = "SPI-24 (24 meses)", ylab = "Índice", xlab = "Tempo")



library(ggplot2)

# Datas de 1980 a 2022
dates <- seq(as.Date("1980-01-01"), as.Date("2022-12-01"), by = "month")

# Usar os valores ajustados de SPI para SPI-1
spi_1_values <- spi_1$fitted  # Valores de SPI-1 calculados

# Garantir que o comprimento dos valores de SPI-1 coincide com as datas
spi_1_values <- spi_1_values[1:length(dates)]  # Ajusta para coincidir com o comprimento do vetor de datas

# Criar um data frame com as datas e os valores de SPI-1
spi_1_data <- data.frame(
  Date = dates,
  SPI_1 = spi_1_values
)

# Plotar SPI-1 com fundo branco e barras coloridas
spi_1_plot <- ggplot(spi_1_data, aes(x = Date, y = SPI_1, fill = ifelse(SPI_1 >= 0, "Positivo", "Negativo"))) +
  geom_col(show.legend = FALSE) +  # Gráfico de barras
  scale_fill_manual(values = c("Positivo" = "blue", "Negativo" = "red")) +  # Azul para positivo, vermelho para negativo
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Linha de referência no eixo Y
  coord_cartesian(ylim = c(-3, 3)) +  # Ajustar os limites do eixo Y
  scale_y_continuous(breaks = seq(-3, 3, by = 1)) +  # Rótulos do eixo Y em incrementos de 1
  labs(
    title = "SPI-1 (Mês)",  # Título do gráfico
    x = NULL,
    y = "SPI-1"
  ) +
  scale_x_date(
    limits = c(as.Date("1980-01-01"), as.Date("2022-12-01")),  # Período completo
    breaks = seq(as.Date("1980-01-01"), as.Date("2022-12-01"), by = "2 years"),  # Intervalos de 2 anos no eixo X
    date_labels = "%Y"  # Exibir apenas o ano
  ) +
  theme(
    panel.background = element_rect(fill = "white", color = "black"),  # Fundo branco com borda preta
    plot.background = element_rect(fill = "white", color = "black"),  # Fundo total branco com borda preta
    panel.grid.major = element_blank(),  # Remover grades maiores
    panel.grid.minor = element_blank(),  # Remover grades menores
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),  # Título centralizado
    axis.text.x = element_text(angle = 0, hjust = 0.5),  # Rótulos do eixo X na horizontal
    axis.title = element_text(face = "bold")  # Títulos dos eixos em negrito
  )

# Mostrar o gráfico
print(spi_1_plot)

# Salvar o gráfico como imagem utilizando ggsave
ggsave(filename = "SPI_1_1980_2022.png", plot = spi_1_plot, width = 8, height = 6, dpi = 300, bg = "white")


#Para o SPEI PLOT

# Datas de 1980 a 2022
dates <- seq(as.Date("1980-01-01"), as.Date("2022-12-01"), by = "month")

# Usar os valores ajustados de SPI para SPI-1
spei_1_values <- spei_1$fitted  # Valores de SPI-1 calculados

# Garantir que o comprimento dos valores de SPI-1 coincide com as datas
spei_1_values <- spei_1_values[1:length(dates)]  # Ajusta para coincidir com o comprimento do vetor de datas

# Criar um data frame com as datas e os valores de SPI-1
spei_1_data <- data.frame(
  Date = dates,
  SPEI_1 = spei_1_values
)

# Plotar SPI-1 com fundo branco e barras coloridas
spei_1_plot <- ggplot(spei_1_data, aes(x = Date, y = SPEI_1, fill = ifelse(SPEI_1 >= 0, "Positivo", "Negativo"))) +
  geom_col(show.legend = FALSE) +  # Gráfico de barras
  scale_fill_manual(values = c("Positivo" = "blue", "Negativo" = "red")) +  # Azul para positivo, vermelho para negativo
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Linha de referência no eixo Y
  coord_cartesian(ylim = c(-3, 3)) +  # Ajustar os limites do eixo Y
  scale_y_continuous(breaks = seq(-3, 3, by = 1)) +  # Rótulos do eixo Y em incrementos de 1
  labs(
    title = "SPEI-1 (Mês)",  # Título do gráfico
    x = NULL,
    y = "SPEI-1"
  ) +
  scale_x_date(
    limits = c(as.Date("1980-01-01"), as.Date("2022-12-01")),  # Período completo
    breaks = seq(as.Date("1980-01-01"), as.Date("2022-12-01"), by = "2 years"),  # Intervalos de 2 anos no eixo X
    date_labels = "%Y"  # Exibir apenas o ano
  ) +
  theme(
    panel.background = element_rect(fill = "white", color = "black"),  # Fundo branco com borda preta
    plot.background = element_rect(fill = "white", color = "black"),  # Fundo total branco com borda preta
    panel.grid.major = element_blank(),  # Remover grades maiores
    panel.grid.minor = element_blank(),  # Remover grades menores
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),  # Título centralizado
    axis.text.x = element_text(angle = 0, hjust = 0.5),  # Rótulos do eixo X na horizontal
    axis.title = element_text(face = "bold")  # Títulos dos eixos em negrito
  )

# Mostrar o gráfico
print(spei_1_plot)

# Salvar o gráfico como imagem utilizando ggsave
ggsave(filename = "SPEI_1_1980_2022.png", plot = spei_1_plot, width = 8, height = 6, dpi = 300, bg = "white")

library(ggplot2)
library(sf)
dir()
# Carregar shapefile das regiões e adicionar SPI por região
mapa <- st_read("Sul_angola_atualizado.shp")  # Substitua pelo shapefile correto
mapa$SPI_1 <- valores_spi_suldeangola  # Insira os dados correspondentes ao SPI-12

ggplot(mapa) +
  geom_sf(aes(fill = SPI_12)) +
  scale_fill_gradient2(low = "red", mid = "yellow", high = "blue", midpoint = 0) +
  labs(title = "SPI-12 na Região Sul de Angola", fill = "SPI-12") +
  theme_minimal()


summary(spi_1$fitted)  # Estatísticas básicas
mean(spi_1$fitted, na.rm = TRUE)  # Média
sd(spi_1$fitted, na.rm = TRUE)  # Desvio padrão

# Contar eventos de seca severa (SPI < -2)
length(spi_1$fitted[spi_1$fitted < -2])


