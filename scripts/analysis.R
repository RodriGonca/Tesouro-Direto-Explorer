
library(Quandl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(ggthemes)

# source scripts -------------------------------------------------------------

reticulate::py_run_file('./scripts/data_download.py')
source('./scripts/tidy_data.R')

# analisys -------------------------------------------------------------------

## get Selic data
selic               <- Quandl("BCB/4189", api_key="-xkrwaz77Fyu-Wd-fs2y")
selic               <- filter(selic, Date >= make_date(2002, 1, 1))
selic$Value         <- selic$Value / 100 
selic_target        <- Quandl("BCB/432", api_key="-xkrwaz77Fyu-Wd-fs2y")
selic_target        <- filter(selic_target, Date >= make_date(2002, 1, 1))
selic_target$Value  <- selic_target$Value / 100

## get IPCA data
ipca_12m        <- Quandl("BCB/13522", api_key="-xkrwaz77Fyu-Wd-fs2y")
ipca_12m        <- filter(ipca_12m, Date >= make_date(2002, 1, 1))
ipca_12m$Value  <- ipca_12m$Value / 100

## remove outliers -----------------------------------------------------------
bond_data <- filter(bond_data, !pu_compra == 0)
bond_data <- filter(bond_data, !pu_venda == 0)

## Histórico da Taxa de Venda - NTN-B ----------------------------------------
NTNB_premium_trend <- left_join(bond_data, bond_names, by = 'classe') %>%
  left_join(bond_status, by = 'titulo') %>% 
  filter(classe == 'NTN-B' | classe == 'NTN-B Princ',
         status == 'active') %>%
  ggplot(aes(x = dia)) +
  geom_line(aes(color = titulo, y = taxa_venda), size = 0.2) +
  geom_smooth(aes(y = taxa_compra), method = 'loess') +
  theme_economist() +
  scale_x_date(date_breaks = '6 month', date_labels = "%b %y") +
  scale_y_continuous(labels = function(x) paste0( x *100, "%"),
                     limits = c(0, 0.2)) +
  labs(y = 'Taxa de Venda %',
       x = 'Data',
       title = 'Histórico da Taxa de Venda - NTN-B')

NTNB_premium_trend <- ggplotly(NTNB_premium_trend)

NTNB_premium_trend

## Histórico da Taxa de Venda - NTN-B ----------------------------------------
prefix_trend <- left_join(bond_data, bond_names, by = 'classe') %>%
  left_join(bond_status, by = 'titulo') %>% 
  filter(classe == 'LTN' | classe == 'NTN-F',
         status == 'active') %>%
  ggplot(aes(x = dia)) +
  geom_line(aes(color = titulo, y = taxa_venda), size = 0.2) +
  geom_smooth(aes(y = taxa_compra), method = 'loess') +
  theme_economist() +
  scale_x_date(date_breaks = '6 month', date_labels = "%b %y") +
  scale_y_continuous(labels = function(x) paste0(x*100, "%"),
                     limits = c(0, 0.2)) +
  labs(y = 'Taxa de Venda %',
       x = 'Data',
       title = 'Histórico da Taxa de Venda - pré-fixado')

prefix_trend <- ggplotly(prefix_trend)

prefix_trend

## Histórico da Taxa de Venda - LFT ----------------------------------------
posfix_trend <- left_join(bond_data, bond_names, by = 'classe') %>%
  left_join(bond_status, by = 'titulo') %>% 
  filter(classe == 'LFT',
         status == 'active') %>%
  ggplot(aes(x = dia)) +
  geom_line(aes(color = titulo, y = pu_venda), size = 0.2) +
  theme_economist() +
  scale_x_date(date_breaks = '6 month', date_labels = "%b %y") +
  scale_y_continuous(labels = function(x) paste0(x, " R$")) +
  labs(y = 'Taxa de Venda %',
       x = 'Data',
       title = 'Histórico da Preço de Venda - pós-fixado')

posfix_trend <- ggplotly(posfix_trend)

posfix_trend

## Histórico da Selic + Inflação -------------------------------------------
selic_inflation <- ggplot() +
  geom_line(data = selic, aes(x = Date, y = Value, color = 'Selic')) +
  geom_line(data = ipca_12m, aes(x = Date, y = Value, color = 'IPCA 12m')) +
  theme_economist() +
  scale_x_date(date_breaks = '6 month', date_labels = "%b %y") +
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  scale_color_manual(values = c('Selic' = 'darkblue', 'IPCA 12m' = 'darkred'),
                     name = NULL) +
  labs(y = '% Anualizado',
       x = 'Data',
       title = 'Inflação e Selic')

selic_inflation <- ggplotly(selic_inflation)

selic_inflation
