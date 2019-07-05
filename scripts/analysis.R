
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(ggthemes)

# source scripts -------------------------------------------------------------

#source('./scripts/tidy_data.R')

# analisys -------------------------------------------------------------------

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
  scale_y_continuous(labels = function(x) paste0(x*100, "%"),
                     limits = c(0, 0.1)) +
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
