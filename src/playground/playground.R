# source scripts --------------------------------------------------------------

# source("./src/datapreparation/execute_data_preparation.R")

# analisys --------------------------------------------------------------------

## Histórico da Taxa de Venda - NTN-B -----------------------------------------
plot_NTNB_premium_trend <- left_join(bond_data, bond_names, by = 'classe') %>%
  left_join(bond_status, by = 'titulo') %>%
  filter(classe == 'NTN-B' | classe == 'NTN-B Princ',
         status == 'active') %>%
  ggplot(aes(x = dia)) +
  geom_line(aes(color = titulo, y = taxa_venda), size = 0.2) +
  geom_smooth(aes(y = taxa_compra), method = 'loess', formula = 'y ~ x') +
  theme_economist() +
  scale_x_date(date_breaks = '6 month', date_labels = "%b %y") +
  scale_y_continuous(labels = function(x) paste0( x * 100, "%"),
                     limits = c(0, 0.1)) +
  labs(y = 'Taxa de Venda %',
       x = 'Data',
       title = 'Histórico da Taxa de Venda - NTN-B')

plot_NTNB_premium_trend <- ggplotly(plot_NTNB_premium_trend)

plot_NTNB_premium_trend

## Histórico da Preco de Venda - NTN-B ----------------------------------------
plot_NTNB_value_trend <- left_join(bond_data, bond_names, by = 'classe') %>%
  left_join(bond_status, by = 'titulo') %>%
  filter(classe == 'NTN-B' | classe == 'NTN-B Princ',
         status == 'active', titulo == 'NTN-B Princ 2035-05-15') %>%
  ggplot(aes(x = dia)) +
  geom_line(aes(color = 'Compra', y = pu_venda), size = 0.2) +
  geom_line(aes(color = 'Venda', y = pu_compra), size = 0.2) +
  geom_smooth(aes(y = pu_venda), method = 'loess', formula = 'y ~ x') +
  theme_economist() +
  scale_x_date(date_breaks = '6 month', date_labels = "%b %y") +
  labs(y = 'Preços',
       x = 'Data',
       title = 'Histórico do Preços - NTN-B')

plot_NTNB_value_trend <- ggplotly(plot_NTNB_value_trend)

plot_NTNB_value_trend

## Histórico da Taxa de Venda - LTN -------------------------------------------
plot_prefix_trend <- left_join(bond_data, bond_names, by = 'classe') %>%
  left_join(bond_status, by = 'titulo') %>%
  filter(classe == 'LTN' | classe == 'NTN-F',
         status == 'active') %>%
  ggplot(aes(x = dia)) +
  geom_line(aes(color = titulo, y = taxa_venda), size = 0.2) +
  geom_smooth(aes(y = taxa_compra), method = 'loess', formula = 'y ~ x') +
  theme_economist() +
  scale_x_date(date_breaks = '6 month', date_labels = "%b %y") +
  scale_y_continuous(labels = function(x) paste0(x * 100, "%"),
                     limits = c(0, 0.2)) +
  labs(y = 'Taxa de Venda %',
       x = 'Data',
       title = 'Histórico da Taxa de Venda - pré-fixado')

plot_prefix_trend <- ggplotly(plot_prefix_trend)

plot_prefix_trend

## Histórico da Taxa de Venda - LFT -------------------------------------------
plot_posfix_trend <- left_join(bond_data, bond_names, by = 'classe') %>%
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

plot_posfix_trend <- ggplotly(plot_posfix_trend)

plot_posfix_trend

## Histórico da Selic + Inflação ----------------------------------------------
plot_selic_inflation <- ggplot() +
  geom_line(data = selic_actual, aes(x = Date, y = Value,
                                     color = 'Selic Annual')) +
  geom_line(data = ipca_12m, aes(x = Date, y = Value, color = 'IPCA 12m')) +
  theme_economist() +
  scale_x_date(date_breaks = '6 month', date_labels = "%b %y") +
  scale_y_continuous(labels = function(x) paste0(x * 100, "%")) +
  scale_color_manual(values = c('Selic Annual' = 'darkblue',
                                'IPCA 12m' = 'darkred'),
                     name = NULL) +
  labs(y = '% Anualizado',
       x = 'Data',
       title = 'Inflação e Selic')

plot_selic_inflation <- ggplotly(plot_selic_inflation)

plot_selic_inflation


## - Curva de Juros

library(av)

my_breaks_function <- function(y) seq(floor(min(y)), ceiling(max(y)), by = step)

save_plot <- function(df) {

  png(paste('images/Curva de juros da taxa compra de títulos pré fixados em', format(df$dia, '%Y%m%d'), '.png'),
      width = 3840,
      height = 2160)

  if (min(df$taxa_compra) < min_taxa_compra) {
    min_taxa_compra <<- min(df$taxa_compra)
    print(paste('min updated to ', min_taxa_compra))
  }


  if (max(df$taxa_compra) > max_taxa_compra) {
    print(paste('max updated from ', max_taxa_compra))
    max_taxa_compra <<- max(df$taxa_compra)
    print(paste('max updated to ', max(df$taxa_compra)))
  }

  min_taxa_compra = floor(round(min_taxa_compra * 100, 2)) / 100
  max_taxa_compra = ceiling(round(max_taxa_compra * 100, 2)) / 100
  
  print(  
    ggplot(data=df) +
      geom_label(aes(
                  x = vencimento,
                  y = taxa_compra * 1,
                  label = paste(format(vencimento, '%b-%Y'),
                                '                  ',
                                format(taxa_compra * 100, nsmall=2), '%')
                ),
                fill='grey',
                alpha=1,
                size=9,
                label.size=0,
                label.padding = unit(1, "lines")) +
      geom_point(aes(x = vencimento,
                     y = taxa_compra,
                     color = 'Taxa Compra'),
                 size = 10) +
      stat_smooth(aes(x = vencimento,
                      y = taxa_compra),
                  method = "lm",
                  formula = y ~ poly(x, 2),
                  se = FALSE,
                  size = 1.5,
                  color = 'blue',
                  linetype = 'dotted') +
      scale_x_date(date_labels = "%b-%Y") +
      scale_y_continuous(labels = scales::percent,
                         limits = c(min_taxa_compra, max_taxa_compra),
                         breaks = seq(min_taxa_compra, max_taxa_compra, by = 0.01)
                         ) +
      #expand_limits(y=min_taxa_compra) +
      scale_color_manual(values = c('Taxa Compra' = 'red'),
                         name = NULL) +
      labs(y = element_blank(),
           x = 'Vencimento',
           title = paste('Curva de juros da taxa compra de títulos pré fixados em', format(df$dia, '%d-%b-%Y'))) +
      facet_wrap(~classe_name,
                 ncol=1,
                 scales="free") +
      theme(legend.position="none",
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.text.y = element_text(size=32),
            axis.text = element_text(size=32),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            plot.title = element_text(size=64, 
                                      hjust = 0.5),
            strip.text = element_text(size=32)
            )
    )
  
  dev.off()

}

classes_filter <- c('NTN-B', 'NTN-B Princ')
#classes_filter <- c('LTN')

dias <- sort(unique(bond_data$dia), decreasing = FALSE)
dias <- dias[dias>="2021-01-01"]


df <- filter(bond_data, dia==dia[1], classe %in% classes_filter)

min_taxa_compra = min(df$taxa_compra)
max_taxa_compra = max(df$taxa_compra)


for (dia_unique in dias) {
  
  df <- filter(bond_data, dia==dia_unique, classe %in% classes_filter)
  df$classe_name = plyr::mapvalues(df$classe, bond_names$classe, bond_names$new_name, warn_missing = FALSE)

  save_plot(df)
  
}
  

av_encode_video(list.files('images/', full.names = TRUE),
                framerate = as.integer(length(dias) / 30),
                output = 'NTN-B 2021.mp4')
