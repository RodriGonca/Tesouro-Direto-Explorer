library(dplyr)
library(stringr)
library(lubridate)
library(readr)
library(readxl)
library(ggplot2)
library(plotly)
library(ggthemes)

# tidy data from raw files ----------------------------------------------------------

files = list.files('data/', full.names = TRUE)

temp_false = tibble()
temp_true = tibble()

for (file in files) {
  if (grepl('historico', file) == FALSE) {
    print(file)
    readxl::excel_sheets(file)
    for (sheet in excel_sheets(file)) {
      temp_false = read_xls(file, skip = 1, sheet = sheet) %>% 
        mutate(classe = trimws(str_extract(trimws(sheet), '[A-Z|\\-\\s|[A-Za-z]]+')),
               vencimento = dmy(str_extract(trimws(sheet), '\\d+$')),
               titulo = paste(classe, vencimento),
               source = file) %>% 
        bind_rows(temp_false)
    }
  } else {
      print(file)
      readxl::excel_sheets(file)
      for (sheet in excel_sheets(file)) {
        temp_true = read_xls(file, skip = 1, sheet = sheet, col_types = 'text') %>% 
          mutate(classe = trimws(str_extract(trimws(sheet), '[A-Z|\\-\\s|[A-Za-z]]+')),
                 vencimento = dmy(str_extract(trimws(sheet), '\\d+$')),
                 titulo = paste(classe, vencimento),
                 source = file) %>% 
          bind_rows(temp_true)
      }    
  }
}

temp_true <- filter(temp_true, !is.na(temp_true$Dia)) %>%
  select(-matches('\\.')) %>% 
  mutate(Dia = as.Date(as.numeric(Dia), origin = "1899-12-30", optional = TRUE)) %>% 
  type_convert() %>% 
  mutate(pu_base = ifelse(is.na(`PU Extrato 9:00`), `PU Base 9:00`, `PU Extrato 9:00`)) %>% 
  select(Dia, `Taxa Compra 9:00`, `Taxa Venda 9:00`, `PU Compra 9:00`, `PU Venda 9:00`,
         `pu_base`, classe, vencimento, titulo, source)

temp_false <- filter(temp_false, !is.na(temp_false$Dia)) %>%
  mutate(dia1 = parse_date(Dia, format = '%d/%m/%Y'),
         dia2 = as.Date(as.numeric(Dia), origin = "1899-12-30", optional = TRUE),
         Dia = as.Date(as.numeric(ifelse(is.na(dia1), dia2, dia1)), 
                       origin = "1970-01-01", optional = TRUE)) %>% 
  select(-dia1, -dia2)

colnames(temp_false) <- c('dia', 'taxa_compra', 'taxa_venda',
                          'pu_compra', 'pu_venda', 'pu_base',
                          'classe', 'vencimento', 'titulo', 'source')

colnames(temp_true) <- c('dia', 'taxa_compra', 'taxa_venda',
                         'pu_compra', 'pu_venda', 'pu_base',
                         'classe', 'vencimento', 'titulo', 'source')

df <- bind_rows(temp_false, temp_true)
df <- filter(df, !is.na(df$taxa_compra))

df$classe = plyr::mapvalues(df$classe, 
                            c('NTNB', 'NTNBP', 'NTNC', 'NTNF', 'NTN-B Principal'),
                            c('NTN-B', 'NTN-B Princ', 'NTN-C', 'NTN-F', 'NTN-B Princ'))

df$titulo <- paste(df$classe, df$vencimento)


rm(temp_false, temp_true, file, files, sheet)
gc()


bond_names = tibble(classe = c('LFT', 'LTN', 'NTN-B', 'NTN-B Princ', 'NTN-C', 'NTN-F'),
                    old_name = c('Letra Financeira do Tesouro', 
                                 'Letra do Tesouro Nacional', 
                                 'Notas do Tesouro Nacional – Série B', 
                                 'Notas do Tesouro Nacional – Série B Principal', 
                                 'Notas do Tesouro Nacional – Série C', 
                                 'Notas do Tesouro Nacional – Série F'),
                    new_name = c('Tesouro selic', 
                                 'Tesouro pré-fixado', 
                                 'Tesouro IPCA com juros semestrais', 
                                 'Tesouro IPCA', 
                                 'Tesouro IGPM com juros semestrais', 
                                 'Tesouro pré-fixado com juros semestrais'),
                    type = c('pós-fixado',
                             'pré-fixado',
                             'índice de preço',
                             'índice de preço',
                             'índice de preço',
                             'pré-fixado'),
                    juros_semestrais = c(FALSE,FALSE,TRUE,FALSE,TRUE,TRUE))

# analisys --------- ----------------------------------------------------------

df <- filter(df, !pu_compra == 0)
df <- filter(df, !pu_venda == 0)

bond_status <- group_by(df, titulo) %>% 
  summarise(start = min(dia),
            vencimento = max(vencimento),
            status = ifelse(today()<=vencimento, 'active', 'expired'))


p <- left_join(df, bond_names, by = 'classe') %>%
  left_join(bond_status, by = 'titulo') %>% 
  filter(classe == 'NTN-B' | classe == 'NTN-B Princ',
         status == 'active') %>%
  ggplot(aes(x = dia)) +
    geom_line(aes(color = titulo, y = taxa_venda), size = 0.2) +
    geom_smooth(aes(y = taxa_compra)) +
    theme_economist() +
  scale_x_date(date_breaks = '6 month', date_labels = "%b %y") +
  scale_y_continuous(labels = function(x) paste0(x*100, "%"),
                     limits = c(0, 0.1))

p <- ggplotly(p)

p  
