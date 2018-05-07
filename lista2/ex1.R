---
title: "MAE0217 - Estatística Descritiva - Lista 2"
author: "Guilherme Marthe"
date: "April 18, 2018"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(
	echo = FALSE,
	message = FALSE,
	warning = FALSE
)
suppressPackageStartupMessages(library(tidyverse))  
suppressPackageStartupMessages(library(lubridate))
theme_set(theme_minimal())

```

## Exercício 1

__Os dados apresentados na tabela abaixo referem-se a instantes nos quais o centro de controle operacional de estradas rodoviárias recebeu chamados solicitando algum tipo de auxílio em duas estradas, num determinado dia.__


```{r message=FALSE, warning=FALSE}
df <- read_csv('dados_ex1.csv', col_types = cols(.default = 'c')) %>% rename(estrada = tipo)

horas <- 
  df %>% select(starts_with('hora'), estrada) %>% 
  gather(key, value, -estrada) %>% 
    separate(key, into = c('hora', 'id')) %>%   
  mutate(id = if_else(is.na(id), '0', id))
periodos <-  
  df %>% select(starts_with('periodo'), estrada) %>% 
  gather(key, value = 'AM_PM', -estrada) %>% 
    separate(key, into = c('periodo', 'id')) %>%   
  mutate(id = if_else(is.na(id), '0', id))
df <- 
left_join(horas, periodos, cy = c('estrada', 'id')) %>% 
  select(-periodo, -hora) %>% 
  mutate(hora_dia = glue::glue('20180418T{value}{am_pm}', value = value, am_pm = AM_PM)) %>% 
  mutate(hora_dia = ymd_hms(hora_dia)) %>% 
  select(-value, -AM_PM, -id)

df %>% group_by(estrada) %>% 
  arrange(hora_dia) %>% 
  mutate(posto = 1:n()) %>% 
  ungroup() %>% 
  mutate(estrada = paste0('Estrada ', estrada)) %>% 
  spread(estrada, hora_dia) %>% select(-posto)

```


#### a) Construa um histograma para a distribuição de frequências de chamados em cada uma das estradas:

```{r fig.height=5, fig.width=8.1, message=FALSE, warning=FALSE}
df %>% 
  mutate(estrada = paste0('Estrada ', estrada)) %>% 
  ggplot(aes(hora_dia)) + 
  geom_histogram(fill = 'skyblue', color = 'black', bins = 24) +
  facet_wrap(~estrada, ncol = 1, scales = 'free_x') + 
  scale_x_datetime(date_breaks = '1 hour', date_labels = '%H:%M' )+ 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(title = 'Distribuição de frequência de chamados', x = 'hora do dia', y = 'contagem') 

```

#### b) Calcule os intervalos de tempo entre as sucessivas chamadas e descreva-os, para cada uma das estradas, utilizando medidas resumo gráficos do tipo boxplot. Existe alguma relação entre o tipo de estrada e o intervalo de tempo entre as chamadas?  

Observando o gráfico a seguir, existem indicativas de que o tempo entre chamadas da estrada 1 é menor que na estrada 2. Isso se dá ao fato de que a mediana do tempo na estrada 1 é menor que na estrada 2.


```{r}
df %>% group_by(estrada) %>% 
  arrange(hora_dia) %>% 
  mutate(posto = 1:n()) %>% 
  mutate(tempo_entre_chamadas = hora_dia - lag(hora_dia)) %>% 
  mutate(tempo_entre_chamadas = tempo_entre_chamadas/dminutes()) %>% 
  ungroup() %>% 
  mutate(estrada = paste0('Estrada ', estrada)) %>% 
  na.omit() %>% 
  ggplot(aes(y = tempo_entre_chamadas, x = estrada, group = estrada)) +
  geom_boxplot(width = 0.3, fill = 'skyblue3') + 
  labs(title = 'Comparação da distribuição de tempos entre chamadas', 
       y = 'tempo entre chaamdas (minutos)',
       x = NULL)
```








