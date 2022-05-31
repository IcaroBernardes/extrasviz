# 0. Controle de Pacotes e Fontes
library(tidyverse)
library(ragg)
library(glue)
library(basedosdados)
library(colorspace)
library(scales)
library(circlize)
library(ComplexHeatmap)
library(ggplotify)
library(cowplot)
library(ggforce)
library(ggtext)
library(nflplotR)

## Define algumas constantes de layout
L <- 1 ### Comprimento dos lados dos hexagonos
hL <- sqrt(3)*L/2 ### Distância horizontal das fileiras de hexagonos
vL <- 3*L/2 ### Distância vertical das fileiras de hexagonos
sans <- "Federo" ### Fonte sans-serif
serif <- "Prata" ### Fonte serif

# 1. Download, carregamento e manejo dos dados
## Define os centros dos hexagonos que representam as UFs
hex <- tibble(
  uf = c("RO", "AC", "AM", "RR", "PA",
         "AP", "TO", "MA", "PI", "CE",
         "RN", "PB", "PE", "AL", "SE",
         "BA", "MG", "ES", "RJ", "SP",
         "PR", "SC", "RS", "MS", "MT",
         "GO", "DF"),
  region = c(rep("N",7),
             rep("NE",9),
             rep("SE",4),
             rep("S",3),
             rep("CO",4)),
  x0 = hL*c(1, 3, 2, 3, 4,
            5, 5, 6, 7, 8,
            10, 11, 9, 12, 10, 
            8, 7, 9, 8, 6,
            5, 6, 5, 4, 3,
            5, 6),
  y0 = vL*c(7, 7, 8, 9, 8, 
            9, 7, 8, 7, 8,
            8, 7, 7, 6, 6, 
            6, 5, 5, 4, 4,
            3, 2, 1, 4, 5,
            5, 6)
)

## Baixa os dados do PAM para lavouras permanentes e salva em um .RDS
# query1 <- basedosdados::bdplyr("br_ibge_pam.municipio_lavouras_permanentes")
# perm <- basedosdados::bd_collect(query1)
# saveRDS(perm, "agrobr/perm.RDS")

## Carrega os dados do PAM para lavouras permanentes
perm <- readRDS("agrobr/perm.RDS")

## Baixa os dados do PAM para lavouras temporarias e salva em um .RDS
# query2 <- basedosdados::bdplyr("br_ibge_pam.municipio_lavouras_temporarias")
# temp <- basedosdados::bd_collect(query2)
# saveRDS(temp, "agrobr/temp.RDS")

## Carrega os dados do PAM para lavouras temporarias
temp <- readRDS("agrobr/temp.RDS")

## Define cores para os produtos de interesse
prod_cor <- tibble(
  produto = c("Cana-de-açúcar",
              "Café (em grão) Total",
              "Cevada (em grão)",
              "Guaraná (semente)"),
  color = c("#429900",
            "#944000",
            "#998000",
            "#99002A")
)

## Une as bases de culturas temporarias e permanentes
## e filtra os cultivos e variáveis de interesse incluindo as cores temáticas
prod_uf <- rbind(temp, perm) %>%
  dplyr::select(ano, sigla_uf, produto, quantidade_produzida) %>% 
  dplyr::right_join(prod_cor)

## Calcula a produção por UF e ano e põe os produtos em escalas similares
prod_uf <- prod_uf %>% 
  dplyr::group_by(ano, sigla_uf, produto) %>% 
  dplyr::summarise(massa = sum(quantidade_produzida, na.rm = TRUE),
                   color = unique(color)) %>% 
  dplyr::ungroup()

## Calcula a produção nacional por ano e elimina zeros
prod_br <- prod_uf %>%
  dplyr::group_by(ano, produto) %>% 
  dplyr::summarise(massa = sum(massa),
                   color = unique(color)) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(massa != 0)

## Calcula a variação relativa da produção do início da série até o fim
mudanca <- prod_br %>% 
  dplyr::group_by(produto) %>% 
  dplyr::arrange(ano) %>% 
  dplyr::summarise(change = round(100*(massa[n()] - massa[1])/massa[1]),
                   y = 1.18*massa[n()]) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(direction = ifelse(change > 0, "▲", "▼"),
                label = glue::glue("<span style='font-family:Arial;'>{direction}</span> {change}%"),
                color = ifelse(change > 0, "#3DC45C", "#C44145"))

## Mantém os dados de produção por UF apenas para o ano mais recente
## e os une com as coordenadas do mapa hexagonal
prod_uf <- prod_uf %>%
  dplyr::filter(ano == 2019) %>% 
  dplyr::select(-ano) %>% 
  dplyr::left_join(hex, by = c("sigla_uf" = "uf"))

## Cria uma variável com range de 0-1 para as massas
## e rearranja os dados em listas
prod_uf <- prod_uf %>%
  dplyr::group_by(produto) %>% 
  dplyr::mutate(scaled = scales::rescale(log10(massa))) %>% 
  tidyr::nest()

## Cria uma função para definir as cores dos hexagonos
## no mapa para cada produto e UF
fun_color <- function(data) {
  
  ### Define o par de cores que define a paleta de cada produto
  colors = rep(unique(data$color), 2) %>% 
    colorspace::lighten(
      amount = c(0.8, 0)
    ) %>% 
    colorspace::desaturate(
      amount = c(0.8, 0)
    )
  
  ### Define uma função que recebe valores no range 0-1 e gera um código hexadecimal
  ramp = circlize::colorRamp2(breaks = c(0,1), colors = colors)
  
  ### Aplica a função a cada linha. Valores infinitos 
  ### (UF com produçao zero) são definidos como cinza.
  data %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(fill = ifelse(
      is.infinite(scaled), "#e6e6e6", ramp(scaled)
    )) %>% 
    dplyr::ungroup()
  
}

## Aplica a função "fun_color" e restaura os dados ao formato original
prod_uf <- prod_uf %>% 
  mutate(data = purrr::map(data, fun_color)) %>% 
  unnest(cols = data)

## Converte a variável produto em um factor (caractere ordenado)
prod_uf <- prod_uf %>% 
  dplyr::mutate(produto = factor(produto, levels = prod_cor$produto)) %>% 
  dplyr::arrange(produto)

## Cria legendas para cada mapa das UFs e converte em formato amigável para ggplot
fun_legend <- function(color, produto, data) {
  
  ### Toma os limites superior e inferior da escala
  labels = data %>% 
    dplyr::filter(!is.infinite(scaled)) %>% 
    dplyr::arrange(scaled) %>% 
    dplyr::slice(1, n()) %>% 
    dplyr::pull(massa) %>% 
    scales::number(scale_cut = cut_short_scale(),
                   accuracy = 0.1,
                   decimal.mark = ",")
  
  ### Define o par de cores que define a paleta de cada produto
  colors = rep(color, 2) %>% 
    colorspace::lighten(
      amount = c(0.8, 0)
    ) %>% 
    colorspace::desaturate(
      amount = c(0.8, 0)
    )
  
  ### Define uma função que recebe valores no range 0-1 e gera um vetor RGB
  ramp = circlize::colorRamp2(breaks = c(0,1), colors = colors)
  
  ### Cria o objeto-legenda
  lgd = ComplexHeatmap::Legend(at = c(0,1),
                               labels = labels,
                               labels_gp = gpar(fontfamily = serif, fontsize = 12),
                               col_fun = ramp,
                               direction = "horizontal",
                               legend_width = unit(80, "mm"))
  
  ### Converte a formato ggplot amigável
  lgd = ggplotify::as.ggplot(cowplot::ggdraw(lgd@grob))
  
  ### Salva as legendas
  assign(glue::glue("lgd{produto}"), lgd, envir = globalenv())
  
}

## Aplica a função às cores associadas aos produtos
prod_uf %>% 
  dplyr::group_by(color, produto) %>%
  dplyr::mutate(produto = as.numeric(produto)) %>% 
  tidyr::nest() %>% 
  purrr::pwalk(fun_legend)

## Define os textos que indicam o total produzido por UF e suas cores
prod_uf <- prod_uf %>% 
  dplyr::mutate(
    massa = scales::number(massa, scale_cut = cut_short_scale(), accuracy = 0.1,
                           decimal.mark = ","),
    label = glue::glue("<span style='font-size:15px;'>{sigla_uf}</span><br>
                       <span style='font-size:7px;'>{massa}</span>
                       <span style='font-size:5px;'>ton</span>"),
    color = ifelse(scaled >= 0.7, "white", "black"))

## Define os subtítulos do gráfico
subtitles <- tibble(
  x = 0.5,
  y = c(0.92, 0.85),
  size = c(6, 5),
  label = c(
    "Os gráficos abaixo exibem a produção em toneladas de cultivos relacionados a bebidas amplamente consumidas no Brasil.<br>
    São cultivos de <span style='color:#429900;font-size:32px;'>Cana-de-açúcar</span>,
    <span style='color:#944000;font-size:32px;'>Café (em grão) Total</span>,
    <span style='color:#998000;font-size:32px;'>Cevada (em grão)</span> e 
    <span style='color:#99002A;font-size:32px;'>Guaraná (semente)</span>.",
    
    "Os dados vêm da Pesquisa Agrícola Municipal (PAM) e foram obtidos por meio da Base dos Dados. O gráfico foi produzido por Ícaro Bernardes (@IcaroBSC)."
  )
)

# 2. Produção do gráfico
## Gera o gráfico de produção total brasileira ao longo dos anos
p1 <- prod_br %>% 
  ggplot() +
  
  ### Insere as linhas de produção nacional para cada produto
  geom_line(aes(x = ano, y = massa, color = I(color)), size = 2) +
  
  ### Insere a info. da variação de produção
  ggtext::geom_textbox(aes(x = 2019, y = y, label = label, color = I(color)),
                       fill = NA, box.color = NA, family = serif, halign = 1,
                       width = unit(0.1, "npc"), hjust = 1, vjust = 0,
                       box.padding = unit(c(0,0,0,0), "pt"), data = mudanca) + 
  
  ### Insere anotação à direita
  annotate("text", x = 2020, y = 1E+08, hjust = 0, vjust = 1, family = sans, size = 4,
           label = "As culturas tiveram\ngrande crescimento\ndesde a década de 70,\ncom exceção do Café\nque teve pequena\ndiminuição.") +
  
  ### Define título do gráfico e eixos
  labs(title = "Produção anual nacional relatada no PAM. Escala logarítimica",
       x = "Ano", y = "Produção [ton]") +
  
  ### Define a escala y em log
  scale_y_log10(labels = scales::label_number(scale_cut = cut_short_scale(),
                                              accuracy = 1,
                                              decimal.mark = ","),
                expand = expansion(mult = 0.1)) +
  
  ### Define limites para a escala x
  scale_x_continuous(breaks = seq(1980, 2020, 10), limits = c(1974, 2025)) +
  
  ### Simplifica e customiza elementos do gráfico
  theme_minimal() +
  theme(
    text = element_text(family = serif),
    
    plot.title = element_text(size = 30, family = sans,
                              margin = margin(0,0,20,0)),
    
    axis.title = element_text(size = 18),
    axis.title.x = element_text(margin = margin(20,0,0,0)),
    axis.title.y = element_text(margin = margin(0,20,0,0)),
    axis.text = element_text(size = 12),
  )

## Gera o gráfico de produção por UF no ano mais recente
p2 <- prod_uf %>% 
  ggplot() +
  
  ### Insere os hexagonos que representam as UFs
  ggforce::geom_regon(aes(x0 = x0, y0 = y0, sides = 6, r = L, angle = 11,
                          fill = I(fill)), color = "black", size = 1.5) +
  
  ### Insere as abreviaturas e valores associados às UFs
  ggtext::geom_textbox(aes(x = x0, y = y0, label = label, color = I(color)),
                       hjust = 0, nudge_x = -0.85, lineheight = 2.5, size = 1,
                       width = unit(0.3, "npc"), fill = NA, box.color = NA,
                       halign = 0, family = serif) +
  
  ### Efetua o facet por produto
  facet_grid(.~produto) +
  
  ### Define o título do gráfico
  labs(title = "Produção por UF e cultivo relatada no PAM em 2019") +
  
  ### Define coordenadas de mesma proporção entre os eixos para
  ### não distorcer os hexagonos
  coord_equal() +
  
  ### Elimina e customiza elementos do gráfico
  theme_void() +
  theme(
    text = element_text(family = serif),
    plot.title = element_text(size = 30, family = sans,
                              margin = margin(0,0,30,0)),
    strip.text = element_text(size = 15, margin = margin(0,0,10,0))
  )

## Une os gráficos
p <- NULL %>% 
  ggplot() +
  
  ### Insere um retângulo para destacar a seção central
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.485, ymax = 0.815,
           fill = "#e8d9be", color = NA) +
  
  ### Insere a figura da cana à direita
  nflplotR::geom_from_path(aes(x = 1, y = 0.82, path = "agrobr/sugarcane.png"),
                           width = 0.2, height = 0.28, hjust = 1, vjust = 1) +
  
  ### Insere a fonte da imagem de cana
  annotate("text", x = 0.98, y = 0.78, label = "Imagem: rawpixel.com",
           family = sans, angle = 90, hjust = 1, vjust = 0) +
  
  ### Insere o título do gráfico
  geom_text(aes(x = 0.5, y = 1, label = "Brasil, o país das boas bebidas!"),
            size = 25, vjust = 1, family = sans) +
  
  ### Insere os subtítulos do gráfico
  ggtext::geom_richtext(aes(x = x, y = y, label = label, size = I(size)),
                        lineheight = 1.9, fill = NA, label.color = NA,
                        vjust = 1, family = serif, data = subtitles) +
  
  ### Define os limites 
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  
  ### Elimina e customiza elementos do gráfico
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#faebcf", color = NA)
  ) +
  
  ### Insere os dois gráficos como "insets"
  cowplot::draw_plot(plot = p1,
                     x = 0, y = 0.8,
                     hjust = 0, vjust = 1,
                     width = 0.8, height = 0.3) +
  cowplot::draw_plot(plot = p2,
                     x = 0.5, y = 0.05,
                     hjust = 0.5, vjust = 0,
                     width = 1, height = 0.4) +
  
  ### Insere as legendas abaixo como "insets"
  cowplot::draw_plot(plot = lgd1,
                     x = 0.155, y = -0.02,
                     hjust = 0.5, vjust = 0,
                     width = 0.23, height = 0.1) +
  cowplot::draw_plot(plot = lgd2,
                     x = 0.385, y = -0.02,
                     hjust = 0.5, vjust = 0,
                     width = 0.23, height = 0.1) +
  cowplot::draw_plot(plot = lgd3,
                     x = 0.615, y = -0.02,
                     hjust = 0.5, vjust = 0,
                     width = 0.23, height = 0.1) +
  cowplot::draw_plot(plot = lgd4,
                     x = 0.845, y = -0.02,
                     hjust = 0.5, vjust = 0,
                     width = 0.23, height = 0.1) 

## Salva o gráfico
ggsave("agrobr/agrobr.png", plot = p, dpi = 320,
       width = 18, height = 16, device = ragg::agg_png, res = 320)

