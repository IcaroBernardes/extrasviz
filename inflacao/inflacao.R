# 0. Controle de Pacotes e Fontes
library(tidyverse)
library(basedosdados)
library(colorspace)
library(ggforce)
library(ggstream)
library(ggtext)
library(lubridate)
library(primerdesignR)
library(ragg)
library(readr)

## Define algumas constantes de layout
fonte_titulo <- "Federo" ### Fonte sans-serif (título)
fonte_corpo <- "Bitter" ### Fonte sans-serif (corpo)
lineheight <- 1.3 ### Altura entre linhas

# 1. Download, carregamento e manejo dos dados
## Carrega dados de 2020/2021 através de query à Base dos Dados
# query <- basedosdados::bdplyr("br_ibge_ipca.mes_categoria_brasil")
# df <- basedosdados::bd_collect(query)

## Carrega dados de 2020/2021 a partir de arquivo armazenado localmente
rawdata_1 <- readr::read_csv("inflacao/mes_categoria_brasil.csv")

## Carrega dados de 2022 a partir de arquivo armazenado localmente (Tabela 7060)
rawdata_2 <- readr::read_csv("inflacao/sidra.csv", skip = 2)

## Adequa nomes e variáveis do banco de 2022 para refletir os de 2020/2021
rawdata_2 <- rawdata_2 %>% 
  stats::na.exclude() %>% 
  tidyr::separate(col = "Mês", into = c("mes","ano"), sep = " ") %>% 
  dplyr::mutate(mes = factor(mes, levels = unique(mes)),
                mes = as.numeric(mes)) %>% 
  dplyr::rename(categoria = "Geral, grupo, subgrupo, item e subitem",
                peso_mensal = "IPCA - Peso mensal (%)",
                variacao_mensal = "IPCA - Variação mensal (%)",
                variacao_anual = "IPCA - Variação acumulada no ano (%)",
                variacao_doze_meses = "IPCA - Variação acumulada em 12 meses (%)") %>% 
  dplyr::select(-`...3`) %>% 
  dplyr::mutate(categoria = stringr::str_replace(categoria, "Índice geral", "0.Índice geral")) %>% 
  tidyr::separate(col = "categoria", into = c("id_categoria","categoria"), sep = "\\.") %>% 
  dplyr::mutate(id_categoria_bd = stringr::str_pad(
    string = id_categoria, width = 7, side = "right", pad = "0")
  ) %>% 
  dplyr::mutate(id_categoria = ifelse(id_categoria == 0, NA, id_categoria)) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(id_categoria_bd = primerdesignR::insert_str(id_categoria_bd, rep(".", 3), c(2,3,5))) %>% 
  dplyr::ungroup()

## Une os bancos
rawdata <- rawdata_2 %>% 
  dplyr::mutate(across(.cols = c(ano, id_categoria), .fns = as.numeric))
rawdata <- rawdata_1 %>% 
  dplyr::bind_rows(rawdata) %>% 
  dplyr::arrange(ano, mes, id_categoria)

## Confirma que os dados em categorias estão coerentes com o total
teste <- rawdata %>% 
  dplyr::mutate(is_total = ifelse(is.na(id_categoria), "Total", "Categorias")) %>% 
  dplyr::filter(id_categoria %in% 1:9 | is.na(id_categoria)) %>% 
  dplyr::mutate(contrib = (peso_mensal/100)*variacao_mensal) %>% 
  dplyr::group_by(ano, mes, is_total) %>% 
  dplyr::summarise(contrib = round(sum(contrib), 2)) %>% 
  dplyr::ungroup() %>% 
  tidyr::pivot_wider(names_from = is_total, values_from = contrib) %>% 
  dplyr::mutate(diff = Total-Categorias)

## Mantém apenas valores das categorias principais
df <- rawdata %>% dplyr::filter(id_categoria %in% 1:9)

## Converte mês/ano a data
df <- df %>%  
  dplyr::mutate(periodo = glue::glue("{ano}-{mes}"),
                periodo = lubridate::ym(periodo))

## Calcula a contribuição de cada categoria para a inflação mensal
## e mantém apenas variáveis de interesse
df <- df %>% 
  dplyr::mutate(contrib = (peso_mensal/100)*variacao_mensal) %>% 
  dplyr::select(periodo, categoria, contrib)

## Calcula, para cada categoria a variação cumulativa desde jan/2020 a cada mês
df <- df %>% 
  dplyr::group_by(categoria) %>% 
  dplyr::mutate(cumulativo = cumsum(contrib),
                cumulativo = round(cumulativo, 2)) %>% 
  dplyr::ungroup()

## Define cores das categorias. Diminui intensidade
## das cores de categorias menos relevantes
paleta <- df %>% 
  dplyr::filter(periodo == "2021-12-01") %>% 
  dplyr::arrange(desc(cumulativo)) %>% 
  dplyr::mutate(cor = c("#00EB44","#107BFF","#EBB900",
                        "#EB0100","#B5EB00","#01E1F0",
                        "#9D00EB","#EB5F00","#CFEB73"),
                amount = c(rep(0,3),rep(0.95,6))) %>% 
  dplyr::mutate(cor = colorspace::desaturate(cor, amount = amount)) %>% 
  dplyr::select(categoria, cor)
df <- df %>% dplyr::left_join(paleta)

## Investiga quais produtos/serviços mais afetaram a inflação
## das três categorias que mais expandiram
detalhes <- c(1,2,5) %>% purrr::map(
  function(i) {
    
    cat = glue::glue("^{i}.[:graph:]+")
    
    rawdata %>% 
      dplyr::filter(stringr::str_detect(id_categoria_bd, cat)) %>% 
      dplyr::filter(stringr::str_length(id_categoria) == 7) %>% 
      dplyr::mutate(contrib = (peso_mensal/100)*variacao_mensal) %>% 
      dplyr::group_by(categoria) %>% 
      dplyr::mutate(cumulativo = cumsum(contrib),
                    cumulativo = round(cumulativo, 2)) %>% 
      dplyr::ungroup() %>% 
      dplyr::filter(ano == 2021, mes == 12) %>% 
      dplyr::select(id_categoria_bd, categoria, cumulativo) %>% 
      dplyr::arrange(desc(cumulativo)) %>% 
      dplyr::filter(cumulativo > 0) %>% 
      dplyr::mutate(pct_c = round(100*cumsum(cumulativo)/sum(cumulativo)),
                    pct_i = round(100*cumulativo/sum(cumulativo)))
    
  }
)

## Define título e subtítulo
titulo <- "UM TURBILHÃO INFLACIONÁRIO"
subtitulo <- c(
  'A pandemia da COVID-19 e a invasão à Ucrânia têm afetado duramente as grandes
  economias, a conjuntura é ainda pior no Brasil dada a instabilidade política e
  econômica que perdura no país. Um dos efeitos práticos dessa "terra arrasada" é a
  avassaladora inflação que tem varrido o poder de compra dos trabalhadores.<br><br>
  O gráfico abaixo expõe o aumento dos preços de produtos e serviços que compõem
  o Índice Nacional de Preços ao Consumidor Amplo (IPCA). Cada "corrente"
  representa a variação percentual de preços acumulada para uma dada categoria.
  As categorias que mais contribuíram para o crescimento da inflação de jan/2020 até jun/2022 foram
  <span style="color:#00EB44;">**Alimentação e bebidas**</span>,
  <span style="color:#107BFF;">**Transportes**</span> e
  <span style="color:#EBB900;">**Habitação**</span>.<br><br>
  <span style="font-size:10px;">Dados coletados pelo IBGE | Obtidos através da Base dos Dados (@basedosdados) e SIDRA (sidra.ibge.gov.br) | Gráfico por: Ícaro Bernardes (@IcaroBSC)</span>'
)

## Define textos explicativos
explica <- tibble(
  x = as.Date(c("2021-03-01", "2021-05-01", "2020-10-01")),
  y = c(-5,6,4),
  hjust = c(1,0,0),
  label = c(
    'Quase 80% da inflação associada a
    <span style="color:#EBB900;">**Habitação**</span>
    vem da **Energia elétrica residencial** (~48%), **Gás de
    botijão** (~18%) e **Aluguel residencial** (~13%).',
    
    'Pouco mais de 80% da inflação associada a
    <span style="color:#107BFF;">**Transportes**</span>
    vem da **Gasolina** (~53%), **Automóvel novo** (~13%),
    **Etanol** (~9%) e **Automóvel usado** (~8%).',
    
    'Pouco mais da metade da inflação associada a
    <span style="color:#00EB44;">**Alimentação e bebidas**</span>
    vem da **Refeição** fora de casa (~8%), **Lanche** fora de
    casa (~6%), **Frango em pedaços** (~6%), **Arroz** (~5%),
    **Óleo de soja** (~4%), **Café moído** (~4%), **Tomate** (~3%),
    **Leite longa vida** (~3%), **Frango inteiro** (~3%),
    **Costela** (~3%), **Queijo** (~3%) e **Contrafilé** (~2%).'
  )
)

## Define as linhas que apontam para os textos explicativos
linhas <- tibble(
  x = as.Date(c("2020-12-05", "2021-03-10",
                "2021-02-05", "2021-05-10",
                "2020-03-28", "2020-10-10")),
  xend = as.Date(c("2021-03-10", "2021-03-10",
                   "2021-05-10", "2021-05-10",
                   "2020-10-10", "2020-10-10")),
  y = c(-4.7, -4.7, 5.7, 5.7, 3.7, 3.7),
  yend = c(-4.7, -3, 5.7, 0, 3.7, 0.5),
  linetype = rep(c("solid", "dotted"), 3)
)

## Define os pontos que indicam as categorias
pontos <- tibble(
  x = as.Date(c("2021-03-10", "2021-05-10", "2020-10-10")),
  y = c(-3, 0, 0.5)
)

## Define as marcas de crescimento da inflação total acumulada
marcas <- tibble(
  x = as.Date(c("2021-02-01","2021-08-01","2022-01-01")),
  y = c(3, 5.2, 7.5),
  label = paste0("~",c(5,10,15),"%")
)

# 2. Produção do gráfico
## Gera o gráfico
p <- df %>% 
  ggplot() + 
  
  ### Insere as correntes
  ggstream::geom_stream(aes(x = periodo, y = cumulativo, fill = I(cor), color = I(cor)),
                        true_range = "both", bw = 0.8) +
  
  ### Insere os textos explicativos
  ggtext::geom_textbox(aes(x = x, y = y, label = label, hjust = hjust, halign = hjust),
                       box.padding = unit(c(0, 0, 0, 0), "pt"), vjust = 1, valign = 1,
                       width = unit(0.24, "npc"), lineheight = lineheight,
                       fill = NA, box.colour = NA, color = "white",
                       size = 4.3, family = fonte_corpo, data = explica) +
  
  ### Insere as linhas que apontam para os textos explicativos
  ggforce::geom_link0(aes(x = x, y = y, xend = xend, yend = yend, linetype = I(linetype)),
                      color = "white", size = 1.2, lineend = "round", data = linhas) +
  
  ### Insere os pontos que indicam as categorias
  geom_point(aes(x = x, y = y), size = 3, color = "white", data = pontos) +
  
  ### Insere as linhas que marcam crescimento da inflação total acumulada
  ggforce::geom_link0(aes(x = x, y = y, xend = x, yend = -y), linetype = "dashed",
                      size = 1.2, lineend = "round", data = marcas) +
  
  ### Insere os valores que marcam crescimento da inflação total acumulada 
  geom_text(aes(x = x, y = 0, label = label), size = 8, family = fonte_titulo, 
            vjust = 1, nudge_x = 30, data = marcas) +
  
  ### Mantém apenas anos no eixo X
  scale_x_date(date_labels = "%Y",
               breaks = as.Date(c("2020-01-01","2021-01-01","2022-01-01"))) +
  
  ### Inverte os eixos
  coord_flip() +
  
  ### Insere título e subtítulo
  labs(title = titulo, subtitle = subtitulo) +
  
  ### Customiza e elimina elementos gráficos 
  theme_void() +
  theme(
    text = element_text(color = "white", lineheight = lineheight, family = fonte_corpo),
    
    plot.background = element_rect(fill = "black", color = NA),
    plot.margin = margin(40, 80, 0, 40),
    plot.title = element_text(family = fonte_titulo, size = 52, hjust = 0.5),
    plot.subtitle = ggtext::element_textbox(
      size = 11.3, hjust = 0.5,
      width = grid::unit(0.9, "npc"),
      margin = margin(20, 0, 0, 0)
    ),
    
    axis.text.y = element_text(size = 20)
  )

## Salva o gráfico
ggsave("inflacao/inflacao.png", plot = p, dpi = 320,
       width = 13, height = 16, device = ragg::agg_png, res = 320)

