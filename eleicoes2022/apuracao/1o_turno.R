################################################################################
# 0. Controle de Pacotes e Fontes
library(dplyr)
library(tidyr)
library(zip)
library(jsonlite)
library(rlang)
library(purrr)
library(lubridate)
library(stringr)
library(glue)
library(ggplot2)
library(ggpath)
library(ggtext)
library(gganimate)
library(scales)
library(ragg)

################################################################################
# 1. Carregamento dos dados
## Obtém a lista de arquivos dentro da pasta compactada e
## seleciona apenas aqueles de interesse
arquivos <- zip::zip_list("eleicoes2022/apuracao/json_eleicoes2022.zip") %>% 
  dplyr::filter(stringr::str_detect(filename, "brazil_")) %>% 
  dplyr::pull(filename)

## Extrai os resultados de dentro dos json
dados <- arquivos %>% 
  purrr::map_dfr(function(filename) {
    ### Estabelece conexão com um dado json dentro da pasta compactada
    con = unz('eleicoes2022/apuracao/json_eleicoes2022.zip', filename)
    
    ### Efetua a leitura do json
    json = jsonlite::fromJSON(con)
    
    ### Extrai a parte dos documento que guarda os resultados
    df = json$cand
    
    ### Extrai a data e tempo do nome do arquivo
    data = filename %>% 
      stringr::str_remove_all("brazil_|.json") %>% 
      lubridate::ymd_hms()
    
    ### Inclui data e tempo no banco de dados
    df = df %>% dplyr::mutate(data = data)
    
  })

## Cria uma tibble que associa informações dos candidatos
cand_info <- tibble(
  candidato = c("LULA", "JAIR BOLSONARO"),
  partido = c("PT", "PL"),
  cor = c("#FF0A01", "#002B8F"),
) %>% 
  dplyr::mutate(path = glue("eleicoes2022/apuracao/{partido}.png"))

################################################################################
# 2. Manejo dos dados
## Mantém apenas as variáveis de interesse
dados <- dados %>% 
  dplyr::select(candidato = nm,
                votos = vap,
                pct = pvap,
                data)

## Mantém apenas os dados dos 2 primeiros
dados <- dados %>% 
  dplyr::filter(candidato %in% c("LULA","JAIR BOLSONARO"))

## Converte os valores de votos válidos e pct de caractere a númerico
dados <- dados %>% 
  dplyr::mutate(pct = str_replace(pct, ",", ".")) %>% 
  dplyr::mutate(across(.cols = c(pct, votos), .fns = as.numeric))

## Adiciona informações dos candidatos
dados <- dados %>% dplyr::left_join(cand_info)

## Gera o texto
dados <- dados %>% 
  dplyr::mutate(label = glue("<strong style='font-size:7px;'>{candidato}</strong>
                             <span style='font-size:5px;'> ({partido})</span><br>
                             <span style='font-size:5px;'>{format(votos, big.mark='.', decimal.mark=',')}</span>"))

## Separa os dados dos candidatos
dados <- dados %>% 
  dplyr::group_by(partido) %>% 
  tidyr::nest()

################################################################################
# 3. Produção do gráfico
## Inicializa o objeto que guarda o gráfico
plot <- NULL %>% ggplot()

## Adiciona a série de cada candidato em uma camada diferente de ggplot
dados$data %>% purrr::walk(
  function(dados){
    
    p = plot +
      geom_path(aes(x = data, y = pct, color = I(cor)),
                size = 1, data = dados) +
      geom_point(aes(x = data, y = pct, color = I(cor)),
                 size = 7, data = dados) +
      ggtext::geom_richtext(aes(x = data, y = pct, label = label),
                            fill = "white", label.colour = NA,
                            hjust = 0, nudge_x = 1000, size = 1.5, data = dados) +
      ggpath::geom_from_path(aes(x = data, y = pct, path = path),
                             width = 0.17, height = 0.17, data = dados)
    
    assign("plot", p, envir = .GlobalEnv)
    
  }
)

## Define limites do eixo-y, elementos do tema e da transição da animação
plot <- plot +
  scale_y_continuous(limits = c(40,50), labels = scales::label_percent(scale = 1)) +
  scale_x_datetime(expand = expansion(mult = c(0.05,0.35))) +
  theme_minimal() +
  theme(
    text = element_text(size = 8),
    legend.position = 'none',
    axis.title = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(0.03,0.03,0.03,0.03,"npc")
  ) +
  gganimate::transition_reveal(along = data) +
  gganimate::ease_aes('cubic-in-out')

## Define configurações da animação
anima <- gganimate::animate(plot, nframes = 350, fps = 60,
                            start_pause = 100, end_pause = 100,
                            device = 'ragg_png', res = 300,
                            width = 1500, height = 500)

## Salva a animação em gif
gganimate::anim_save("eleicoes2022/apuracao/1o_turno.gif", anima)
