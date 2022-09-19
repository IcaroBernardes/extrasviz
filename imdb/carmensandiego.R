# 0. Package and fonts management
library(dplyr)
library(data.table)
library(ggplot2)
library(ggpath)
library(imgpalr)
library(colorspace)

# 1. Loading and wrangling data
# You can get the data from this link: https://datasets.imdbws.com
## Loads basic info.
raw_basics <- data.table::fread("imdb/title.basics.tsv.gz", encoding = "UTF-8")

## Gets the code of the show
showcode <- raw_basics %>% 
  dplyr::filter(primaryTitle == "Carmen Sandiego",
                titleType == "tvSeries") %>% 
  dplyr::pull(tconst)

## Removes basic info. from workspace and liberates space in the memory
rm("raw_basics")
gc()

## Loads episodes info.
raw_episode <- data.table::fread("imdb/title.episode.tsv.gz")

## Gets info on the episodes
episodesinfo <- raw_episode %>% 
  dplyr::filter(parentTconst == showcode)

## Removes episodes info. from workspace and liberates space in the memory
rm("raw_episode")
gc()

## Loads ratings info.
raw_ratings <- data.table::fread("imdb/title.ratings.tsv.gz")

## Joins the info on the episodes with the ratings while filtering it
df <- episodesinfo %>% 
  dplyr::left_join(raw_ratings)

## Selects variables of interest and converts them to numeric
df <- df %>% 
  dplyr::select(seasonNumber, episodeNumber, averageRating) %>% 
  dplyr::mutate(across(.fns = as.numeric))

## Removes ratings info. from workspace and liberates space in the memory
rm("raw_ratings")
gc()

# 2. Creating color palettes
## Creates a neutral palette based on
## an image from Inspector Devineaux (ACME)
neutralpal <- imgpalr::image_pal(
  "imdb/images/acme.jpg", n = 3, type = "qual",
  bw = c(0,1), saturation = c(0,0.5),
  brightness = c(0.5,1), seed = 13, plot = TRUE
)

## Creates a divergent pallete based on
## an image from Carmen Sandiego and VILE faculty
divpal <- imgpalr::image_pal(
  "imdb/images/carmen_vile.png", n = 6, type = "div",
  bw = c(0.3,1), div_center = neutralpal[3],
  seed = 13, plot = TRUE
)

## Revert the order of the vector of colors
divpal <- rev(divpal)

## Saturates the selected colors since the original image
## doesn't have saturated greens
divpal <- colorspace::desaturate(divpal,-c(0,0.4,0.8,0.8,0.4,0))

## Adds a neutral color at the center of the palette
divpal <- append(divpal, neutralpal[3], after = 3L)

# 3. Create the graphic
## Creates a tibble for the ratings text
df_text <- df %>% dplyr::filter(averageRating %in% range(averageRating))

## Defines color of the ratings text
df_text <- df_text %>% 
  dplyr::mutate(color = ifelse(averageRating >= 9, "white", "black"))

## Makes the heatmap
df %>% 
  ggplot(aes(x = episodeNumber, y = seasonNumber)) +
  
  ### Places the tiles and text
  geom_tile(aes(fill = averageRating), color = "black", size = 2) +
  geom_text(aes(label = averageRating, color = I(color)),
            size = 10, fontface = "bold", data = df_text) +
  
  ### Applies the divergent color scale
  scale_fill_gradientn(colours = divpal, limits = c(7.1,9.1)) +
  
  ### Adds prefix to the y-axis labels
  scale_y_continuous(labels = ~paste0("S", .)) +
  
  ### Eliminates and customizes theme elements
  theme_void() +
  theme(
    axis.text.y = element_text(size = 20, color = "white"),
    legend.position = "none"
  )
  
## Saves the plot
ggsave("imdb/carmen_heatmap.png", dpi = 320, width = 10, height = 4)
