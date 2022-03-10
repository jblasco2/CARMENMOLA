###############################
#Análisis de letras de canciones: Análisis predictivo usando Machine Learning con R
# https://www.datacamp.com/community/tutorials/R-nlp-machine-learning
# 
#https://rstudio-pubs-static.s3.amazonaws.com/583851_e8f2489f7d75407bac5dae68318ceabe.html

####
# ESPECIALMENTE PREPARDO PARA
# ANALIZAR LA TRAYECTORIA POÉTICA DE UN POETA O CANTANTE
# CON LAS SIGUIENTES OPERACIONES
# 0. Preparación de la tabla de datos oportuna (he 
# cambiado 'decada' por 'nombre' HABRÁ QUE TENER MUY ENCUENTA LAS COLUMNAS
# DE LA TABLA CUANDO SE UTILICE EL SCCRIPT)
# 00. Guardar esa tabla en disco duro para otras operaciones.
# 1- Polaridad, y su reflejo en los textos de más éxito
# 2. Gráfica del éxito por años
# 2. Léxico (motivos y sentimiento) en los textos de más éxito.
# 4. Estadísticas léxico
# 5. Número de palabras por poema y su reflejo en el éxito
# 6. Palabras más representativas según el grado de éxito
# 7. Palabras atemporales
# 8. Diversidad y densidad léxica y relación con el éxito
# 9. TF-IDF según los años y según el éxito
# 10. Análisis sentimiento
##############################

# Cargamos librerías
library(tidyverse) #tidyr, #dplyr, #magrittr, #ggplot2
library(tidytext) #unnesting text into single words
library(mlr) #machine learning framework for R
library(tidyr) #Spread, separate, unite, text mining (also included in the tidyverse package)
library(widyr) #Use for pairwise correlation
library(dplyr) #data manipulation
#most of the libraries needed GRÁFICOS
library(circlize) #cool circle plots
library(jpeg) #read in jpg files for the circle plots
library(gridExtra) #viewing multiple plots together
library(wordcloud2) #creative visualizations
library(ggplot2) #Visualizations (also included in the tidyverse package)
library(ggrepel) #`geom_label_repel`
library(knitr) #Create nicely formatted output tables
library(kableExtra) #Create nicely formatted output tables
library(formattable) #For the color_tile function
library(memery) #Memes - images with plots
library(magick) #Memes - images with plots (image_read)
library(yarrr)  #Pirate plot
library(radarchart) #Visualizations
library(igraph) #ngram network diagrams
library(ggraph) #ngram network diagrams

# Establecer directorio
setwd("~/Desktop/Carmen_mola/corpus")

# Carga la lista de palabras vacías
vacias <- read_csv("~/Desktop/diccionarios/vacias.txt",
                   locale = default_locale())

# Los textos los lees desde un repositorio externo
direccion <- ("~/Desktop/Carmen_mola/corpus/")
# Localiza los textos
titulos <- c("Cartas a Palacio",
             "Tengo en mí todos los sueños",
             "La Malahierba",
             "Monteperdido",
             "El caso de las japonesas muertas",
             "El final del hombre",
             "La Nena",
             "La novia gitana",
             "La red púrpura")
             
ficheros <- c("DIAZ_Cartas.txt",
              "DIAZ_Tengo_todos_sueños.txt",
              "Martinez_LaMalaHierba.txt",
              "Martinez_MontePerdido.txt",
              "Mercero_CasoJaponesasMuertas.txt",
              "Mercero_ElFinalDelHombre.txt",
              "MOLA_LaNena.txt",
              "MOLA_LaNoviaGitana.txt",
              "MOLA_LaRedPurpura.txt")
autores <- c("DIAZ",
             "DIAZ",
             "MARTINEZ",
             "MARTINEZ",
             "MERCERO",
             "MERCERO",
             "MOLA",
             "MOLA",
             "MOLA")
año <- c("2014",
         "2016",
         "2017",
         "2017",
         "2017",
         "2018",
         "2020", 
         "2018",
         "2019")

genero <- c("novela bélica",
            "romance histórico",
            "suspense",
            "suspense",
            "misterio",
            "suspense",
            "suspense",
            "suspense",
            "suspense")

nivel <- c("NA",
           "NA",
           "2",
           "1",
           "NA",
           "NA",
           "2",
           "1",
           "1")

#Lo siguiente es crear la tabla novelas en la que se guardarán los cuatro textos divididos en autores:
novelas <- tibble(autor = character(),
                  texto = character(),
                  titulo = character(),
                  pagina = numeric(),
                  año = character(),
                  genero = character(),
                  nivel = character())


#Lo que sigue es el proceso de carga de los  textos y su subdivisión en autores
for (j in 1:length(ficheros)){
  texto.entrada <- read_lines(paste(direccion,
                                    ficheros[j],
                                    sep = ""),
                              locale = default_locale())
  texto.todo <- paste(texto.entrada, collapse = " ")
  por.palabras <- strsplit(texto.todo, " ")
  texto.palabras <- por.palabras[[1]]
  trozos <- split(texto.palabras,
                  ceiling(seq_along(texto.palabras)/375))
  for (i in 1:length(trozos)){
    fragmento <- trozos[i]
    fragmento.unido <- tibble(texto = paste(unlist(fragmento),
                                            collapse = " "),
                              titulo = titulos[j],
                              autor = autores [j],
                              pagina = i,
                              año = año [j],
                              genero = genero [j],
                              nivel = nivel [j])
    novelas <- bind_rows(novelas, fragmento.unido)
  }
}
texto.palabras
length(ficheros)


# Vemos las columnas de los datos
names(novelas)

# Quedarmos sólo con las variables que nos interesan
novelas <- novelas %>% 
  select(autores = autor, texto, titulo, pagina, año, genero, nivel)

# el texto ahora lo tenenmos dividido en autores y podemos ver la que queramos
glimpse(novelas[21, ])

#Comprobamos las dimensiones: filas y columnas
dim(novelas)

# Vemos cómo están estructuradas las letras de las canciones: por ejemplo
# la canción 139
str(novelas[139, ]$autores, nchar.max = 300)

# Limpiamos el texto cambiando formas anómalas por formas normales
fix.contractions <- function(doc) {
  # "won't" is a special case as it does not expand to "wo not"
  doc <- gsub("won't", "will not", doc)
  doc <- gsub("can't", "can not", doc)
  doc <- gsub("n't", " not", doc)
  doc <- gsub("'ll", " will", doc)
  doc <- gsub("'re", " are", doc)
  doc <- gsub("'ve", " have", doc)
  doc <- gsub("'m", " am", doc)
  doc <- gsub("'d", " would", doc)
  # 's could be 'is' or could be possessive: it has no expansion
  doc <- gsub("'s", "", doc)
  return(doc)
}

#Comprobamos
novelas$autores <- sapply(novelas$autores, fix.contractions)

# function to remove special characters
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)
# remove special characters
novelas$autores <- sapply(novelas$autores, removeSpecialChars)

# convert everything to lower case
novelas$autores <- sapply(novelas$autores, tolower)

# Examinar
str(novelas[139, ]$autores, nchar.max = 300)

# Visión general de los datos estadísticos
summary(novelas)

# Añadir la columna de "nombre" para agrupar canciones por autor
#create la  columna 'autor'
novelas <- novelas %>% 
  mutate(nombre = 
           ifelse(novelas$autores %in% "diaz", "DÍAZ",
                  ifelse(novelas$autores %in% "martinez", "MARTINEZ", 
                         ifelse(novelas$autores %in% "mercero", "MERCERO", 
                                ifelse(novelas$autores %in% "mola", "C_MOLA",
                                       "NA")))))

# Creamos una columna nueva con el rango de los datos
#create the chart level column (nivel 1: calificación Top 10 con valoraciones google por encima del 8; nivel 2: Top 100 por encima del 6)
novelas <- novelas %>%
  mutate(ranking = 
           ifelse(novelas$nivel %in% 1, "Top 10", 
                  ifelse(novelas$nivel %in% 2, "Top 100", "Uncharted")))


# Ver que camciones entraron en las listas de más ventas
#create binary field called charted showing if a song hit the charts at all
novelas <- novelas %>%
  mutate(charted = 
           ifelse(novelas$nivel %in% 1:2, "Charted", "Uncharted"))

#save the new dataset to .csv for use in later tutorials
write.csv(novelas, file = "novelas_new.csv")

# #define some colors to use throughout
my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00")

theme_autores <- function() 
{
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")
}

# Agrupamos  y vemos canciones por grupos
novelas %>%
  filter(nombre != "NA") %>%
  group_by(nombre, charted) %>%
  summarise(number_of_songs = n()) %>%
  ggplot() + 
  geom_bar(aes(x = nombre, y = number_of_songs, 
               fill = charted), stat = "identity")  +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        panel.grid.minor = element_blank()) +
  ggtitle("Released Songs") +
  labs(x = NULL, y = "Song Count")

# Graficamos por ranking
charted_pagina <- novelas %>%
  filter(nivel > 0) %>%
  group_by(nombre, nivel) %>%
  summarise(páginas = n())

charted_pagina %>% 
  ggplot() + 
  geom_bar(aes(x = nombre, y = páginas, 
               fill = nivel), stat = "identity") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = NULL, y = "Páginas") +
  ggtitle("Número de páginas en el ranking")

# para el análisis de letras, puede eliminar las referencias 
# al nivel de la lista y al año de lanzamiento
#look at the full data set at your disposal
novelas %>%
  group_by(nombre, nivel) %>%
  summarise(páginas = n()) %>%
  ggplot() +
  geom_bar(aes(x = nombre, y = páginas, 
               fill = nivel), stat = "identity")  +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = NULL, y = "Páginas") +
  ggtitle("Todas las páginas indexadas")

# Canciones que alcanzaron el número 1
library(knitr) # for dynamic reporting
library(kableExtra) # create a nicely formated HTML table
library(formattable) # for the color_tile function
novelas %>%
  filter(ranking == "Top 10") %>%
  select(año, pagina, ranking) %>%
  arrange(año) %>%
  mutate(año = color_tile("lightblue", "lightgreen")(año)) %>%
  mutate(ranking = color_tile("lightgreen", "lightgreen")(ranking)) %>%
  kable("html", escape = FALSE, align = "c", caption = "novelas's No. 1 Songs") %>%
  kable_styling(bootstrap_options = 
                  c("striped", "condensed", "bordered"), 
                full_width = FALSE)


# Creación de un corpus con las letras
# Eliminamos palabras que no deseamos
undesirable_words <- c("cervantes", "salamanca", "pancracio", 
                       "cristina", "señor", "señora", "pues", "merced","miguel")

# mostrar una lista aleatoria de estas palabras vacías y 
# head()para limitar a 3 palabras.
head(sample(stop_words$word, 3), 3)

# Filtramos palabras vacías e indeseadas
#unnest and remove stop, undesirable and short words
novelas_words_filtered <- novelas %>%
  unnest_tokens(word, texto) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(!word %in% undesirable_words) %>%
  filter(nchar(word) > 3)

# examinar la clase y las dimensiones de su nueva estructura de datos ordenada:
class(novelas_words_filtered)
dim(novelas_words_filtered)

#la estructura de datos ordenados, no resumidos y tokenizados.
novelas_words_filtered %>% 
  filter(word == "casa") %>%
  select(word, titulo, año, ranking, nombre, nivel, charted) %>%
  arrange() %>%
  top_n(10,titulo) %>%
  mutate(song = color_tile("lightblue","lightblue")(titulo)) %>%
  mutate(word = color_tile("lightgreen","lightgreen")(word)) %>%
  kable("html", escape = FALSE, align = "c", caption = "Tokenized Format Example") %>%
  kable_styling(bootstrap_options = 
                  c("striped", "condensed", "bordered"), 
                full_width = FALSE)


# ¿Existe una correlación entre la frecuencia de las palabras y las canciones exitosas?
# 1. Canciones con el mayor número de palabras
full_word_count <- novelas %>%
  unnest_tokens(word, autores) %>%
  group_by(titulo, nivel) %>%
  summarise(num_words = n()) %>%
  arrange(desc(num_words)) 

full_word_count[1:10,] %>%
  ungroup(num_words, titulo) %>%
  mutate(num_words = color_bar("lightblue")(num_words)) %>%
  mutate(titulo = color_tile("lightpink","lightpink")(titulo)) %>%
  kable("html", escape = FALSE, align = "c", caption = "Songs With Highest Word Count") %>%
  kable_styling(bootstrap_options = 
                  c("striped", "condensed", "bordered"), 
                full_width = FALSE)


# 2. Relación número de palabras y éxito
full_word_count %>%
  ggplot() +
  geom_histogram(aes(x = num_words, fill = nivel )) +
  ylab("Song Count") + 
  xlab("Word Count per Song") +
  ggtitle("Word Count Distribution") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        panel.grid.minor.y = element_blank())

# por curiosidad, eche un vistazo a esa canción Top 10 con más de 800 palabras.
full_word_count %>%
  filter(nivel == '1' & num_words > 10) %>%
  left_join(novelas, by = "titulo") %>%
  select(Título = titulo, 
         "Word Count" = num_words, 
         "Peak Position" = ranking) %>%
  kable("html", escape = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "condensed", "bordered"))


# palabras más utilizadas en el conjunto completo de letras
novelas_words_filtered %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot() +
  geom_col(aes(word, n), fill = my_colors[4]) +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank()) +
  xlab("") + 
  ylab("Song Count") +
  ggtitle("Most Frequently Used Words in novelas autores") +
  coord_flip()

# Nube de palabras
novelas_words_counts <- novelas_words_filtered %>%
  count(word, sort = TRUE) 

wordcloud2(novelas_words_counts[1:300, ], size = .5)

# palabras que prevalecen más en las canciones que llegaron 
# a las listas de éxitos que en las canciones desconocidas? Estas son consideradas palabras populares por la sociedad.
popular_words <- novelas_words_filtered %>% 
  group_by(nivel) %>%
  count(word, nivel, sort = TRUE) %>%
  slice(seq_len(8)) %>%
  ungroup() %>%
  arrange(nivel,n) %>%
  mutate(row = row_number()) 

popular_words %>%
  ggplot(aes(row, n, fill = nivel)) +
  geom_col(show.legend = NULL) +
  labs(x = NULL, y = "Song Count") +
  ggtitle("Popular Words by Chart Level") + 
  theme_autores() +  
  facet_wrap(~nivel, scales = "free") +
  scale_x_continuous(  # This handles replacement of row 
    breaks = popular_words$row, # notice need to reuse data frame
    labels = popular_words$word) +
  coord_flip()

# Algunas palabras en la música se consideran atemporales
timeless_words <- novelas_words_filtered %>% 
  filter(nombre != 'NA') %>%
  group_by(nombre) %>%
  count(word, nombre, sort = TRUE) %>%
  slice(seq_len(8)) %>%
  ungroup() %>%
  arrange(nombre,n) %>%
  mutate(row = row_number()) 

timeless_words %>%
  ggplot(aes(row, n, fill = nombre)) +
  geom_col(show.legend = NULL) +
  labs(x = NULL, y = "Song Count") +
  ggtitle("Timeless Words") + 
  theme_autores() +  
  facet_wrap(~nombre, scales = "free", ncol = 5) +
  scale_x_continuous(  # This handles replacement of row 
    breaks = timeless_words$row, # notice need to reuse data frame
    labels = timeless_words$word) +
  coord_flip()

#La longitud de la palabra es un tema interesante para los compositores
# Cuanto más larga sea la palabra, más difícil será rimar y encajar en un patrón.
#unnest and remove undesirable words, but leave in stop and short words
novelas_word_lengths <- novelas %>%
  unnest_tokens(word, texto) %>%
  group_by(titulo, nombre) %>%
  distinct() %>%
  filter(!word %in% undesirable_words) %>%
  mutate(word_length = nchar(word)) 

novelas_word_lengths %>%
  count(word_length, sort = TRUE) %>%
  ggplot(aes(word_length), 
         binwidth = 10) + 
  geom_histogram(aes(fill = ..count..),
                 breaks = seq(1,25, by = 2), 
                 show.legend = FALSE) + 
  xlab("Word Length") + 
  ylab("Word Count") +
  ggtitle("Word Length Distribution") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank())

# Veamos cuáles son esas palabras anormalmente largas
wc <- novelas_word_lengths %>%
  ungroup() %>%
  select(word, word_length) %>%
  distinct() %>%
  arrange(desc(word_length))

wordcloud2(wc[1:100, ], 
           size = .15,
           minSize = .0005,
           ellipticity = .3, 
           rotateRatio = 1, 
           fontWeight = "bold")

# diversidad léxica: palabras únicas por canción a lo largo de los años
lex_diversity_per_year <- novelas %>%
  filter(nombre != "NA") %>%
  unnest_tokens(word, texto) %>%
  group_by(titulo, año) %>%
  summarise(lex_diversity = n_distinct(word)) %>%
  arrange(desc(lex_diversity)) 

diversity_plot <- lex_diversity_per_year %>%
  ggplot(aes(año, lex_diversity)) +
  geom_point(color = my_colors[3],
             alpha = .4, 
             size = 4, 
             position = "jitter") + 
  stat_smooth(color = "black", se = FALSE, method = "lm") +
  geom_smooth(aes(x = año, y = lex_diversity), se = FALSE,
              color = "blue", lwd = 2) +
  ggtitle("Lexical Diversity") +
  xlab("") + 
  ylab("") +
  scale_color_manual(values = my_colors) +
  theme_classic() + 
  theme_autores()

diversity_plot

# Densidad léxica: a mayor densidad léxica (RTT), menor repetición de palabras
lex_density_per_year <- novelas %>%
  filter(nombre != "NA") %>%
  unnest_tokens(word, texto) %>%
  group_by(titulo,año) %>%
  summarise(lex_density = n_distinct(word)/n()) %>%
  arrange(desc(lex_density))

density_plot <- lex_density_per_year %>%
  ggplot(aes(año, lex_density)) + 
  geom_point(color = my_colors[4],
             alpha = .4, 
             size = 4, 
             position = "jitter") + 
  stat_smooth(color = "black", 
              se = FALSE, 
              method = "lm") +
  geom_smooth(aes(x = año, y = lex_density), 
              se = FALSE,
              color = "blue", 
              lwd = 2) +
  ggtitle("Lexical Density") + 
  xlab("") + 
  ylab("") +
  scale_color_manual(values = my_colors) +
  theme_classic() + 
  theme_autores()

density_plot

#  Miramos el éxito de ventas y lo comparamos con la diversidad y densdad léxica
chart_history <- novelas %>%
  filter(ranking > 0) %>%
  group_by(año, nivel) %>%
  summarise(number_of_songs = n()) %>%
  ggplot(aes(año, number_of_songs)) + 
  geom_point(color = my_colors[5],
             alpha = .4, 
             size = 4, 
             position = "jitter") +
  geom_smooth(aes(x = año, y = number_of_songs), 
              se = FALSE, 
              method = "lm", 
              color = "black" ) +
  geom_smooth(aes(x = año, y = number_of_songs), 
              se = FALSE,
              color = "blue", 
              lwd = 2) +
  ggtitle("Chart History") +
  xlab("") + 
  ylab("") +
  scale_color_manual(values = my_colors) +
  theme_classic() + 
  theme_autores()

grid.arrange(diversity_plot, density_plot, chart_history, ncol = 3)


#TF-FDI: importancia de las palabras en un documento 
# concreto con respecto a la totalidad
# (TF): número de veces que aparece un término en un documento
# (DF): Número de documentos que contienen cada palabra
# (IDF) = 1/DF
#TF-IDF = TF * IDF

popular_tfidf_words <- novelas %>%
  unnest_tokens(word, texto) %>%
  distinct() %>%
  filter(!word %in% undesirable_words) %>%
  filter(nchar(word) > 3) %>%
  count(nombre, word, sort = TRUE) %>%
  ungroup() %>%
  bind_tf_idf(word, nombre, n)

head(popular_tfidf_words)

# Palabras importantes según las listas de éxitos
top_popular_tfidf_words <- popular_tfidf_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(nombre) %>% 
  slice(seq_len(8)) %>%
  ungroup() %>%
  arrange(nombre, tf_idf) %>%
  mutate(row = row_number())

top_popular_tfidf_words %>%
  ggplot(aes(x = row, tf_idf, 
             fill = nombre)) +
  geom_col(show.legend = NULL) +
  labs(x = NULL, y = "TF-IDF") + 
  ggtitle("Important Words using TF-IDF by Chart Level") +
  theme_autores() +  
  facet_wrap(~nombre, ncol = 3, scales = "free") +
  scale_x_continuous(  # This handles replacement of row 
    breaks = top_popular_tfidf_words$row, # notice need to reuse data frame
    labels = top_popular_tfidf_words$word) +
  coord_flip()
###########################
# NO LO USO AHORA PORQUE AL SE  MUY POCOS TEXTOS NO PUEDO AGRUPAR POR DÉCADAS
# TF-IDF a lo largo del tiempo :
tfidf_words_decade <- prince %>%
  unnest_tokens(word, lyrics) %>%
  distinct() %>%
  filter(!word %in% undesirable_words & decade != 'NA') %>%
  filter(nchar(word) > 3) %>%
  count(decade, word, sort = TRUE) %>%
  ungroup() %>%
  bind_tf_idf(word, decade, n) %>%
  arrange(desc(tf_idf))

top_tfidf_words_decade <- tfidf_words_decade %>% 
  group_by(decade) %>% 
  slice(seq_len(8)) %>%
  ungroup() %>%
  arrange(decade, tf_idf) %>%
  mutate(row = row_number())

top_tfidf_words_decade %>%
  ggplot(aes(x = row, tf_idf, fill = decade)) +
  geom_col(show.legend = NULL) +
  labs(x = NULL, y = "TF-IDF") + 
  ggtitle("Important Words using TF-IDF by Decade") +
  theme_lyrics() +  
  facet_wrap(~decade, 
             ncol = 3, nrow = 2, 
             scales = "free") +
  scale_x_continuous(  # this handles replacement of row 
    breaks = top_tfidf_words_decade$row, # notice need to reuse data frame
    labels = top_tfidf_words_decade$word) +
  coord_flip()

# Una nube de palabras rápida con este método tfidf
wc <- tfidf_words_decade %>%
  arrange(desc(tf_idf)) %>%
  select(word, tf_idf)

wordcloud2(wc[1:300, ], 
           color = "random-dark", 
           minRotation = -pi / 6, 
           maxRotation = -pi / 3, 
           minSize = .002, 
           ellipticity = .3, 
           rotateRatio = 1, 
           size = .2, 
           fontWeight = "bold", 
           gridSize = 1.5 )

##############################

#Define some colors to use throughout
my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00", "#D65E00")

#Customize ggplot2's default theme settings
#This tutorial doesn't actually pass any parameters, but you may use it again in future tutorials so it's nice to have the options
theme_autores <- function(aticks = element_blank(),
                         pgminor = element_blank(),
                         lt = element_blank(),
                         lp = "none")
{
  theme(plot.title = element_text(hjust = 0.5), #Center the title
        axis.ticks = aticks, #Set axis ticks to on or off
        panel.grid.minor = pgminor, #Turn the minor grid lines on or off
        legend.title = lt, #Turn the legend title on or off
        legend.position = lp) #Turn the legend on or off
}

#Customize the text tables for consistency using HTML formatting
my_kable_styling <- function(dat, caption) {
  kable(dat, "html", escape = FALSE, caption = caption) %>%
    kable_styling(bootstrap_options = c("striped", "condensed", "bordered"),
                  full_width = FALSE)
}





# Leer los datos
novelas_data <- read.csv("~/Desktop/Carmen_mola/data/novelas_new.csv", stringsAsFactors = FALSE, row.names = 1)

# Vistazo a datos
glimpse(novelas_data) #Transposed version of `print()`

# Procedemos a la limpieza
# Carga la lista de palabras vacías
vacias <- read_csv("~/Desktop/diccionarios/vacias.txt",
                   locale = default_locale())
#Created in the first tutorial
undesirable_words <- read_csv("~/Desktop/diccionarios/vacias.txt",
                              locale = default_locale())

#Create tidy text format: Unnested, Unsummarized, -Undesirables, Stop and Short words
novelas_tidy <- novelas_data %>%
  unnest_tokens(word, autores) %>% #Break the autores into individual words
  filter(!word %in% undesirable_words) %>% #Remove undesirables
  filter(!nchar(word) < 3) %>% #Words like "ah" or "oo" used in music
  anti_join(stop_words) #Data provided by the tidytext package

# recuento de palabras distintas por canción, restableciendo la columna de 
# decada
word_summary <- novelas_tidy %>%
  mutate(decade = ifelse(is.na(decade),"NONE", decade)) %>%
  group_by(decade, song) %>%
  mutate(word_count = n_distinct(word)) %>%
  select(song, Released = decade, Charted = charted, word_count) %>%
  distinct() %>% #To obtain one record per song
  ungroup()

pirateplot(formula =  word_count ~ Released + Charted, #Formula
           data = word_summary, #Data frame
           xlab = NULL, ylab = "Song Distinct Word Count", #Axis labels
           main = "Lexical Diversity Per Decade", #Plot title
           pal = "google", #Color scheme
           point.o = .2, #Points
           avg.line.o = 1, #Turn on the Average/Mean line
           theme = 0, #Theme
           point.pch = 16, #Point `pch` type
           point.cex = 1.5, #Point size
           jitter.val = .1, #Turn on jitter to see the songs better
           cex.lab = .9, cex.names = .7) #Axis label size

# muestra el número relativo de canciones por año.
songs_year <- novelas_data %>%
  select(song, year) %>%
  group_by(year) %>%
  summarise(song_count = n())

id <- seq_len(nrow(songs_year))
songs_year <- cbind(songs_year, id)
label_data = songs_year
number_of_bar = nrow(label_data) #Calculate the ANGLE of the labels
angle = 90 - 360 * (label_data$id - 0.5) / number_of_bar #Center things
label_data$hjust <- ifelse(angle < -90, 1, 0) #Align label
label_data$angle <- ifelse(angle < -90, angle + 180, angle) #Flip angle
ggplot(songs_year, aes(x = as.factor(id), y = song_count)) +
  geom_bar(stat = "identity", fill = alpha("purple", 0.7)) +
  geom_text(data = label_data, aes(x = id, y = song_count + 10, label = year, hjust = hjust), color = "black", alpha = 0.6, size = 3, angle =  label_data$angle, inherit.aes = FALSE ) +
  coord_polar(start = 0) +
  ylim(-20, 150) + #Size of the circle
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-4,4), "in"),
        plot.title = element_text(margin = margin(t = 10, b = -10)))

#década en que se lanzó una canción y si llegó o no a las listas 
decade_chart <-  novelas_data %>%
  filter(decade != "NA") %>% #Remove songs without release dates
  count(decade, charted)  #Get SONG count per chart level per decade. Order determines top or bottom.

circos.clear() #Very important - Reset the circular layout parameters!
grid.col = c("1970s" = my_colors[1], "1980s" = my_colors[2], "1990s" = my_colors[3], "2000s" = my_colors[4], "2010s" = my_colors[5], "Charted" = "grey", "Uncharted" = "grey") #assign chord colors
# Set the global parameters for the circular layout. Specifically the gap size
circos.par(gap.after = c(rep(5, length(unique(decade_chart[[1]])) - 1), 15,
                         rep(5, length(unique(decade_chart[[2]])) - 1), 15))

chordDiagram(decade_chart, grid.col = grid.col, transparency = .2)
title("Relationship Between Chart and Decade")

# Análisis sentimiento
new_sentiments <- sentiments %>% #From the tidytext package
group_by(lexicon = "NRC") %>%
  mutate(words_in_lexicon = n_distinct(word)) %>%
  ungroup()

new_sentiments %>%
  group_by(lexicon, sentiment, words_in_lexicon) %>%
  summarise(distinct_words = n_distinct(word)) %>%
  ungroup() %>%
  spread(sentiment, distinct_words) %>%
  mutate(lexicon = color_tile("lightblue", "lightblue")(lexicon),
         words_in_lexicon = color_bar("lightpink")(words_in_lexicon)) %>%
  my_kable_styling(caption = "Word Counts Per Lexicon")

novelas_tidy %>%
  mutate(words_in_autores = n_distinct(word)) %>%
  inner_join(new_sentiments) %>%
  group_by(lexicon, words_in_autores, words_in_lexicon) %>%
  summarise(lex_match_words = n_distinct(word)) %>%
  ungroup() %>%
  mutate(total_match_words = sum(lex_match_words), #Not used but good to have
         match_ratio = lex_match_words / words_in_autores) %>%
  select(lexicon, lex_match_words,  words_in_autores, match_ratio) %>%
  mutate(lex_match_words = color_bar("lightpink")(lex_match_words),
         lexicon = color_tile("lightgreen", "lightgreen")(lexicon)) %>%
  my_kable_styling(caption = "autores Found In Lexicons")

# Eche un vistazo a algunas palabras específicas de las letras de novelas que parecen tener un impacto en el sentimiento
new_sentiments %>%
  filter(word %in% c("dark", "controversy", "gangster",
                     "discouraged", "race")) %>%
  arrange(word) %>% #sort
  mutate(word = color_tile("lightblue", "lightblue")(word),
         words_in_lexicon = color_bar("lightpink")(words_in_lexicon),
         lexicon = color_tile("lightgreen", "lightgreen")(lexicon)) %>%
  my_kable_styling(caption = "Specific Words")

# Observar presencias de una determinada palabra, por ejemplo "sex"
my_word_list <- novelas_data %>%
  unnest_tokens(word, autores) %>%
  filter(grepl("sex", word)) %>% #Use `grepl()` to find the substring `"sex"`
  count(word) %>%
  select(myword = word, n) %>% #Rename word
  arrange(desc(n))

new_sentiments %>%
  #Right join gets all words in `my_word_list` to show nulls
  right_join(my_word_list, by = c("word" = "myword")) %>%
  filter(word %in% my_word_list$myword) %>%
  mutate(word = color_tile("lightblue", "lightblue")(word),
         instances = color_tile("lightpink", "lightpink")(n),
         lexicon = color_tile("lightgreen", "lightgreen")(lexicon)) %>%
  my_kable_styling(caption = "Dependency on Word Form")

# creando conjuntos de datos de opinión de novelas para cada 
# uno de los léxicos realizando una get_sentiments()función.
novelas_bing <- novelas_tidy %>%
  inner_join(get_sentiments("bing"))

novelas_nrc <- novelas_tidy %>%
  inner_join(get_sentiments("nrc"))

novelas_nrc_sub <- novelas_tidy %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(!sentiment %in% c("positive", "negative"))


# análisis de opinión de la NRC de todo el conjunto de datos
nrc_plot <- novelas_nrc %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) %>%
  #Use `fill = -word_count` to make the larger bars darker
  ggplot(aes(sentiment, word_count, fill = -word_count)) +
  geom_col() +
  guides(fill = FALSE) + #Turn off the legend
  theme_autores() +
  labs(x = NULL, y = "Word Count") +
  scale_y_continuous(limits = c(0, 15000)) + #Hard code the axis limit
  ggtitle("novelas NRC Sentiment") +
  coord_flip()
plot(nrc_plot)

# Sentimiento con bing del conjunto
bing_plot <- novelas_bing %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) %>%
  ggplot(aes(sentiment, word_count, fill = sentiment)) +
  geom_col() +
  guides(fill = FALSE) +
  theme_autores() +
  labs(x = NULL, y = "Word Count") +
  scale_y_continuous(limits = c(0, 8000)) +
  ggtitle("novelas Bing Sentiment") +
  coord_flip()
plot(bing_plot)

# Polaridad por porcentajes
novelas_polarity_chart <- novelas_bing %>%
  count(sentiment, chart_level) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(polarity = positive - negative,
         percent_positive = positive / (positive + negative) * 100)

#Polarity by chart
plot1 <- novelas_polarity_chart %>%
  ggplot( aes(chart_level, polarity, fill = chart_level)) +
  geom_col() +
  scale_fill_manual(values = my_colors[3:5]) +
  geom_hline(yintercept = 0, color = "red") +
  theme_autores() + theme(plot.title = element_text(size = 11)) +
  xlab(NULL) + ylab(NULL) +
  ggtitle("Polarity By Chart Level")

#Percent positive by chart
plot2 <- novelas_polarity_chart %>%
  ggplot( aes(chart_level, percent_positive, fill = chart_level)) +
  geom_col() +
  scale_fill_manual(values = c(my_colors[3:5])) +
  geom_hline(yintercept = 0, color = "red") +
  theme_autores() + theme(plot.title = element_text(size = 11)) +
  xlab(NULL) + ylab(NULL) +
  ggtitle("Percent Positive By Chart Level")

grid.arrange(plot1, plot2, ncol = 2)


# ver si el clima emocional cambia o no con el tiempo
novelas_polarity_year <- novelas_bing %>%
  count(sentiment, year) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(polarity = positive - negative,
         percent_positive = positive / (positive + negative) * 100)

polarity_over_time <- novelas_polarity_year %>%
  ggplot(aes(year, polarity, color = ifelse(polarity >= 0,my_colors[5],my_colors[4]))) +
  geom_col() +
  geom_smooth(method = "loess", se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, aes(color = my_colors[1])) +
  theme_autores() + theme(plot.title = element_text(size = 11)) +
  xlab(NULL) + ylab(NULL) +
  ggtitle("Polarity Over Time")

relative_polarity_over_time <- novelas_polarity_year %>%
  ggplot(aes(year, percent_positive , color = ifelse(polarity >= 0,my_colors[5],my_colors[4]))) +
  geom_col() +
  geom_smooth(method = "loess", se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, aes(color = my_colors[1])) +
  theme_autores() + theme(plot.title = element_text(size = 11)) +
  xlab(NULL) + ylab(NULL) +
  ggtitle("Percent Positive Over Time")

grid.arrange(polarity_over_time, relative_polarity_over_time, ncol = 2)

# las relaciones entre los sentimientos de la NRC y las décadas
grid.col = c("1970s" = my_colors[1], "1980s" = my_colors[2], "1990s" = my_colors[3], "2000s" = my_colors[4], "2010s" = my_colors[5], "anger" = "grey", "anticipation" = "grey", "disgust" = "grey", "fear" = "grey", "joy" = "grey", "sadness" = "grey", "surprise" = "grey", "trust" = "grey")

decade_mood <-  novelas_nrc %>%
  filter(decade != "NA" & !sentiment %in% c("positive", "negative")) %>%
  count(sentiment, decade) %>%
  group_by(decade, sentiment) %>%
  summarise(sentiment_sum = sum(n)) %>%
  ungroup()

circos.clear()
#Set the gap size
circos.par(gap.after = c(rep(5, length(unique(decade_mood[[1]])) - 1), 15,
                         rep(5, length(unique(decade_mood[[2]])) - 1), 15))
chordDiagram(decade_mood, grid.col = grid.col, transparency = .2)
title("Relationship Between Mood and Decade")


####################################################
###### CORRELACIÓN ENTRE EVENTOS POR AÑOS Y TEXTOS
####################################################
events <- read.csv('novelasEvents.csv', stringsAsFactors = FALSE)

year_polarity_bing <- novelas_bing %>%
  group_by(year, sentiment) %>%
  count(year, sentiment) %>%
  spread(sentiment, n) %>%
  mutate(polarity = positive - negative,
         ratio = polarity / (positive + negative)) #use polarity ratio in next graph

events %>%
  #Left join gets event years with no releases
  left_join(year_polarity_bing) %>%
  filter(event != " ") %>% #Account for bad data
  mutate(event = reorder(event, year), #Sort chart by desc year
         sentiment = ifelse(positive > negative,
                            "positive", "negative")) %>%
  ggplot(aes(event, polarity, fill = sentiment)) +
  geom_bar(stat = "identity") +
  theme_minimal() + theme(legend.position = "none") +
  xlab(NULL) +
  ggtitle("Sentiment by Events") +
  coord_flip()
###################################################
# crear una puntuación de polaridad por año
plot_words_94_96 <- novelas_nrc %>%
  filter(year %in% c("1994", "1995", "1996")) %>%
  group_by(sentiment) %>%
  count(word, sort = TRUE) %>%
  arrange(desc(n)) %>%
  slice(seq_len(8)) %>% #consider top_n() from dplyr also
  ungroup()

plot_words_94_96 %>%
  #Set `y = 1` to just plot one variable and use word as the label
  ggplot(aes(word, 1, label = word, fill = sentiment )) +
  #You want the words, not the points
  geom_point(color = "transparent") +
  #Make sure the labels don't overlap
  geom_label_repel(force = 1,nudge_y = .5,  
                   direction = "y",
                   box.padding = 0.04,
                   segment.color = "transparent",
                   size = 3) +
  facet_grid(~sentiment) +
  theme_autores() +
  theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
        axis.title.x = element_text(size = 6),
        panel.grid = element_blank(), panel.background = element_blank(),
        panel.border = element_rect("lightgray", fill = NA),
        strip.text.x = element_text(size = 9)) +
  xlab(NULL) + ylab(NULL) +
  ggtitle("1994 - 1996 NRC Sentiment") +
  coord_flip()

# DE UN AÑO CONCRETO
plot_words_1998 <- novelas_nrc %>%
  filter(year == "1998") %>%
  group_by(sentiment) %>%
  count(word, sort = TRUE) %>%
  arrange(desc(n)) %>%
  slice(seq_len(10)) %>%
  ungroup()

#Same comments as previous graph
plot_words_1998 %>%
  ggplot(aes(word, 1, label = word, fill = sentiment )) +
  geom_point(color = "transparent") +
  geom_label_repel(force = 1,nudge_y = .5,  
                   direction = "y",
                   box.padding = 0.05,
                   segment.color = "transparent",
                   size = 3) +
  facet_grid(~sentiment) +
  theme_autores() +
  theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
        axis.title.x = element_text(size = 6),
        panel.grid = element_blank(), panel.background = element_blank(),
        panel.border = element_rect("lightgray", fill = NA),
        strip.text.x = element_text(size = 9)) +
  xlab(NULL) + ylab(NULL) +
  ggtitle("1998 NRC Sentiment") +
  coord_flip()

# GRÁFICO ARAÑA
#Get the count of words per sentiment per year
year_sentiment_nrc <- novelas_nrc_sub %>%
  group_by(year, sentiment) %>%
  count(year, sentiment) %>%
  select(year, sentiment, sentiment_year_count = n)

#Get the total count of sentiment words per year (not distinct)
total_sentiment_year <- novelas_nrc_sub %>%
  count(year) %>%
  select(year, year_total = n)

#Join the two and create a percent field
year_radar_chart <- year_sentiment_nrc %>%
  inner_join(total_sentiment_year, by = "year") %>%
  mutate(percent = sentiment_year_count / year_total * 100 ) %>%
  filter(year %in% c("1978","1994","1995")) %>%
  select(-sentiment_year_count, -year_total) %>%
  spread(year, percent) %>%
  chartJSRadar(showToolTipLabel = TRUE,
               main = "NRC Years Radar")

# observe las palabras de cada categoría.
novelas_tidy %>%
  filter(song %in% 'sign o the times') %>%
  distinct(word) %>%
  inner_join(get_sentiments("nrc")) %>%
  ggplot(aes(x = word, fill = sentiment)) +
  facet_grid(~sentiment) +
  geom_bar() + #Create a bar for each word per sentiment
  theme_autores() +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_blank()) + #Place the words on the y-axis
  xlab(NULL) + ylab(NULL) +
  ggtitle("Sign O' The Times Sentiment Words") +
  coord_flip()

# Mire las categorías de opinión de algunas canciones más con títulos distintivos
novelas_nrc_sub %>%
  filter(song %in% c("so blue", "controversy", "raspberry beret",
                     "when doves cry", "the future", "1999")) %>%
  count(song, sentiment, year) %>%
  mutate(sentiment = reorder(sentiment, n), song = reorder(song, n)) %>%
  ggplot(aes(sentiment, n, fill = sentiment)) +
  geom_col() +
  facet_wrap(year ~ song, scales = "free_x", labeller = label_both) +
  theme_autores() +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_blank()) +
  labs(x = NULL, y = NULL) +
  ggtitle("NRC Sentiment Song Analysis") +
  coord_flip()

# BIGRAMAS POR DÉCADA
novelas_bigrams <- novelas_data %>%
  unnest_tokens(bigram, autores, token = "ngrams", n = 2)

bigrams_separated <- novelas_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% undesirable_words) %>%
  filter(!word2 %in% undesirable_words)

#Because there is so much repetition in music, also filter out the cases where the two words are the same
bigram_decade <- bigrams_filtered %>%
  filter(word1 != word2) %>%
  filter(decade != "NA") %>%
  unite(bigram, word1, word2, sep = " ") %>%
  inner_join(novelas_data) %>%
  count(bigram, decade, sort = TRUE) %>%
  group_by(decade) %>%
  slice(seq_len(7)) %>%
  ungroup() %>%
  arrange(decade, n) %>%
  mutate(row = row_number())

bigram_decade %>%
  ggplot(aes(row, n, fill = decade)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~decade, scales = "free_y") +
  xlab(NULL) + ylab(NULL) +
  scale_x_continuous(  # This handles replacement of row
    breaks = bigram_decade$row, # Notice need to reuse data frame
    labels = bigram_decade$bigram) +
  theme_autores() +
  theme(panel.grid.major.x = element_blank()) +
  ggtitle("Bigrams Per Decade") +
  coord_flip()

# SENTIMIENTOS DE LOS BIGRAMAS
# GRÁFICO DE RED
negation_words <- c("not", "no", "never", "without")

negation_bigrams <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word1, word2, sort = TRUE) %>%
  mutate(contribution = n) %>%
  arrange(desc(abs(contribution))) %>%
  group_by(word1) %>%
  slice(seq_len(20)) %>%
  arrange(word1,desc(contribution)) %>%
  ungroup()

bigram_graph <- negation_bigrams %>%
  graph_from_data_frame() #From `igraph`

set.seed(123)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(alpha = .25) +
  geom_edge_density(aes(fill = n)) +
  geom_node_point(color = "purple1", size = 1) + #Purple for novelas!
  geom_node_text(aes(label = name),  repel = TRUE) +
  theme_void() + theme(legend.position = "none",
                       plot.title = element_text(hjust = 0.5)) +
  ggtitle("Negation Bigram Network")

#correlación entre palabras: número de veces que cada par 
# de palabras aparecen juntas dentro de una canción
pwc <- novelas_tidy %>%
  filter(n() >= 20) %>%  #High counts
  pairwise_count(word, song, sort = TRUE) %>%
  filter(item1 %in% c("love", "peace", "gangster", "hate")) %>%
  group_by(item1) %>%
  slice(seq_len(7)) %>%
  ungroup() %>%
  mutate(row = -row_number()) #Descending order

pwc %>%
  ggplot(aes(row, n, fill = item1)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~item1, scales = "free") +
  scale_x_continuous(  #This handles replacement of row
    breaks = pwc$row, #Notice need to reuse data frame
    labels = pwc$item2) +
  theme_autores() + theme(panel.grid.major.x = element_blank()) +
  xlab(NULL) + ylab(NULL) +
  ggtitle("Pairwise Counts") +
  coord_flip()

# correlación por pares: frecuencia con la que las palabras aparecen juntas 
novelas_tidy %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, song, sort = TRUE) %>%
  filter(item1 %in% c("love", "peace", "gangster", "hate")) %>%
  group_by(item1) %>%
  top_n(7) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation, fill = item1)) +
  geom_bar(stat = 'identity', show.legend = FALSE) +
  facet_wrap(~item1, scales = 'free') +
  theme_autores() + theme(panel.grid.major.x = element_blank()) +
  xlab(NULL) + ylab(NULL) +
  ggtitle("Pairwise Correlation") +
  coord_flip()

