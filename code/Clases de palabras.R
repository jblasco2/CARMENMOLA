#ANÁLISIS MORFOLÓGICO#
#1 Analisis de un solo texto
#Cargamos las librerías
library(udpipe)
library(tidyverse)
library(tidytext)
udpipe_download_model(language = "spanish-gsd")
udpipe_download_model(language = "spanish-ancora")
#Caragamos el modelo udpipe.Utiliza ancora
modelo_ancora <- udpipe_load_model(file = 'spanish-ancora-ud-2.5-191206.udpipe')
#Cargamos el texto objeto de análisis
CarmenMola <- read_lines("~/Desktop/Carmen_mola/corpus/MOLA_LaNoviaGitana.txt",
                          skip = 3,
                          locale = default_locale())
#Limpiamos el texto
#Suprimimos todo tipo de guiones
CarmenMola <- gsub("[-–—]", " — ", CarmenMola)
#borrar cualquier espacio en blanco que haya antes de un punto, una coma, un punto y coma o dos puntos
CarmenMola <- gsub(" ([\\.,;:])", "\\1", CarmenMola)
#dos o más espacios en blanco juntos
CarmenMola <- gsub(" {2,10}", " ", CarmenMola)
#borrar el espacio en blanco que haya antes de las rayas inicial de diálogo –el acento circunflejo indica en el comienzo de la cadena–.
CarmenMola <- gsub("^ ", "", CarmenMola)

#Pedimos el análisis
CarmenMola_analisis <- udpipe_annotate(modelo_ancora, CarmenMola)
CarmenMola_analisis <- as_tibble(CarmenMola_analisis)

#ver las estadísticas básicas de las clases de palabra 
CarmenMola_analisis %>%
  drop_na(upos) %>%
  count(upos, sort = T) %>%
  mutate(upos = reorder(upos, n)) %>%
  ggplot(aes(upos, n)) +
  geom_col(fill = "darkgreen") +
  coord_flip()
#Averiguar cuáles son los sustantivos (NOUN) más frecuentes
CarmenMola_analisis %>%
  filter(upos == "NOUN") %>%
  count(token, sort = T) %>%
  mutate(token = reorder(token, n)) %>%
  top_n(20) %>%
  ggplot(aes(token, n)) +
  geom_col(fill = "darkgreen") +
  coord_flip()
#Si cambias NOUN por cualquier otro valor de upos 
# puedes extraer los datos referentes a cada clase de palabra.
#Pero para verbos mejor trabjar con lemas que con tokens y separando los 
#auxiliares
CarmenMola_analisis %>%
  filter(upos == "AUX" | upos == "VERB") %>%
  count(lemma, sort = T) %>%
  mutate(lemma = reorder(lemma, n)) %>%
  top_n(30) %>%
  ggplot(aes(lemma, n)) +
  geom_col(fill = "orange") +
  coord_flip()


# 2. Análisis de varios textos
#Cargamos librerías

#Cargamos archivos
ficheros <- list.files(path ="~/Desktop/ZZZZ/data5/novelas", pattern = ".txt")
ficheros
nombre <- gsub("\\.txt",
               "",
               ficheros, perl = T)
#Establecemos los autores: el número que sigue al nombre indica 
#el número de obras suyas que hay en el corpus de análisis
Espina <- rep("Concha Espina", 1)
Galdos <- rep("Benito Pérez Galdós", 1)
Pardo <- rep("Emilia Pardo Bazán", 1)
Pereda <- rep("José María Pereda", 1)
autores <- c(Espina, Galdos, Pardo, Pereda)

#Creamos tabla y cargamos los textos
novelas <- tibble(nombre = character(),
                  autores = character(),
                  parrafo = numeric(),
                  texto = character())
for (i in 1:length(ficheros)){
  novela <- readLines(paste("~/Desktop/ZZZZ/data5/novelas",
                            ficheros[i],
                            sep = "/"))
  temporal <- tibble(nombre = nombre[i],
                     autores = autores[i],
                     parrafo = seq_along(novela),
                     texto = novela)
  novelas <- bind_rows(novelas, temporal)
}

novelas$texto <- iconv(novelas$texto, from = "Latin1", to = "UTF-8")

#Creamos nuevas columnas en el proceso de análisis
NombreNovela <- nombre
AutoresNovela <- autores

#Creamos una nueva tabla
Novelas_Analizado <- tibble(parrafo_id = integer(),
                            enunciado_id = integer(),
                            enunciado = character(),
                            token_id = character(),
                            token = character(),
                            lema = character(),
                            upos = character(),
                            xpos = character(),
                            rasgos = character(),
                            nombre = character(),
                            autores = character())
for(i in 1:length(NombreNovela)){
  temporal <- novelas %>%
    filter(nombre == NombreNovela[i]) %>%
    select(texto)
  analisis <- as_tibble(udpipe_annotate(modelo_ancora,
                                        temporal$texto))
  analisis <- analisis %>%
    add_column(nombre = NombreNovela[i],
               autores = AutoresNovela[i]) %>%
    select(-(paragraph_id),
           -(deps),
           -(misc),
           -(head_token_id),
           -(dep_rel)) %>%
    rename(parrafo_id = doc_id,
           enunciado_id = sentence_id,
           enunciado = sentence,
           lema = lemma,
           rasgos = feats) %>%
    mutate(parrafo_id = as.numeric(str_extract(parrafo_id, "\\d+")))
  Novelas_Analizado <- bind_rows(Novelas_Analizado, analisis)
  rm(temporal, analisis)
}

#Identificamos las columnas de la nueva tabla
colnames(Novelas_Analizado)
#Creamos un vector de números –efímero– con el orden en que queremos tener las columnas c(10,11,1,2,3,4,5,6,7,8,9)
Novelas_Analizado <- Novelas_Analizado[c(10, 11, 1, 2, 3, 4, 5, 6, 7, 8, 9)]

#Gráfico del POS de todas las novelas como conjunto
Novelas_Analizado %>%
  drop_na(upos) %>%
  count(upos, sort = T) %>%
  mutate(upos = reorder(upos, n)) %>%
  ggplot(aes(upos, n)) +
  geom_col(fill = "darkgreen") +
  coord_flip()

# Gráfico de valores relativos
#Gráfico del POS de todas las novelas como conjunto
Novelas_Analizado %>%
  drop_na(upos) %>%
  count(upos, sort = T) %>%
  mutate(frecuencia = n/sum(n)*100) %>%
  ggplot(aes(upos, n)) +
  geom_col(fill = "darkgreen") +
  coord_flip()
# Comparativa entre las clases de palabras utilizadas por cada autor
clases <- Novelas_Analizado %>%
  group_by(autores) %>%
  drop_na(upos) %>%
  count(upos, sort = T) %>%
  mutate(frecuencia = n/sum(n)*100)
  
ggplot(clases, aes(upos, frecuencia)) +
  geom_col() +
  geom_col(fill = "orange") +
  coord_flip() +
  facet_wrap(~autores)
