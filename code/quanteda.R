#####################################################3
#######Contenidos con QUANTEDA
#######INFO:  https://content-analysis-with-r.com/1-basics.html
#######################################
#CARGAMOS LIBRARIA
library(quanteda)
library(quanteda.textstats)
library(quanteda.textmodels)
library(quanteda.textplots)
library(readtext)
library(xlsx)
library(base)
library(tidyverse)
library(tidytext)
library(dplyr) #Data manipulation (also included in the tidyverse package)
library(tidyr) #Spread, separate, unite, text mining (also included in the tidyverse package)
library(scales)
library(lubridate)
library(widyr) #Use for pairwise correlation

library(RColorBrewer)
library(ggplot2) #Visualizations (also included in the tidyverse package)
library(ggrepel) #`geom_label_repel`
library(gridExtra) #`grid.arrange()` for multi-graphs
library(knitr) #Create nicely formatted output tables
library(kableExtra) #Create nicely formatted output tables
library(formattable) #For the color_tile function
library(circlize) #Visualizations - chord diagram
library(memery) #Memes - images with plots
library(magick) #Memes - images with plots (image_read)
library(yarrr)  #Pirate plot
library(radarchart) #Visualizations
library(igraph) #ngram network diagrams
library(ggraph) #ngram network diagrams

#######
# Podemos incorporar también 'quanteda.sentiment' y 'quanteda.dictionaries' de
# https://github.com/quanteda/quanteda.sentiment/tree/master/R

#ESTABLECEMOS DIRECTORIO
setwd("~/Desktop/Carmen_mola/corpus")
#CARGAMOS LOS TEXTOS PARA ANÁLISIS
novelas <- readtext("~/Desktop/Carmen_mola/corpus/*.txt") 
novelas$doc_id <- str_sub(novelas$doc_id, start = 1, end = -5)
#CONSTRUIMOS UN CORPUS
my.corpus <- corpus(novelas)
docvars(my.corpus, "Textno") <- sprintf("%02d", 1:ndoc(my.corpus))
my.corpus
#Reordenamos
my.corpus.stats <- summary(my.corpus)
my.corpus.stats$Text <- reorder(my.corpus.stats$Text, 1:ndoc(my.corpus), order = T)
my.corpus.stats
#VISUALIZAMOS TOKEN
ggplot(my.corpus.stats, aes(Text, Tokens, group = 1)) + geom_line() + geom_point() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + ggtitle("Palabras") + xlab("") + ylab("")
#VISUALIZAMOS TYPES
ggplot(my.corpus.stats, aes(Text, Types, group = 1)) + geom_line() + geom_point() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + ggtitle("Palabras tipo") + xlab("") + ylab("")
#VISUALIZAMOS ORACIONES
ggplot(my.corpus.stats, aes(Text, Sentences, group = 1)) + geom_line() + geom_point() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + ggtitle("Oraciones") + xlab("") + ylab("")
#ESTABLECEMOS LA RATIO TYPES/TOKENS
#la relación tipo-token es algo más interesante: mientras que dos entremeses 
#novelas (el 3 y el 5) tienen cada una un TTR por debajo del promedio, 
#otros dos están por encima de la relación lineal (1 y 7), con las 
# cuatro entremess restantes correspondiendo bastante exactamente al promedio. 
#Por lo tanto, el TTR se puede usar para sacar conclusiones 
#sobre la densidad de la información; volveremos a eso más adelante.
ggplot(my.corpus.stats, aes(Tokens, Types, group = 1, label = Textno)) + geom_smooth(method = "lm", se = FALSE) + geom_text(check_overlap = T) + ggtitle("Ratio (TTR)")

#######################################################################
#*********Podemos remodelar el corpus para trabajar sólo con una parte de uno de los textos 
str_sub(my.corpus[5], start = 1, end = 500)
#*******Dividimos un documento en frases
my.corpus.sentences <- corpus_reshape(my.corpus, to = "sentences")
my.corpus.sentences[201]
#******* De esta manera se puede formar un corpus parcial en el que 
# solo se contengan oraciones más largas de 25 palabras.
docvars(my.corpus.sentences, "CharacterCount") <- ntoken(my.corpus.sentences)
docvars(my.corpus.sentences, "LongSentence") <- ntoken(my.corpus.sentences) >= 25
my.corpus.sentences_long <- corpus_subset(my.corpus.sentences, LongSentence == TRUE)
my.corpus.sentences_long[1:5]
####################################################################
# TOKENIZACIÓN
my.tokens <- tokens(my.corpus) %>% as.list()
head(my.tokens$`DIAZ_Cartas`, 20)
# ELIMINAR TOKENS QUE NO DESEAMOS EMPLEAR
my.tokens <- tokens(my.corpus)
tokens.removed <- tokens_remove(my.tokens, c(",", "de", "y", "Salamanca", "Cristina", "Pancracio")) %>% as.list()
head(tokens.removed$`DIAZ_Cartas`)
# LIMPIAR TEXTO: NÚMEROS, PUNTUACIÓN, SÍMBOLOS, MAYÚSCULAS
my.tokens <- tokens(my.corpus, remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE) %>%
  tokens_tolower() %>% 
  tokens_remove(c(stopwords("spanish"), "cervantes", "salamanca", "pancracio", "cristina", "señor", "señora", "pues", "merced","miguel")) %>% 
  as.list()
head(my.tokens$`DIAZ_Cartas`)
# ELIMINAMOS PALABRAS DE FUNCIÓN
my.dfm <- dfm(my.corpus, remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, remove = stopwords("spanish"))
my.dfm

#PALABRAS CARACTERÍSTICAS POR APARECER EN POCAS OBRAS
head(my.dfm, n = 7, nf = 10)
# ELIMINAMOS AQUELLAS PALABRAS QUE SON POCO DIFERENCIALES
head(dfm_sort(my.dfm, decreasing = TRUE, margin = "both"), n = 7, nf = 10) 

dfm_select(
  my.dfm,
  pattern = NULL,
  selection = c("keep", "remove"),
  valuetype = c("fixed"),
  case_insensitive = TRUE,
  min_nchar = NULL,
  max_nchar = NULL,
  verbose = quanteda_options("verbose")
)

my.dfm <- dfm_remove(my.dfm, c(stopwords("spanish"), "así", "lorenza", "hortigosa", "leonarda", "chanfalla", "chirinos", "mariana", "guiomar", "cañizares", "salamanca", "pancracio", "cristina", "señor", "señora", "pues", "merced","miguel"))
my.dfm
#ANALIZAMOS LAS CARACTERÍSTICAS DE TODA LA TABLA DFM
#Con localización de dónde y en qué rango aparece cada característica
word.frequencies <- textstat_frequency(my.dfm) # more elaborate frequencies
head(word.frequencies)
#clasificar fácilmente por documentos y frecuencias de funciones utilizando dfm_sort .
head(dfm_sort(my.dfm, decreasing = TRUE, margin = "both"), n = 7, nf = 10) 
#Observar presencias concretas de ciertas palabras
dfm_select(my.dfm, pattern = "ayud*")
ayuda <- dfm_select(my.dfm, pattern = "ayud*")
#ANALIZAMOS LA FRECUENCIA RELATIVA DE LAS PALABRAS
#Propmax escala la frecuencia de palabras en relación con la palabra más frecuente
my.dfm.proportional <- dfm_weight(my.dfm, scheme = "propmax")
my.dfm.proportional
#TF-IDF ENCUENTRAN LOS TÉRMINOS MÁS DISTINTIVOS
my.dfm.tfidf <- dfm_tfidf(my.dfm)
topfeatures(my.dfm.tfidf)
#CREAMOS UNA MATRIZ CON LOS TÉRMINOS QUE SE PRODUCEN ALMENOS EN 6 DOCUMENTOS
my.dfm.trim <- dfm_trim(my.dfm, min_docfreq = 6)
head(my.dfm.trim, n = 7, nf = 10) 
#CREAMOS MATRIZ CON PALABRAS que se encuentran en el percentil de frecuencia 95
my.dfm.trim <- dfm_trim(my.dfm, min_termfreq = 0.95, termfreq_type = "quantile")
head(my.dfm.trim, n = 7, nf = 10) 
#GRAFICAMOS TF-IDF: los términos más distintivos según TF-IDF para 
# cuatro ENTREMESES, donde el color indica la novela respectiva. 
# El hecho de que el tamaño de la palabra en el gráfico no indique 
# la frecuencia absoluta sino el valor de TF-IDF hace que dicho 
# gráfico sea útil para la comparación directa.
textplot_wordcloud(my.dfm[4:7,], color = brewer.pal(4, "Set1"), min_size = 0.2, max_size = 4, max_words = 50, comparison = TRUE)


## SEGUNDO PASO: CONCORDANCIAS
#Concordancias con búsqueda de una palabra exacta
concordance <- kwic(my.corpus, "familia")
concordance
#Concordancias con búsqueda de lexemas
concordance <- kwic(my.corpus, c("logr*", "promet*"), window = 10, case_insensitive = FALSE)
concordance
#Guardar el resultado en u fichero de excel
write_delim(concordance, path = "concordance.csv", delim = ";") # file is Excel compatible

## SEGUNDO PASO: FRECUENCIA Y DISPERSIÓN
#calculamos la frecuencia y la dispersión de los tokens por narración, que contienen los términos 'x' y 'z'.
term1 <- kwic(my.corpus, "familia", valuetype = "regex", case_insensitive = FALSE) %>% group_by(docname) %>% summarise(hits = n()) %>% mutate(percentage = hits/(my.corpus.stats$Tokens/100), searchterm = "familia") %>% arrange(desc(percentage))
term2 <- kwic(my.corpus, "salud", valuetype = "regex", case_insensitive = FALSE) %>% group_by(docname) %>% summarise(hits = n()) %>% mutate(percentage = hits/(my.corpus.stats$Tokens/100), searchterm = "salud") %>% arrange(desc(percentage))
term1
term2
# Graficamos el resultado por frecuencia absoluta
terms.combined <- bind_rows(term1, term2)
terms.combined$docname <- factor(terms.combined$docname, levels = levels(my.corpus.stats$Text))
ggplot(terms.combined, aes(docname, hits, group = searchterm, fill = searchterm)) + 
  geom_bar(stat='identity', position = position_dodge(), size = 1) + 
  scale_fill_brewer(palette = "Set1") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
  ggtitle("Frequencia de \"familia\" and \"salud\" (absolute)") + 
  xlab("") + ylab("words (total)")

# Graficamos el resultado por frecuencia relativa
terms.combined <- bind_rows(term1, term2)
terms.combined$docname <- factor(terms.combined$docname, levels = levels(my.corpus.stats$Text))
ggplot(terms.combined, aes(docname, percentage, group = searchterm, fill = searchterm)) + 
  geom_bar(stat='identity', position = position_dodge(), size = 1) + 
  scale_fill_brewer(palette = "Set1") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
  ggtitle("Frequency de \"familia\" and \"salud\"  (relativa)") + 
  xlab("") + ylab("words (%)")

# Colocaciones
novelas.tokens <- tokens(my.corpus)
collocations <- textstat_collocations(novelas.tokens, min_count = 10)
arrange(collocations, desc(count))

#Ordenamos por frecuencia (a mayor porcentaje lambda, más raro es 
# el fenómeno medido: Lambda mide la resistencia a la asociación de la colocación  (y z es sólo un lambda z-estandarizada)
arrange(collocations, desc(lambda))

# TERCER PASO:
#Podemos calcular la distancia entre dos textos o documentos 
corpus.sentences <- corpus_reshape(my.corpus, to = "sentences")
dfm.sentences <- dfm(corpus.sentences, remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, remove = stopwords("spanish"))
dfm.sentences <- dfm_trim(dfm.sentences, min_docfreq = 5)
#Palabras más próximas
similarity.words <- textstat_simil(dfm.sentences, dfm.sentences[,"policía"], margin = "features", method = "cosine")
head(similarity.words[order(similarity.words[,1], decreasing = T),], 10)
#Palabras más alejadas
distance.words <- textstat_dist(dfm.sentences, dfm.sentences[,"frases"], margin = "features", method = "euclidean")
head(distance.words[order(distance.words[,1], decreasing = T),], 10)

# KEYNES
# esto es, medida conveniente del carácter distintivo de un
# término para un texto determinado (es decir, qué tan fuertemente 
# el término caracteriza el texto respectivo en comparación con
# el corpus completo).
# Comenzamos con Partido Popular
keyness <- textstat_keyness(my.dfm, target = "MOLA_LaNena", measure = "lr")
textplot_keyness(keyness)
#Vox
keyness <- textstat_keyness(my.dfm, target = "Vox", measure = "lr")
textplot_keyness(keyness)

#Diversidad léxica
# Describe la diversidad de palabras y, por lo tanto, también
# proporciona información sobre la complejidad de un texto. 
# Calculamos numerosas métricas para la diversidad léxica
lexical.diversity <- textstat_lexdiv(my.dfm, measure = "all")
lexical.diversity

# Exportar datos más usadas por usuario
# install.packages("xlsx")
library("writexl")
# convertir tabla en data.frame
df <- data.frame(lexical.diversity)
write_xlsx(df, "diversidad_lexica.xlsx")

#Legibillidad
readability <- textstat_readability(my.corpus, measure = "all")
readability

# Exportar datos más usadas por usuario
# install.packages("xlsx")
library("writexl")
# convertir tabla en data.frame
df <- data.frame(readability)
write_xlsx(df, "legibilidad.xlsx")
#CUARTO PASO
# Creación de un diccionario quanteda
test.lexicon <- dictionary(list(positive.terms = c("felicidad", "alegría", "luz"), negative.terms = c("tristeza", "angustia", "oscuridad")))
test.lexicon

#Importar un diccionario
dictliwc <- dictionary(file = "~/Desktop/DICCIONARIOS/Spanish_LIWC2021_Dictionary.dic", format = "LIWC")
#aplicarlo al corpus
sentimiento <- head(dfm(my.corpus, dictionary = "nrc"))
head(sentimiento)
# Psarlo a un fichero excel

df <- convert(sentimiento, to = "data.frame")
write.xlsx(df, "sentimiento_liwc.xlsx")

# #Importar OTRO diccionario
dictliwc2 <- dictionary(file = "~/Desktop/DICCIONARIOS/sentimientosTOTAL.txt", format = "wordstat")



#Trabajamos con positivas/negativos
positive.words.bl <- scan("~/Desktop/DICCIONARIOS/positivo.txt", what = "char", skip = 35, quiet = T)
negative.words.bl <- scan("~/Desktop/DICCIONARIOS/negativo.txt", what = "char", skip = 35, quiet = T)


#Configuramos nuestro diccionario
sentiment.dictionary <- dictionary(list(positive = positive.words.bl, negative = negative.words.bl))
str(sentiment.dictionary)

#Aplicamos al corpus
dfm.sentiment <- dfm(my.corpus, dictionary = sentiment.dictionary)
dfm.sentiment

# Psarlo a un fichero excel
library("writexl")
df <- data.frame(dfm.sentiment)
write_xlsx(df, "positivo_negativo.xlsx")

# o en su caso

df <- convert(dfm.sentiment, to = "data.frame")
write.xlsx(df, "positivo_negativo.xlsx")

#Gráfico
sentimient <- convert(dfm.sentiment, "data.frame") %>%
  gather(positive, negative, key = "Polarity", value = "Words") %>% 
  mutate(document = as_factor(doc_id)) %>% 
  rename(Novel = doc_id)
ggplot(sentimient, aes(doc_id, Words, fill = Polarity, group = Polarity)) + geom_bar(stat='identity', position = position_dodge(), size = 1) + scale_fill_brewer(palette = "Set1") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + ggtitle("Sentiment scores in twelve Sherlock Holmes novels") + xlab("")


# Frecuencia relativa
dfm.sentiment.prop <- dfm_weight(dfm.sentiment, scheme = "prop")
dfm.sentiment.prop

#Gráfico
sentiment <- convert(dfm.sentiment.prop, "data.frame") %>%
  gather(positive, negative, key = "Polarity", value = "Share") %>% 
  mutate(document = as_factor(doc_id)) %>% 
  rename(novelas = document)
ggplot(sentiment, aes(doc_id, Share, fill = Polarity, group = Polarity)) + geom_bar(stat='identity', position = position_dodge(), size = 1) + scale_fill_brewer(palette = "Set1") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + ggtitle("Sentiment scores in twelve Sherlock Holmes novels (relative)")


#Cargamos diccionarios

sentimientos <- read_tsv("~/Desktop/DICCIONARIOS/sentimientos_2.txt",
                         col_types = "cccn",
                         locale = default_locale())
sentimientos

# Diccionario
library(tidyverse)
library(tidytext) #unnesting text into single words
library(mlr) #machine learning framework for R
library(kableExtra) #create attractive tables
library(circlize) #cool circle plots
library(jpeg) #read in jpg files for the circle plots

#define some colors to use throughout
my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00", "#D65E00")

#customize the text tables for consistency using HTML formatting
my_kable_styling <- function(dat, caption) {
  kable(dat, "html", escape = FALSE, caption = caption) %>%
    kable_styling(bootstrap_options = c( "condensed", "bordered"),
                  full_width = FALSE)
}



sentimientos %>%
  group_by(lexicon, sentimiento, palabra) %>%
  summarise(distinct_words = n_distinct(palabra)) %>%
  ungroup() %>%
  spread(sentimiento, distinct_words) %>%
  mutate(lexicon = color_tile("lightblue", "lightblue")(lexicon),
         words_in_lexicon = color_bar("lightpink")(palabra)) %>%
  my_kable_styling(caption = "Word Counts Per Lexicon")

#####################################################
load("data/twitter/trumpclinton.RData")
trumpclinton.stats.monthly



