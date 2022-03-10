#################ROLLING.CLASSIFY#############################
#Establecemos directorio
setwd("~/Desktop/ZZZ")
#Cargamos la librería de 'stylo'
library(stylo)
stylo()

# Generamos las dos carpetas preceptivas en el directorio 
# (reference_set, con los potenciales autores, y test_set, con el texto dubitado)
# y aplicamos diferentes algoritmos: 
# svm
rolling.classify(write.png.file = TRUE, classification.method = "svm", mfw=500, training.set.sampling = "normal.sampling", slice.size = 5000, slice.overlap = 4500)
# nsc
rolling.classify(write.png.file = TRUE, classification.method = "nsc", mfw=50, training.set.sampling = "normal.sampling", slice.size = 5000, slice.overlap = 4500) 
# delta
rolling.classify(write.png.file = TRUE, classification.method = "delta", mfw=1000) 
# delta
rolling.classify(write.png.file = TRUE, classification.method = "delta", mfw=500) 

rolling.classify(write.png.file = TRUE, classification.method = "svm", mfw = 100,
                 training.set.sampling = "normal.sampling", slice.size = 5000,
                 slice.overlap = 4500)
