#Exercice: Utiliser vos connaissances de la première leçon et utiliser la fonction
#pour lire le magic number du fichier “emnist-digits-train-images-idx3-ubyte”.
WORKDIR <- "~/Documents/M2-fouille-donn-es/PLafaye/TP"
EMNIST.URL <- "http://www.itl.nist.gov/iaui/vip/cs_links/EMNIST/gzip.zip"
setwd(WORKDIR)
if(!dir.exists(paste(WORKDIR, "/Data", sep = ""))) dir.create(paste(WORKDIR, "/Data", sep = ""))
if(!file.exists(paste(WORKDIR, "/Data/emnist.zip", sep = "")))
  download.file(EMNIST.URL, destfile = paste(WORKDIR, "/Data/emnist.zip", sep = ""))
unzip("Data/emnist.zip", exdir = "Data")
if (!"R.utils" %in% installed.packages()) install.packages("R.utils")
if (!file.exists("Data/emnist-digits-train-images-idx3-ubyte")) {
  for (file in c("emnist-digits-train-images-idx3-ubyte",
                 "emnist-digits-train-labels-idx1-ubyte",
                 "emnist-digits-test-images-idx3-ubyte",
                 "emnist-digits-test-labels-idx1-ubyte")) {
    R.utils::gunzip(paste("Data/", file, ".gz", sep = ""),
                    destname = paste("Data/", file, sep = ""))
  }
}
unlink("Data/gzip", recursive = TRUE) # Pour supprimer le dossier 'gzip'.

WORKDIR.Data <- paste( WORKDIR,"/Data", sep = "")
setwd(WORKDIR.Data)

file_training_set_image <- "emnist-digits-train-images-idx3-ubyte"
file_training_set_label <- "emnist-digits-train-labels-idx1-ubyte"
file_test_set_image <- "emnist-digits-test-images-idx3-ubyte"
file_test_set_label <- "emnist-digits-test-labels-idx1-ubyte"

#Exercice: Utiliser vos connaissances de la première leçon et utiliser la fonction pour lire le magic number du fichier “emnist-digits-train-images-idx3-ubyte”.re
# see : http://yann.lecun.com/exdb/mnist/
raw <- paste("0x",paste(readBin(file_training_set_image,"raw",n=4)[0:4],collapse = ""),sep="")
raw
as.numeric(raw) 
#2051


#Exercice: Créer un code R permettant de lire le nombre d’images stockées dans le fichier“emnist-digits-train-images-idx3-ubyte”.
#position 5 à  8
raw <- paste("0x",paste(readBin(file_training_set_image,"raw",n=8)[5:8],collapse = ""),sep="")
raw
as.numeric(raw) 
# 240 00 images 

#Exercice: Extraire la première image du fichier “emnist-digits-train-images-idx3-ubyte”. Af-
#  ficher la dans R au moyen de la fonction R image() .

#Exercice: Calculer la taille totale de toutes les images (en GB) du training set. Vérifier votre
#calcul au moyen de la fonction object.size() .




readBindata <-function(file, from,n){
  as.numeric(paste("0x",paste(readBin(file,"raw",n=n)[from:n],collapse = ""),sep=""))
}
extract_images <- function(file, nbimages = NULL) {
  if (is.null(nbimages)) { # On extrait toutes les images
    nbimages <- readBindata(file, 5, 8)
  }
  nbrows <- readBindata(file, 9, 12)
  nbcols <- readBindata(file, 13, 16)
  raw <- readBin(file, "raw", n = nbimages * nbrows * nbcols + 16)[-(1:16)]
  return(array(as.numeric(paste("0x", raw, sep="")), dim = c(nbcols, nbrows, nbimages)))
}
extract_labels <- function(file) {
  nbitem <- readBindata(file, 5, 8)
  raw <- readBin(file, "raw", n = nbitem + 8)[-(1:8)]
  return(as.numeric(paste("0x", raw, sep="")))
}
nbimages_to_extract <- 10
images_training_set <- extract_images(file_training_set_image, nbimages_to_extract)
images_test_set <- extract_images(file_test_set_image, nbimages_to_extract)
labels_training_set <- extract_labels(file_training_set_label)
labels_test_set <- extract_labels(file_test_set_label)
par(ask = FALSE)
for (i in 1:nbimages_to_extract) {
  img <- images_training_set[,,i]
  image(t(img)[,nrow(img):1], col = gray((255:0)/256))
  title(paste("Label:", labels_training_set[i]))
}
# Taille totale de toutes les images (en GB):
(as.numeric(object.size(images_training_set)) / 10) * 280000 / 1024 / 1024 / 1024




