# Caricamento della lista di matrici da un file RDS
lista_matrici_caricata <- readRDS("resampling_res.rds")

# Visualizzazione della prima matrice nella lista
print(lista_matrici_caricata[[1]])

