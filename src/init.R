# ----------------------------------------------------
# Initialisation des variables globales

varglobales = list()

# Paths
varglobales$dir         = list()
varglobales$dir$root    = getwd()

varglobales$dir$src     = file.path(varglobales$dir$root,"src")
varglobales$dir$data    = file.path(varglobales$dir$root,"data")
varglobales$dir$R       = file.path(varglobales$dir$root,"R")


# ----------------------------------------------------
# Functions

'%!in%' <- function(x,y){!('%in%'(x,y))}