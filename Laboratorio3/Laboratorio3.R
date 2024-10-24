library(ape)
library(phangorn)
library(phytools)

fraxatin <- read.phyDat(file = "fraxatin_aligned.fasta", 
                        format = "FASTA", type = "AA")
fraxatin
#Los estados son los aminoácidos A (alanina), R (arginina), etc
#La clase debe ser aa senquence, as.AAbin(fraxatin)
matrizdist <- as.AAbin(fraxatin)
##dist.aa, calcula una matriz de distancias por pares de las secuencias de aminoácidos partir de una objecto de clase AAbin utilizando un modelo de evolución de aminoácidos (por ejemplo Dayhoff).
matrizdist <- dist.aa(matrizdist)
matrizdist
#creamos un árbol con el método de grupo de pares no ponderados con media aritmética (UPGMA) usando la matriz de distancia que acabamos de calcular.
arbolUPGMA <- upgma(matrizdist)
plot(arbolUPGMA)
#Ahora hacemos un árbol con el método de unión de vecinos (NJ) usando la misma matriz de distancias:
arbolNJ <- nj(matrizdist)
plot(arbolNJ)
#Para personalizar los árboles podemos agregar argumentos a parámetros como cex, para el tamaño de la letra, edge.color, para el grosos de las ramas, etc. También se puede escoger entre diferentes visualizaciones de árbol como filograma, cladograma, radial y demás.
plot(arbolUPGMA, type= "p", cex=0.8, edge.width=2, edge.color="red", font=3)
#cambiando type cambiamos la estructura de las ramas
plot(arbolUPGMA, type= "c", cex=0.8, edge.width=2, edge.color="blue", font=3)
#Otro tipo:
plot(arbolUPGMA, type= "p", label.offset=0.0005, edge.lty=1, node.pos=2, cex=0.8, edge.width=2, edge.color="black", font=3)
#Además de plot podemos graficar árboles con el método plotTree del paquete phytools, el cual es compatible con ape y con phangorn.
plotTree(arbolNJ)
#este tambien puede modificarse como:
Arbolparaguardar<- plotTree(arbolNJ, ftype="b", fsize=0.8, offset=1, color="red", lwd=2)
#Para cambiar el orden en que los grupos son visualizados.
plotTree(ladderize(arbolNJ))
#IMPORTANTE Para guardar un árbol podemos usar el comando write.tree(nombredelarbol, file = "file_name.nex"). Este archivo puede ser leído usando read.tree(file = "file_name.nex").
#Para enraizarlos podemos usar la función root del paquete ape.
arbolNJraiz <-root(arbolNJ, outgroup = "Ornitorrinco", r = TRUE)
plot(arbolNJraiz)
#Se puede hacer lo mismo con el árbol creado a partir del método UPGMA.
#Cuando se enraíza un árbol, se determina un punto de origen evolutivo (el ancestro común), y se asigna un sentido direccional a la evolución de las especies representadas.
arbolUPGMAraiz <-root(arbolUPGMA, outgroup = "Ornitorrinco", r=TRUE)
plot(arbolUPGMAraiz)
#Voy a guardar el arbol NJ
write.tree(arbolNJ, file = "ArbolNJ.nex")
#Para leerlo
read.tree(file = "ArbolNJ.nex")
#Además podemos visualizar los dos árboles a la vez con los siguientes comandos:(layout=disposicion)
layout(matrix(c(1,2)), height=c(10,10))
par(mar=c(1,1,1,1))
plot(arbolUPGMAraiz, label.offset=0.0005, main="ARBOL UPGMA", cex=0.4)
plot(arbolNJraiz, label.offset=0.0005, main="ARBOL NJ", cex=0.4)
#La parsimonia busca disminuir el número de pasos que explican un árbol evolutivo contando el número de cambios de cada uno de los caracteres.
#Se utiliza un árbol de inicio obtenido por distancia y se cuenta su número de pasos
parsimony(arbolUPGMAraiz, fraxatin)
#[313]
#El árbol arbolUPGMAraiz tiene 313 pasos, algo importante es que aunque esté con raíz o no el número de pasos debe ser el mismo. Probémoslo usando el árbol sin raíz:
#número de cambios o longitudes de las ramas en el árbol,
#Enraizar un arbol no cambia la distancia entre nodos
# los nodos son puntos clave en la estructura del árbol que representan distintos niveles de relaciones evolutivas
#El nodo es el punto de bifurcación
#ahora miraremos los enraizados, que no debe haber diferencia solo por enraizarlos.
parsimony(arbolUPGMA, fraxatin)
#[313]
#Con el método optim.parsimony se obtiene el árbol con mejor parsimonia. Este método permite encontrar árboles bajo máxima parsimonia usando árboles de distancia de inicio.
mejorUPGMA <- optim.parsimony(arbolUPGMAraiz, fraxatin)
#Resultado-->Final p-score 307 after  2 nni operations 
#con la parsimonia se define el arbol que requiere menor numero de pasos evolutivos
#Ahora hagámoslo con el árbol de NJ:
mejorNJ <- optim.parsimony(arbolNJraiz, fraxatin)
#Resultado:Final p-score 307 after  1 nni operations 
#optimiza la búsqueda del árbol más parsimonioso al iterar entre el árbol actual y pequeñas modificaciones de este, cambiando repetidamente las longitudes de las ramas y las relaciones entre los nodos. Al ser un método heurístico, no garantiza encontrar siempre el árbol más parsimonioso, pero puede ser más rápido y efectivo que otros métodos cuando se trabaja con conjuntos de datos grandes o complejos
fraxatin_parsimonia <- pratchet(fraxatin, all = TRUE)
fraxatin_parsimonia
#result: 4 phylogenetic trees
#Para poderlos comparar es necesario enraizarlos.
fraxatin_parsimoniaR <- root(phy = fraxatin_parsimonia, outgroup = "Ornitorrinco")
plot(fraxatin_parsimoniaR, cex = 0.6)
#Para hacer un árbol de consenso estricto podemos usar el método ape con parámetro p de 1, que corresponde a un 100% de consenso entre ramas.
estrictode100 <- consensus(fraxatin_parsimoniaR, p = 1)
plot(estrictode100, cex = .6)
#Para un árbol menos estricto podemos cambiar el valor del parámetro p
estrictode30 <- consensus(fraxatin_parsimoniaR, p = 0.3)
plot(estrictode30, cex = .6)
#Bootstrap
arbolesbootstrap <- bootstrap.phyDat(fraxatin, FUN = pratchet, bs = 10)
#En este caso usamos la función pratchet, si usáramos otra se demoraría más. También usamos un número de réplicas irrisosio pues este es un ejercicio demostrativo y no es necesario gastar mucho tiempo en él. La rutina anterior genera entonces 10 árboles pseudoréplicas.
plot(arbolesbootstrap, cex = .6)
#Secuencia alineamiento matriz arbol--> orden; parsimonia es buscar un proceso en menos pasos posibles, transformar la matriz en ramas y el valor que te da es el nº de pasos seguidos
#Ahora bien, generamos un consenso; en este caso con un consenso al 60%:
estricto60 <- consensus(arbolesbootstrap, p = 0.6)
plot(estricto60, cex = .6)
#Árboles de máxima verosimilitud
arbolazar <- rtree(n = 11, tip.label = names(fraxatin))
plot(arbolazar, cex = .5)
#En seguida lo enraizamos por las secuencias de Ornitorinco para poderlo visualizar mejor. Además los «escalerizamos» hacia la derecha y le agregamos escala; aquí la longitud de la rama sí es significativa, indica cantidad de cambio en cuanto a sustituciones de aminoácidos.
arbolazarR <- root(phy = arbolazar, outgroup = "Ornitorrinco")
plot(ladderize(arbolazarR), cex = .5); add.scale.bar()
#Con pml (Phylogenetic maximum likelihood), podemos computar tal verosimilitud.
ajustado <- pml(arbolazarR, fraxatin)
ajustado
#Resultado: modelmk
#loglikelihood: -3965.793 
#unconstrained loglikelihood: -1479.871 
#Rate matrix: 4 phylogenetic trees
#Lo que hay que hacer es encontrar un árbol que optimice la verosimilitud usando un modelo de sustitución
#vamos a usar el método optim.pml del paquete phangorn
ajustadoconDay <- optim.pml(object = ajustado, model = "Dayhoff", rearrangement = "ratchet")
#Para ver el árbol oculto usamos $tree. También lo enraizamos. 
ajustadoconDay$tree
#y enraizamos
ajustadoconDayraíz <- root(ajustadoconDay$tree, outgroup = "Ornitorrinco")
plot(ladderize(ajustadoconDayraíz), cex = .5); add.scale.bar()
#El árbol anterior fue generado usando la matriz de sustitución de Dayhoff. Pero se pueden usar diferentes modelos.
ajustadoconBlo <- optim.pml(object = ajustado, model = "Blosum62", rearrangement = "ratchet")
ajustadoconJTT <- optim.pml(object = ajustado, model = "JTT", rearrangement = "ratchet")
#Podemos comparar los modelos calculando el Criterio de información de Akaike AIC:
AIC(ajustadoconDay, ajustadoconBlo, ajustadoconJTT)
#La primera columna corresponde a los grados libertad. Según el criterio anterior, el mejor modelo que se ajusta con los datos (con el AIC más bajo) es JTT modelo de Jones-Taylor-Thornton para evaluar la distancia entre secuencias de proteínas y optimiza la verosimilitud.
mejorarbol <- optim.pml(
  object = ajustadoconDay, 
  model = "JTT", 
  rearrangement = "ratchet")
mejorarbol
mejorarbolR <- root(mejorarbol$tree, outgroup = "Ornitorrinco")
plot(ladderize(mejorarbolR), cex = 0.5); add.scale.bar()
