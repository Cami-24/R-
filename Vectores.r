#Vectores en R
Edades <- c(23, 45, 12, 34, 56, 78, 90)
# Calcular la media de las edades
media_edades <- mean(Edades)
cat(paste("Hay un total de 7 edades en nuestro vector. Estas son: "))
cat(Edades)
cat(sep="\n")

cat("La media de las edades contenidas en este vector es: ", media_edades)
cat(sep="\n")

#Lista
Lista <- list("Hola!", "Esta es", "una lista", "en R!")
cat("Tambien podemos hacer listas en R. Esto contiene nuestra lista: ")
cat(sep="\n")
print(Lista)

#Lista heterogeneas
Listah <- list("Jose", 14, TRUE, 3.14, Edades)
cat(paste("Las listas tambien pueden contener diferentes tipos de datos (son heterogeneas): "))
cat(sep="\n")
cat(paste("Pueden haber Strings: ", Listah[[1]]))
cat(sep="\n")
cat(paste("Numeros enteros: ", Listah[[2]]))
cat(sep="\n")
cat(paste("Valores booleanos: ", Listah[[3]]))
cat(sep="\n")
cat(paste("Numeros decimales: ", Listah[[4]]))
cat(sep="\n")
cat("  Tambien pueden contener otros objetos, como nuestro vector de edades: ")
cat(Listah[[5]])
 # :)