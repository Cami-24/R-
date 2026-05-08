
# sistema para recomendar películas a usuarios en R
# Estructuras usadas: vectores, listas, data frames, matrices, environments
# Algoritmo: Similitud de coseno + Árbol de decisión manual

cat("SISTEMA DE RECOMENDACIÓN DE PELÍCULAS\n\n")


# 1. Estructuras


# 1.1 data frame: catálogo de películas
catalogo <- data.frame(
  id = 1:15,
  
  titulo = c(
    "The Shawshank Redemption", "The Dark Knight", "Pulp Fiction",
    "Forrest Gump", "The Matrix", "Inception",
    "Interstellar", "The Godfather", "Fight Club",
    "Parasite", "Toy Story", "Finding Nemo",
    "The Avengers", "Titanic", "Coco"
  ),
  genero = c(
    "Drama", "Accion", "Thriller",
    "Drama", "SciFi", "SciFi",
    "SciFi", "Drama", "Thriller",
    "Thriller", "Animacion", "Animacion",
    "Accion", "Romance", "Animacion"
  ),
  anio = c(
    1994, 2008, 1994,
    1994, 1999, 2010,
    2014, 1972, 1999,
    2019, 1995, 2003,
    2012, 1997, 2017
  ),
  rating_promedio = c(
    9.3, 9.0, 8.9,
    8.8, 8.7, 8.8,
    8.6, 9.2, 8.8,
    8.5, 8.3, 8.2,
    8.0, 7.9, 8.4
  ),
  stringsAsFactors = FALSE
)

cat("Catálogo de películas (Data Frame)\n")
print(catalogo[, c("id", "titulo", "genero", "rating_promedio")])
cat("\n")

# 1.2 matriz: ratings de usuarios x películas
# Filas = usuarios, Columnas = películas 
# 0 = no ha visto la película
set.seed(42)
n_usuarios <- 6
n_peliculas <- nrow(catalogo)

nombres_usuarios <- c("Ana", "Bruno", "Carla", "Diego", "Elena", "Felipe")

ratings_matrix <- matrix(
  c(
    5, 4, 0, 5, 0, 3, 0, 5, 0, 0, 4, 3, 0, 5, 4,  # Ana
    0, 5, 4, 0, 5, 5, 4, 0, 4, 3, 0, 0, 5, 0, 0,  # Bruno
    5, 0, 0, 5, 0, 0, 0, 4, 0, 0, 5, 5, 0, 4, 5,  # Carla
    0, 5, 5, 0, 4, 5, 5, 0, 5, 4, 0, 0, 4, 0, 0,  # Diego
    4, 0, 0, 4, 0, 0, 3, 5, 0, 0, 0, 0, 0, 5, 3,  # Elena
    0, 4, 3, 0, 5, 4, 4, 0, 3, 5, 0, 0, 5, 0, 0   # Felipe
  ),
  nrow = n_usuarios, ncol = n_peliculas, byrow = TRUE
)
rownames(ratings_matrix) <- nombres_usuarios
colnames(ratings_matrix) <- catalogo$titulo

cat("Matriz de ratings (usuarios x películas)\n")
print(ratings_matrix)
cat("\n")

# 1.3 lista: perfil de preferencias por usuario
# Cada usuario tiene una lista con sus géneros favoritos y rating promedio
construir_perfiles <- function(ratings_mat, catalogo_df, nombres) {
  perfiles <- list()
  for (i in seq_along(nombres)) {
    peliculas_vistas <- which(ratings_mat[i, ] > 0)
    generos_vistos <- catalogo_df$genero[peliculas_vistas]
    ratings_dados <- ratings_mat[i, peliculas_vistas]

    # Tabla de frecuencia de géneros 
    freq_generos <- table(generos_vistos)

    perfiles[[nombres[i]]] <- list(
      peliculas_vistas = peliculas_vistas,
      ratings = ratings_dados,
      rating_promedio = mean(ratings_dados),
      genero_favorito = names(which.max(freq_generos)),
      frecuencia_generos = as.list(freq_generos)
    )
  }
  return(perfiles)
}

perfiles_usuarios <- construir_perfiles(ratings_matrix, catalogo, nombres_usuarios)

cat("Perfiles de usuarios\n")
for (nombre in nombres_usuarios) {
  p <- perfiles_usuarios[[nombre]]
  cat(sprintf("  %s: género favorito = %s, rating promedio = %.1f, películas vistas = %d\n",
              nombre, p$genero_favorito, p$rating_promedio, length(p$peliculas_vistas)))
}
cat("\n")

# 1.4 hashmap: caché de similitudes
cache_similitudes <- new.env(hash = TRUE, parent = emptyenv())


# 2. Algoritmo con similitud de coseno para filtrado colaborativo usuario-usuario


similitud_coseno <- function(vec_a, vec_b) {
  # Solo considerar películas que ambos hayan visto
  comunes <- which(vec_a > 0 & vec_b > 0)
  if (length(comunes) < 2) return(0)

  a <- vec_a[comunes]
  b <- vec_b[comunes]

  dot_product <- sum(a * b)
  norma_a <- sqrt(sum(a^2))
  norma_b <- sqrt(sum(b^2))

  if (norma_a == 0 || norma_b == 0) return(0)
  return(dot_product / (norma_a * norma_b))
}

# Calcula similitudes entre todos los pares de usuarios y guarda en caché
cat("Similitudes entre usuarios (Coseno + caché en Environment)\n")
for (i in 1:(n_usuarios - 1)) {
  for (j in (i + 1):n_usuarios) {
    sim <- similitud_coseno(ratings_matrix[i, ], ratings_matrix[j, ])
    # Guardar en caché (environment como hash map)
    clave <- paste(nombres_usuarios[i], nombres_usuarios[j], sep = "-")
    assign(clave, sim, envir = cache_similitudes)
    if (sim > 0) {
      cat(sprintf("  %s <-> %s : %.4f\n", nombres_usuarios[i], nombres_usuarios[j], sim))
    }
  }
}
cat("\n")

# Función para obtener los usuarios más similares
obtener_vecinos <- function(usuario_idx, k = 3) {
  similitudes <- numeric(n_usuarios)
  for (j in 1:n_usuarios) {
    if (j == usuario_idx) next
    i_min <- min(usuario_idx, j)
    i_max <- max(usuario_idx, j)
    clave <- paste(nombres_usuarios[i_min], nombres_usuarios[i_max], sep = "-")
    sim <- tryCatch(get(clave, envir = cache_similitudes), error = function(e) 0)
    similitudes[j] <- sim
  }
  # Retornar los k con mayor similitud
  orden <- order(similitudes, decreasing = TRUE)
  vecinos <- orden[1:min(k, length(orden))]
  return(list(indices = vecinos, similitudes = similitudes[vecinos]))
}


# 3. arbol de decisión manual para filtrado basado en contenido

# El árbol filtra las recomendaciones según preferencias del usuario.
# Estructura: lista donde cada nodo tiene:
#   - criterio: nombre de la variable de decisión
#   - umbral: valor para dividir o categorías 
#   - izquierda / derecha: subárboles o hojas
#   - hoja: true o false si es nodo terminal
#   - decision: "recomendar" o "no_recomendar"

# Construye el árbol
arbol <- list(
  criterio = "rating_promedio",
  umbral = 8.0,
  hoja = FALSE,
  # Rating >= 8.0
  derecha = list(
    criterio = "genero_match",
    umbral = TRUE,
    hoja = FALSE,
    # Género coincide con favorito del usuario
    derecha = list(
      hoja = TRUE,
      decision = "RECOMENDAR (alta prioridad)",
      prioridad = 1
    ),
    # Género no coincide pero rating alto
    izquierda = list(
      criterio = "rating_promedio",
      umbral = 8.5,
      hoja = FALSE,
      derecha = list(
        hoja = TRUE,
        decision = "RECOMENDAR (explorar nuevo género)",
        prioridad = 2
      ),
      izquierda = list(
        hoja = TRUE,
        decision = "CONSIDERAR",
        prioridad = 3
      )
    )
  ),
  # Rating < 8.0
  izquierda = list(
    criterio = "genero_match",
    umbral = TRUE,
    hoja = FALSE,
    derecha = list(
      hoja = TRUE,
      decision = "CONSIDERAR (género favorito pero rating bajo)",
      prioridad = 3
    ),
    izquierda = list(
      hoja = TRUE,
      decision = "NO RECOMENDAR",
      prioridad = 4
    )
  )
)

# Función para recorrer el árbol
evaluar_arbol <- function(nodo, pelicula_rating, genero_match) {
  if (nodo$hoja) {
    return(list(decision = nodo$decision, prioridad = nodo$prioridad))
  }

  if (nodo$criterio == "rating_promedio") {
    if (pelicula_rating >= nodo$umbral) {
      return(evaluar_arbol(nodo$derecha, pelicula_rating, genero_match))
    } else {
      return(evaluar_arbol(nodo$izquierda, pelicula_rating, genero_match))
    }
  } else if (nodo$criterio == "genero_match") {
    if (genero_match) {
      return(evaluar_arbol(nodo$derecha, pelicula_rating, genero_match))
    } else {
      return(evaluar_arbol(nodo$izquierda, pelicula_rating, genero_match))
    }
  }
}

# Función para imprimir el árbol
imprimir_arbol <- function(nodo, nivel = 0) {
  indent <- paste(rep("  ", nivel), collapse = "")
  if (nodo$hoja) {
    cat(sprintf("%s└── [HOJA] %s (prioridad: %d)\n", indent, nodo$decision, nodo$prioridad))
    return()
  }
  cat(sprintf("%s├── %s (umbral: %s)\n", indent, nodo$criterio,
              as.character(nodo$umbral)))
  cat(sprintf("%s│   ├── SÍ:\n", indent))
  imprimir_arbol(nodo$derecha, nivel + 2)
  cat(sprintf("%s│   └── NO:\n", indent))
  imprimir_arbol(nodo$izquierda, nivel + 2)
}

cat("Árbol de decisión (estructura de lista anidada) \n")
imprimir_arbol(arbol)
cat("\n")


# 4. generar

recomendar <- function(usuario_nombre, top_n = 5) {
  usuario_idx <- which(nombres_usuarios == usuario_nombre)
  perfil <- perfiles_usuarios[[usuario_nombre]]
  genero_fav <- perfil$genero_favorito

  cat(sprintf("Recomendaciones para %s (género favorito: %s)\n\n",
              usuario_nombre, genero_fav))

  # Paso 1: se encuentran vecinos similares
  vecinos <- obtener_vecinos(usuario_idx, k = 3)
  cat("Vecinos más similares:\n")
  for (v in seq_along(vecinos$indices)) {
    cat(sprintf("  %s (similitud: %.4f)\n",
                nombres_usuarios[vecinos$indices[v]], vecinos$similitudes[v]))
  }
  cat("\n")

  # Paso 2: se predicen ratings para películas no vistas
  peliculas_no_vistas <- which(ratings_matrix[usuario_idx, ] == 0)

  if (length(peliculas_no_vistas) == 0) {
    cat("  ¡Este usuario ya vio todas las películas!\n")
    return(invisible(NULL))
  }

  predicciones <- data.frame(
    id = catalogo$id[peliculas_no_vistas],
    titulo = catalogo$titulo[peliculas_no_vistas],
    genero = catalogo$genero[peliculas_no_vistas],
    rating_catalogo = catalogo$rating_promedio[peliculas_no_vistas],
    rating_predicho = numeric(length(peliculas_no_vistas)),
    decision_arbol = character(length(peliculas_no_vistas)),
    prioridad = integer(length(peliculas_no_vistas)),
    stringsAsFactors = FALSE
  )

  for (p in seq_along(peliculas_no_vistas)) {
    pelicula_idx <- peliculas_no_vistas[p]

    # Rating predicho = promedio ponderado por similitud de los vecinos
    suma_ponderada <- 0
    suma_pesos <- 0
    for (v in seq_along(vecinos$indices)) {
      vecino_idx <- vecinos$indices[v]
      rating_vecino <- ratings_matrix[vecino_idx, pelicula_idx]
      if (rating_vecino > 0) {
        peso <- vecinos$similitudes[v]
        suma_ponderada <- suma_ponderada + (rating_vecino * peso)
        suma_pesos <- suma_pesos + peso
      }
    }

    if (suma_pesos > 0) {
      predicciones$rating_predicho[p] <- round(suma_ponderada / suma_pesos, 2)
    } else {
      predicciones$rating_predicho[p] <- catalogo$rating_promedio[pelicula_idx]
    }

    # Paso 3: pasar por el árbol de decisión
    genero_match <- catalogo$genero[pelicula_idx] == genero_fav
    resultado_arbol <- evaluar_arbol(arbol, catalogo$rating_promedio[pelicula_idx], genero_match)
    predicciones$decision_arbol[p] <- resultado_arbol$decision
    predicciones$prioridad[p] <- resultado_arbol$prioridad
  }

  # Ordenar por prioridad (menor = mejor) y luego por rating predicho
  predicciones <- predicciones[order(predicciones$prioridad, -predicciones$rating_predicho), ]

  cat("Películas evaluadas por el árbol de decisión:\n")
  for (r in 1:nrow(predicciones)) {
    cat(sprintf("  %d. %-30s | Género: %-10s | Rating pred: %.2f | %s\n",
                r,
                predicciones$titulo[r],
                predicciones$genero[r],
                predicciones$rating_predicho[r],
                predicciones$decision_arbol[r]))
  }

  cat(sprintf("\n>> TOP %d recomendaciones:\n", min(top_n, nrow(predicciones))))
  top <- head(predicciones, top_n)
  for (r in 1:nrow(top)) {
    cat(sprintf("   ★ %s (rating predicho: %.2f)\n", top$titulo[r], top$rating_predicho[r]))
  }
  cat("\n")

  return(invisible(predicciones))
}

# 5. Ejecutar recomendaciones para cada usuario


for (usuario in nombres_usuarios) {
  recomendar(usuario, top_n = 3)
  cat(paste(rep("-", 70), collapse = ""), "\n")
}


# 6. Resumen de estructuras de datos 

cat("\nRESUMEN DE ESTRUCTURAS DE DATOS UTILIZADAS \n\n")
cat("1. DATA FRAME   → Catálogo de películas (título, género, año, rating)\n")
cat("2. MATRIZ       → Ratings de usuarios × películas (filtrado colaborativo)\n")
cat("3. LISTA        → Perfiles de usuarios (anidada: géneros, ratings, favoritos)\n")
cat("4. ENVIRONMENT  → Caché hash map para similitudes precalculadas\n")
cat("5. VECTOR       → Ratings individuales, frecuencias de género\n")
cat("6. ÁRBOL (lista anidada recursiva) → Decisión de recomendación\n")
cat("\n")
cat("Algoritmos implementados:\n")
cat("  • Similitud de coseno (filtrado colaborativo usuario-usuario)\n")
cat("  • Árbol de decisión manual (filtrado basado en contenido)\n")
cat("  • Sistema híbrido: combina ambos enfoques\n")
