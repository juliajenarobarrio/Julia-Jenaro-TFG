# Se instalan y cargan las librerías necesarias
install.packages(c("readr", "dplyr", "ggplot2", "tidyverse", "data.table", "stringr", "textclean", "tm", "cld3"))

library(cld3)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(data.table)
library(stringr)
library(textclean)
library(tm)
library(readr)
library(stringr)


# Se cargan los datos que se van a utilizar desde un archivo CSV
df <- read_csv("/Users/juliajenarobarrio/Desktop/reddit_posts.csv")

#Se visualizan los datos en conjunto así como el contenido de las columnas actuales
View(df)
colnames(df)
df$Título
df$Fecha
df$URL
df$Texto
df$Subreddit
df$Autor

#Se verifico cuántas líneas tiene mi dataset
nrow(df)
#30904 comentarios

#Se eliminan aquellas columnas que no aportan datos relevantes para realizar el análisis: en este caso solo el URL
df$URL <- NULL
View(df)

# Se convierten las fechas al mismo formato
df$date <- as.Date(df$Fecha, format="%Y-%m-%d")
df$year <- format(df$Fecha, "%Y")
df$month <- format(df$Fecha, "%m")
df$day <- format(df$Fecha, "%d")

#Se realizan los principales análisis

#Se analiza en número de publicaciones por fecha 
number_reddit_byfecha <- df %>% 
  group_by(date) %>% 
  summarise(count = n())

#Se analiza en que fechas hay más comentarios
number_reddit_byfecha
print(number_reddit_byfecha, n = Inf)

#En los picos de febrero, se ven ejemplos de comentarios para entender por qué hay más en esas fechas
fechas_pico <- as.Date(c("2025-02-01", "2025-02-04", "2025-02-11"))

# Se filtran los comentarios válidos y se seleccionan los 3 primeros por fecha
comentarios_picos_limpios <- df %>%
  filter(date %in% fechas_pico, !is.na(Texto)) %>%
  group_by(date) %>%
  slice_head(n = 3) %>%
  select(date, Texto)
# Se ve el resultado para poder entender este aumento de comentarios
print(comentarios_picos_limpios, n = Inf)

# Se genera un gráfico de líneas para el número de publicaciones por fecha
ggplot(number_reddit_byfecha, aes(x = date, y = count)) + 
  geom_line(color = "steelblue", size = 1) + 
  geom_point(color = "steelblue") +  
  theme_minimal() + 
  labs(x = "Fecha", 
       y = "Número de Reddits", 
       title = "Número de Reddits por Fecha") + 
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5), 
    plot.title = element_text(hjust = 0.5)             
  )

# Se detecta el idioma de los comentarios en la columna 'Texto' y se guarda en una nueva columna llamada 'language'
df$language <- detect_language(df$Texto)

# Se visualiza la distribución de idiomas detectados para revisar la diversidad lingüística en los comentarios
number_reddits_bylang_cleaned <- df %>%
  filter(language != "und") %>%  # Se eliminan aquellos donde el idioma es 'und' es decir, no detectado
  group_by(language) %>%
  summarise(count = n()) %>%
  arrange(desc(count))  # Se ordena de forma descendente por el número de publicaciones

# Se imprimen los resultados para ver cuántos comentarios hay en cada idioma
print(number_reddits_bylang_cleaned)

# Se cogen solo los top 5 idiomas con más publicaciones
number_reddits_bylang_top5 <- head(number_reddits_bylang_cleaned, 5)

# Se define el diccionario de abreviaturas a nombres completos de los idiomas detectados
idiomas <- c(
  "en" = "Inglés",
  "ig" = "Igbo",
  "es" = "Español",
  "lb" = "Luxemburgués",
  "de" = "Alemán"
)

# Se aplica el mapeo de los idiomas en el DataFrame con los idiomas detectados
number_reddits_bylang_cleaned$language <- idiomas[number_reddits_bylang_cleaned$language]

# Se extrae un ejemplo (primer comentario encontrado) para cada idioma detectado
ejemplo_por_idioma <- df %>%
  group_by(language) %>%
  summarise(ejemplo = first(Texto))
# Se imprime el resultado
print(ejemplo_por_idioma)

# Se reemplazan códigos ISO por nombres completos (opcional, si quieres que diga "Inglés" y no "en")
number_reddits_bylang_top5 <- number_reddits_bylang_top5 %>%
  mutate(language_name = recode(language,
                                "en" = "Inglés",
                                "es" = "Español",
                                "ig" = "Igbo",
                                "lb" = "Luxemburgués",
                                "de" = "Alemán"))

# Se añade la columna para el color: rosa para 'Inglés' y azul para los demás
number_reddits_bylang_top5 <- number_reddits_bylang_top5 %>%
  mutate(color = ifelse(language_name == "Inglés", "pink", "steelblue"))


# Se genera el gráfico (en el documento del TFG)
ggplot(number_reddits_bylang_top5, aes(x = reorder(language_name, -count), y = count, fill = color)) +
  geom_col(show.legend = FALSE, width = 0.8) +
  scale_fill_identity() +
  theme_minimal(base_size = 10) +  # Base más pequeña
  labs(
    x = "Idioma",
    y = "Número de comentarios",
    title = "Número de comentarios por idioma"
  ) +
  theme(
    axis.text.x = element_text(size = 9, angle = 0, hjust = 0.5, face = "plain"),
    axis.text.y = element_text(size = 9, face = "plain"),
    axis.title.x = element_text(size = 10, face = "plain", margin = margin(t = 5)),
    axis.title.y = element_text(size = 10, face = "plain", margin = margin(r = 5)),
    plot.title = element_text(size = 12, face = "plain", hjust = 0.5, margin = margin(b = 8)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA)
  )



# Se calcula la longitud de cada comentario y se guarda en una nueva columna
df$texto_length <- nchar(df$Texto)

# Se extraen los bigotes inferior y superior para identificar los outliers en la longitud
boxplot_stats <- boxplot.stats(df$texto_length)

lower_whisker <- boxplot_stats$stats[1]
upper_whisker <- boxplot_stats$stats[5]

# Se filtran los datos sin outliers para la visualización
df_filtered <- df[df$texto_length >= lower_whisker & df$texto_length <= upper_whisker, ]

# Se genera el boxplot para visualizar la distribución de la longitud de los comentarios
ggplot(df_filtered, aes(x=factor(0), y=texto_length)) +
  geom_boxplot(outlier.shape = NA, coef=1.5) +  
  scale_y_continuous() +  
  labs(x="", y="Longitud del comentario", title="Distribución de longitud de comentarios") +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size=15),  
    axis.title.y = element_text(size=17), 
    plot.title = element_text(size=17, hjust=0.5)  # Título centrado
  )

# Se extraen los hashtags más utilizados en los comentarios
all_hashtags <- unlist(str_extract_all(df$Texto, "#\\w+"))

# Se crea un DataFrame con la frecuencia de los hashtags
freq_count <- as.data.frame(table(all_hashtags))
colnames(freq_count) <- c("Hashtag", "Frequency")
freq_count <- freq_count[order(-freq_count$Frequency),]

# Se seleccionan los top 20 hashtags más utilizados
top_hashtags <- head(freq_count, 20)

# Se genera el histograma con los hashtags más utilizados
ggplot(top_hashtags, aes(x=reorder(Hashtag, -Frequency), y=Frequency)) +
  geom_col(fill="steelblue") +
  labs(title="Principales Hashtags utilizados", x="Hashtags", y="Frecuencia") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle=90, hjust=1),  # Ajusta el texto del eje X para mejor visualización
    panel.grid.major = element_blank(),  # Elimina las líneas de la cuadrícula principales
    panel.grid.minor = element_blank(),  # Elimina las líneas de la cuadrícula menores
    panel.background = element_blank(),   # Elimina el fondo del panel
    plot.title = element_text(hjust=0.5)  # Centra el título
  )

# Se imprime el DataFrame de los top 20 hashtags para revisar
print(top_hashtags)

# Se filtran los hashtags que contienen al menos una letra (A-Z o a-z) para eliminar ruido o códigos raros
filtered_hashtags <- freq_count[grepl("[A-Za-z]", freq_count$Hashtag), ]

# Se seleccionan los top 20 hashtags después de aplicar el filtro
top_hashtags_filtered <- head(filtered_hashtags, 20)

# Se genera el histograma con los hashtags más utilizados (después de filtrar)
ggplot(top_hashtags_filtered, aes(x=reorder(Hashtag, -Frequency), y=Frequency)) +
  geom_col(fill="steelblue") +
  labs(title="Principales Hashtags utilizados", x="Hashtags", y="Frecuencia") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle=90, hjust=1),  # Ajusta el texto del eje X para mejor visualización
    panel.grid.major = element_blank(),  # Elimina las líneas de la cuadrícula principales
    panel.grid.minor = element_blank(),  # Elimina las líneas de la cuadrícula menores
    panel.background = element_blank(),  # Elimina el fondo del panel
    plot.title = element_text(hjust=0.5)  # Centra el título
  )

# Se imprime el DataFrame de los top 20 hashtags filtrados para volver revisar
print(top_hashtags_filtered)

# Se define una lista de hashtags irrelevantes o genéricos
hashtags_irrelevantes <- c("#The", "#Because", "#cite_note")

# Se filtran los hashtags para eliminar los irrelevantes
filtered_hashtags_clean <- filtered_hashtags[!filtered_hashtags$Hashtag %in% hashtags_irrelevantes, ]

# Se seleccionan los top 20 después de filtrar
top_hashtags_cleaned <- head(filtered_hashtags_clean, 20)

# Se generao el histograma con los hashtags filtrados
ggplot(top_hashtags_cleaned, aes(x=reorder(Hashtag, -Frequency), y=Frequency)) +
  geom_col(fill="steelblue") +
  labs(title="Principales Hashtags utilizados", x="Hashtags", y="Frecuencia") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle=90, hjust=1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.title = element_text(hjust=0.5)
  )

# Se cuenta la frecuencia de los subreddits
subreddit_freq <- as.data.frame(table(df$Subreddit))
colnames(subreddit_freq) <- c("Subreddit", "Frequency")

# Se ordena de mayor a menor frecuencia
subreddit_freq <- subreddit_freq[order(-subreddit_freq$Frequency), ]

# Se seleccionan los top 20 subreddits
top_subreddits <- head(subreddit_freq, 20)

# Se genera el histograma con los top 20 subreddits
ggplot(top_subreddits, aes(x=reorder(Subreddit, -Frequency), y=Frequency)) +
  geom_col(fill="steelblue") +
  labs(title="Top 20 Subreddits con más comentarios", x="Subreddit", y="Número de comentarios") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle=90, hjust=1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.title = element_text(hjust=0.5)
  )


# Se realiza el preprocesamiento de los datos 

# Se filtran solo los comentarios en inglés
df_english <- df[df$language == "en", ]

# Se realiza el preprocesamiento de los datos solo en los comentarios en inglés
clean <- function(x) {
  if (!(is.na(x))) {
    x <- gsub("http[s]?://\\S+", "", x)  # eliminar URLs
    x <- gsub("@\\w+", "", x)            # eliminar menciones
    x <- gsub("#\\w+", "", x)            # eliminar hashtags
    x <- gsub("\\d+", "", x)             # eliminar dígitos
    x <- iconv(x, "latin1", "ASCII", sub="")  # arreglar codificación
    x <- gsub('\\b\\w{1,2}\\b', '', x)  # eliminar palabras cortas
    x <- gsub("\\s+", " ", x)           # reemplazar espacios múltiples con uno solo
    x <- trimws(x)                      # eliminar espacios en blanco al principio y final
  }
  return(x)
}

# Se aplica la limpieza solo a los comentarios en inglés
df_english$cleaned_text <- sapply(df_english$Texto, clean)

# Se filtra el dataset para eliminar filas con 'cleaned_text' vacío o NA
df_english_filtered <- df_english[!(is.na(df_english$cleaned_text) | df_english$cleaned_text == ""), ]

# Se eliminan columnas que ya no son necesarias: en este caso, la columna original de texto
drop <- c("Texto")
df_english_filtered <- df_english_filtered[, !(names(df_english_filtered) %in% drop)]

# Se revisa el dataset filtrado
View(df_english_filtered)

# Se exporta el dataset limpio a un archivo CSV para los posteriores pasos en Python
write.csv2(df_english_filtered, "df_reddit_comentarios_en_limpios.csv", row.names = FALSE)














