# JuanCarlosCorreroMalia_Trabajo2.R
# Trabajo final Bioinformática - Curso 25/26
# Análisis de parámetros biomédicos por tratamiento
 
# 1. Cargar librerías (si necesarias) y datos del archivo "datos_biomed.csv". (0.5 pts)
datos <- read.csv("datos_biomed.csv", header = TRUE, sep = ",")
if(!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)
# Leemos el archivo "datos_biomed.csv" con la función read.csv() y lo guardamos en una variable. 
# Aseguramos que tenemos la librería "ggplot2" o, en el caso de no tenerla, la instalamos.
# Esta librería permite trabajar con dataframes, y facilita la realización de gráficos de mejor calidad.  
# 2. Exploración inicial con las funciones head(), summary(), dim() y str(). ¿Cuántas variables hay? ¿Cuántos tratamientos? (0.5 pts)
head(datos)
summary(datos)
dim(datos)
str(datos)
numerovariables <- ncol(datos)
numerotratamientos <- length(unique(datos$Tratamiento))
cat("El numero de variables es", numerovariables, "\n")
cat("El numero de tratamientos es", numerotratamientos, "\n")
# Se usan las funciones citadas, cada una con diferente propósito: head(enseña las 6 prímeras filas del dataframe), summary(estadísticas básicas por cada variable), dim(aporta las dimensiones del dataset, número de columnas y de filas) y str(la estructura, es decir, los tipos de datos)
# Con la función ncol(datos) obtenemos el  número de variables, mientras que con length(unique(datos$Tratamiento)) obtenemos el número de tratamientos. 
# 3. Una gráfica que incluya todos los boxplots por tratamiento. (1 pt)
par(mfrow = c(1,3))
boxplot(Glucosa ~ Tratamiento, data = datos,
        main = "Boxplot de glucosa por tratamiento",
        col = rainbow(numerotratamientos))
boxplot(Presion ~ Tratamiento, data = datos,
        main = "Boxplot de presión por tratamiento",
        col = rainbow(numerotratamientos))
boxplot(Colesterol ~ Tratamiento, data = datos,
        main = "Boxplot de colesterol por tratamiento",
        col = rainbow(numerotratamientos))
par(mfrow = c(1,1))
# Generamos diferentes boxplot, para cada variable. En los boxplot se observa la mediana, los cuartiles y posibles outliers de cada grupo. 
# Utilizamos la función boxplot, donde en "y" colocamos la variable que queremos medir y en "x" los diferentes tratamientos. 
# 4. Realiza un violin plot (investiga qué es). (1 pt)
ggplot(datos, aes(x = Tratamiento, y = Glucosa, fill = Tratamiento)) + geom_violin(trim = FALSE, color = "black") + geom_boxplot(width = 0.1, color = "gray80", fill = "white") + labs(title = "Violin plot de la glucosa por cada tratamiento", x = "Tratamiento", y = "Glucosa")
ggplot(datos, aes(x = Tratamiento, y = Presion, fill = Tratamiento)) + geom_violin(trim = FALSE, color = "black") + geom_boxplot(width = 0.1, color = "gray80", fill = "white") + labs(title = "Violin plot de la presión por cada tratamiento", x = "Tratamiento", y = "Presión")
ggplot(datos, aes(x = Tratamiento, y = Colesterol, fill = Tratamiento)) + geom_violin(trim = FALSE, color = "black") + geom_boxplot(width = 0.1, color = "gray80", fill = "white") + labs(title = "Violin plot de la colesterol por cada tratamiento", x = "Tratamiento", y = "Colesterol")
# Por medio de la libreria ggplot2, realizamos de cada variable un violin plot con la función geom_violin(). 
# En este tipo de gráficas se presenta la distribución de los datos de cada variable cuantitativa, en nuestro caso, colesterol, presión y glucosa, dependiendo de una variable categórica, en nuestro caso los tratamientos.
# 5. Realiza un gráfico de dispersión "Glucosa vs Presión". Emplea legend() para incluir una leyenda en la parte inferior derecha. (1 pt)
plot(datos$Glucosa, datos$Presion, col = as.numeric(as.factor(datos$Tratamiento)), pch = 20, xlab = "Glucosa", ylab = "Presión", main = "Glucosa vs Presión")
legend("bottomright", legend = unique(datos$Tratamiento),
       col = 1:numerotratamientos, pch = 15)
# Lo realizamos con la función plot(), y establecemos la leyenda por medio de la función legend(). 
# En concreto en la función plot establecemos glucosa en el eje x"" y presión en el eje  "y", y convertimos cada tratamiento a un color diferente, primero transformándolos a categoria, y luego a número. pch asigna la figura con la que se quiere representar los puntos en el gráfico de dispersión. 
# En la función legend, establecemos prímero la colocación, y luego los datos que utilizará para la leyenda, que serán los tratamientos.Por último, asignamos los colores y las figuras que utilizamos en la leyenda. He decido cambiar la forma en la leyenda porque me parecia más estético. 
# Este tipo de gráficos es muy útil para estudiar relaciones entre dos variables continuas.  
# 6. Realiza un facet Grid (investiga qué es): Colesterol vs Presión por tratamiento. (1 pt)
ggplot(datos, aes(x = Presion, y = Colesterol)) +
  geom_point(color = "orange") +
  facet_wrap(~ Tratamiento) +
  theme_minimal() +
  labs(title = "Colesterol vs Presión por tratamiento",x = "Presión",y = "Colesterol")
  par(mfrow = c(1,1))
# Un facet grid es un tipo de gráfico de dispersión donde se separan las variables cualitativas que se estan estudiando en diferentes gráficos, para poder estudiarlas separadamente sin que los puntos se superpongan entre si. 
# En nuestro caso, se forman 3 gráficos de dispersión diferentes, uno para cada tratamiento con sus respectivos puntos.
# Establecemos de donde se obtendran los datos y las variables, y en que ejes se colocarán por medio de ggplot(). 
# Con geom_point() añadimos los puntos, y les atribuimos el color que deseemos. 
# Con face_wrap(), dividimos los gráficos en paneles separados. 
# theme_minimal() es simplemente una función que atribuye el tema visual, aportando uno más simple y científico.
# Por último, labs() establece los títulos, tanto el general como el de cada eje.
# 7. Realiza un histogramas para cada variable. (0.5 pts)
par(mfrow = c(1,3))
hist(datos$Glucosa, main = "Histograma glucosa", col = "blue", xlab = "Glucosa")
hist(datos$Colesterol, main = "Histograma colesterol", col = "yellow", xlab = "Colesterol")
hist(datos$Presion,main = "Histograma presión", col = "red", xlab = "Presión")
par(mfrow = c(1,1))
# Generamos para cada diferente variable un histograma con la función hist(). 
# Utilizamos par() para cambiar los parametros gráficos globales, permitiendo que los 3 diferentes histogramas esten presentes en la misma imagen. 
# 8. Crea un factor a partir del tratamiento. Investiga factor(). (1 pt)
datos$Tratamiento <- as.factor(datos$Tratamiento)
str(datos)
levels(datos$Tratamiento)
# Generamos el factor, que es una variable categórica que representa diferentes grupos o categorias, por medio de la función as.factor().
# Con str() observamos la estructura del data frame, y vemos que tratamientto ya aparece como un factor. 
# levels() nos enseña las diferentes categorias que se han generado en el factor. En nuestro caso, FarmacoA, FarmacoB y Placebo. 
# 9. Obtén la media y desviación estándar de los niveles de glucosa por tratamiento. Emplea aggregate() o apply(). (0.5 pts)
media_glucosa <- tapply(datos$Glucosa, datos$Tratamiento, mean)
desv_glucosa <- tapply(datos$Glucosa, datos$Tratamiento, sd)
cat("Las medias de glucosa por tratamiento son ", media_glucosa, "\n")
cat("Las desviaciones estándar de glucosa por tratamiento son",desv_glucosa ,"\n")
# Se calcula la media y la desviación estándar de los niveles de glucosa para cada tratamiento utilizando la función tapply() y se guarda en respectivas variables.Asi, obtenemos un resumen estadístico de la glúcosa dentro de cada grupo experimental.
# Con cat() mostramos los diferentes resultados en la consola.  
# 10. Extrae los datos para cada tratamiento y almacenalos en una variable. Ejemplo todos los datos de Placebo en una variable llamada placebo. (1 pt)
farmacoa <- subset(datos, Tratamiento == "FarmacoA")
farmacob <- subset(datos, Tratamiento == "FarmacoB")
placebo  <- subset(datos, Tratamiento == "Placebo")
cat("Datos del tratamiento FarmacoA:\n")
head(farmacoa)
cat("Datos del tratamiento FarmacoB:\n")
head(farmacob)
cat("Datos del tratamiento Placebo:\n")
head(placebo)
# Usamos la función subset() para extraer las filas que cumplan con cada condición que pedimos, en este caso, cada diferente tratamiento. Guardamos esta información en sus respectivas variables de tratamiento. 
# Despues, con cat() imprimimos el enunciado y posteriormente con head() imprimimos los 6 prímeros valores para asegurnarnos que la función anterior ha funcionado como queriamos. 
# 11. Evalúa si los datos siguen una distribución normal y realiza una comparativa de medias acorde. (1 pt)
variablesum <- c("Glucosa", "Presion", "Colesterol")
# Definimos un vector con los diferentes grupos de variables.
for (var in variablesum) { cat("Variable:", var, "\n")
# Para que se repita el bucle por cada variable numérica, asi realizando los cálculos en cada variable.
  separacion <- split(na.omit(datos[[var]]), droplevels(datos$Tratamiento))
  k <- length(separacion)
# En este paso lo que hacemos es guardar en una variable los datos separados por tratamientos de cada variable. Utilizamos tambien na.omit() para eliminar, si los hubiese, valores vacios.
# k sera una variable que almacenara el número de tratamientos dentro del subgrupo de la variable.  
  pnorm <- sapply( separacion, function(sg) if (length(sg) >= 3) shapiro.test(sg)$p.value else NA_real_)
  cat("p-valores Shapiro-Wilk por tratamiento:",pnorm, "\n")}
# Creamos la variable pnorm, que aplicará con sapply() la función del test a cada subgrupo "sg", devolviendo los p-values. Pedimos tambien que la longuitud de los subgrupos sea 3 o mayor, ya que si no es así, no podriamos apliar el test de Shapiro. 
# Con la función shapiro.test() comprobamos si la función es normal o no lo es, por medio de la extracción del p-value de la tabla resultante de esta función. 
# Añadimos tambien a la variable que si no se cumplen las condiciones, se devuelva un NA de tipo numérico.  
# Los resultados del test muestras normalidad en todas las variables.
for (var in variablesum) { 
  cat("\n\n==============================================================\n")
  cat("variable:", var, "\n") 
  datosANOVA <- aov(formula(paste(var,"~Tratamiento")), data = datos)
  print(summary(datosANOVA))
# Creamos un bucle que busque cada variable en la agrupación de variables de estudio. 
# Creamos una variable, en la que se almacenarán los datos resultantes del ANOVA, que se realizará con la función aov(). Antes, se transforma la variable en una fórmula estadística que pueda utilizar. Paste() relaciona las tres variables con los tratamientos. 
# Ahora comprabamos la significancia de los datos en el ANOVA, que diremos son significativo si el p-value < 0.05. Si esto se cumple, se procederá a realizar la prueba post-hoc Tukey.
  PvANOVA <- summary(datosANOVA)[[1]][["Pr(>F)"]][1]
# En esta variable se extrae directamente el valor de los p-values de la tabla que te da como solución la función aov().
# Se accede a la primera tabla, se toma el primer valor,es decir el del factor tratamiento, se selecciona la columna del p-value y se devuelve una lista con esta información. 
  if (PvANOVA < 0.05) {
    cat("El ANOVA es significativo, se procede a realizar la prueba post-hoc de Tukey HSD:")
    print(TukeyHSD(datosANOVA))
  } else {
    cat("El ANOVA no es significativo, por lo tanto no se observan diferencias de medias.")
  }}
# Este bucle se encarga de comprobar si el resultado del ANOVA es significativo. En el caso de que lo sea, se procede a realizar una prueba de Tukey, que compara todas las parejas de tratamiento y ajusta los p-values para que asi no se produzcan falsos positivos. En el caso de que no sea significativo, se imprime un mensaje que lo comenta. 
# En conclusión, obtenemos que colesterol y presión son significativos, pero glucosa no lo es para ANOVA. 
# 12. Realiza un ANOVA sobre la glucosa para cada tratamiento. (1 pt)
anova_result <- aov(Glucosa ~ Tratamiento, data = datos)
summary(anova_result)
TukeyHSD(anova_result)
# Por último, realizamos un ANOVA de la glucosa y posteriormente un Tukey. 

