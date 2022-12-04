# setwd("./Prácticafinal")
datos_prac <- read.csv("data.csv")
str(datos_prac)
library(psych)
cortest.bartlett(cor(datos_prac))
boxplot(datos_prac)

str(datos_prac)
datos_prac <- subset(datos_prac, select = -c(Net.Income.Flag))

PCA <- prcomp(datos_prac[, -1], center = TRUE, scale = TRUE )

PCA$rotation
plot(cumsum(PCA$sdev^2) / (sum(PCA$sdev^2)), type = "l") # check this
summary(PCA)


varianza_explicada <- PCA$sdev^2 / sum(PCA$sdev^2)
ggplot(
    data = data.frame(varianza_explicada),
    aes(x = 14, y = varianza_explicada, fill = varianza_explicada)
) +
    theme_bw() +
    labs(x = "Componente principal", y = " Proporcion de varianza explicada")

varianza_acum <- cumsum(varianza_explicada)

ggplot(
    data = data.frame(varianza_acum, pc = 1:11),
    aes(x = pc, y = varianza_acum, fill = varianza_acum)
) +
    geom_col(width = 0.5) +
    scale_y_continuous(limits = c(0, 1)) +
    theme_bw() +
    labs(
        x = "Componente principal",
        y = "Proporci?n varianza acumulada"
    )

mean(PCA$sdev^2)

