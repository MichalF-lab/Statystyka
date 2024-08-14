# Imoprt
data <- read.table('lab1_dane.txt',dec = ",", header = T)

# Zad 1
catetegory_count <- table(data$typ)
typ_opis = c('kierownik', 'sprzedawca / marketing', 'urzednik', 'obsluga', 'wolny', 'inne')
pie(catetegory_count, labels = paste0(round(catetegory_count / 5.36), "%", names(catetegory_count), "."))
legend("topleft", legend = paste(names(catetegory_count), typ_opis), bty = "n")

# Zad 2
data$wyksztalcenie <- cut(data$edu, breaks = c(0, 8, 12, 100), labels = c(1, 2, 3), include.lowest = TRUE)
data$pensja <- as.numeric(data$zarobki) * 8 * 22 * 3.98

# Zad 3
srednia <- mean(data$pensja)
mediana <- median(data$pensja)

kwartyl1 <- quantile(data$pensja, 0.25)
kwartyl3 <- quantile(data$pensja, 0.75)

rozstep_miedzykwartylowy <- IQR(data$pensja)

wariancja <- var(data$pensja)
odchylenie_standardowe <- sd(data$pensja)

min_wartosc <- min(data$pensja)
max_wartosc <- max(data$pensja)

# Zad 4
# Podstawowe
Podstawowe <- list(
    wyksztalcenie <- subset(data$pensja,  data$wyksztalcenie == 1),

    jsrednia = mean(wyksztalcenie),
    mediana = median(wyksztalcenie),

    kwartyl1 = quantile(wyksztalcenie, 0.25),
    kwartyl3 = quantile(wyksztalcenie, 0.75),

    rozstep_miedzykwartylowy = IQR(wyksztalcenie),

    wariancja = var(wyksztalcenie),
    odchylenie_standardowe = sd(wyksztalcenie),

    min_wartosc = min(wyksztalcenie),
    max_wartosc = max(wyksztalcenie))

# Œrednie
Srednie <- list(
    wyksztalcenie <- subset(data$pensja, data$wyksztalcenie == 2),

    jsrednia = mean(wyksztalcenie),
    mediana = median(wyksztalcenie),

    kwartyl1 = quantile(wyksztalcenie, 0.25),
    kwartyl3 = quantile(wyksztalcenie, 0.75),

    rozstep_miedzykwartylowy = IQR(wyksztalcenie),

    wariancja = var(wyksztalcenie),
    odchylenie_standardowe = sd(wyksztalcenie),

    min_wartosc = min(wyksztalcenie),
    max_wartosc = max(wyksztalcenie))


# Wy¿sze
Wyzsze <- list(
    wyksztalcenie <- subset(data$pensja, data$wyksztalcenie == 3),

    jsrednia = mean(wyksztalcenie),
    mediana = median(wyksztalcenie),

    kwartyl1 = quantile(wyksztalcenie, 0.25),
    kwartyl3 = quantile(wyksztalcenie, 0.75),

    rozstep_miedzykwartylowy = IQR(wyksztalcenie),

    wariancja = var(wyksztalcenie),
    odchylenie_standardowe = sd(wyksztalcenie),

    min_wartosc = min(wyksztalcenie),
    max_wartosc = max(wyksztalcenie))



boxplot(subset(data$pensja, data$wyksztalcenie == 1),
        subset(data$pensja, data$wyksztalcenie == 2),
        subset(data$pensja, data$wyksztalcenie == 3),
        outline = TRUE)


lista <- list(Podstawowe, Srednie, Wyzsze)
tabela <- do.call(rbind, lista)
print(tabela)

# Zad 5 
# Wychodzi to samo
# hist(data$pensja, breaks = "Sturges", main = "Histogram pensji", xlab = "Pensja")

# Sta³e
x <- seq(min(data$pensja), max(data$pensja), length.out = 1000)
const <- 10 ^ 6 * 15/20

hist(data$pensja, breaks = "FD", main = "Histogram pensji FD", xlab = "Pensja")

# Gêstoœæ rozk³adu normalnego
lines(x, dnorm(x, mean = mean(data$pensja) - 2500, sd = sd(data$pensja))* const, col = "blue")

# Gêstoœæ rozk³adu jednostajnego
lines(x, dunif(x, min = min(data$pensja), max = max(data$pensja)) * const, col = "red")

# Gêstoœæ rozk³adu gamma
lines(x, dgamma(x+8000, shape = 13, scale = 1000) * const, col = "green")

# Zad 6
Tab <- table(rasa = data$rasa, wyksztacenie = data$wyksztalcenie)
dimnames(Tab) <- list(rasa = c("biala", "Ameryka centralna / poludniowa", "inna"), wyksztalcenie = c("Podstawowe", "Srednie", "Wyzsze"))
print(Tab)

summary(data$pensja)