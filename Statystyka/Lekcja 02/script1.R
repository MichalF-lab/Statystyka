# Imoprt

data <- read.csv('lab2.csv', sep = ",", dec = ",", header = T, stringsAsFactors = FALSE)
data$X1 <- as.numeric(data$X1)
data$X2 <- as.numeric(data$X2)
data$X3 <- as.numeric(data$X3)
data$X4 <- as.numeric(data$X4)

# Zad 1
hist(data$X1, freq = FALSE)
lines(seq(-3, 3, 0.01), dnorm(seq(-3, 3, 0.01)))

hist(data$X2, freq = FALSE)
lines(seq(0, 3, 0.01), dexp(seq(0, 3, 0.01),1.2))

hist(data$X3, freq = FALSE)
lines(seq(0, 6, 0.01), dgamma(seq(0, 6, 0.01),2))

hist(data$X4, freq = FALSE)
lines(seq(0, 1, 0.001), dnorm(seq(0, 1, 0.001), 0.45, 0.15))

# Zad 2
datan <- rnorm(101,1,4)
datae <- rexp(101,2)
datab <- rbeta(101,1,1)

# Oblicz kwantyle
quantiles <- quantile(datan, probs = c(0, 0.25, 0.5, 0.75, 1))

qqnorm(datan)
qqnorm(datae)
qqnorm(datab)


# Zad 3
n = 200
# Próby z rozk³adów normalnych
data_N01 <- rnorm(n, 0, 1)
data_N01_pom <- rnorm(n*100, 0, 1)
data_N022 <- rnorm(n, 0, 2.2)
data_N11 <- rnorm(n, 1, 1)
data_N133 <- rnorm(n, 1, 3.3)


boxplot(data_N01, data_N022, data_N11, data_N133)

quantiles <- quantile(data_N01, probs = c(0, 0.25, 0.5, 0.75, 1))
print(quantiles[1])
