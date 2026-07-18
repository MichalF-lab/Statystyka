set.seed(131131)

# Funkcja dla testu Ko³mogorowa-Smirnowa
ks_test <- function(x, F0) {
    Fn <- ecdf(x)
    ext <- max(abs(Fn(x) - F0(x)))
    return(ext)
}

# Funkcja dla testu Craméra-von Misesa
cvm_test <- function(x, F0) {
    n <- length(x)
    z <- F0(sort(x))
    ext <- sum((z - (2 * (1:n) - 1) / (2 * n)) ^ 2) + 1 / (12 * n)
    return(ext)
}

# Funkcja dla testu Andersona-Darlinga
ad_test <- function(x, F0) {
    n <- length(x)
    z <- F0(sort(x))
    ext <- -n - mean((2 * (1:n) - 1) * (log(z) + log(1 - rev(z))))
    return(ext)
}

# Standardowy rozk³ad normalny N(0,1)
generate_1 <- function(n) {
    return(rnorm(n, mean = 0, sd = 1))
}

# Rozk³ad normalny N(0,5^2)
generate_2 <- function(n) {
    return(rnorm(n, mean = 0, sd = 5))
}

# Mieszanka rozk³adów normalnych
generate_3 <- function(n, epsilon = 0.01, sigma = 100) {
    z <- rbinom(n, 1, 1 - epsilon)
    return(z * rnorm(n, 0, 1) + (1 - z) * rnorm(n, 0, sigma))
}

# Rozk³ad podwójnie wyk³adniczy
generate_4 <- function(n) {
    u <- runif(n)
    return(ifelse(u < 0.5, log(2 * u), - log(2 * (1 - u))))
}


# Sta³e
n <- 100
M <- 1000


# H0: N(0,1) H1: N(0,5)
rH0 <- 0
rH1 <- 0
test_func <- ks_test
crit <- 0.148
F0 <- function(x) pnorm(x, mean = 0, sd = 1)

for (i in 1:M) {
    x_H0 <- generate_1(n)
    testH0 <- test_func(x_H0, F0)
    statment0 <- (testH0 > crit)
    rH0 <- rH0 + statment0

    x_H1 <- generate_2(n)
    testH1 <- test_func(x_H1, F0)
    statment1 <- (testH1 > crit)
    rH1 <- rH1 + statment1
}
level <- rH0 / M
power <- rH1 / M

print(c(level, power))

