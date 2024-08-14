set.seed(131131)

wald_co_prob <- function(n, p, alpha) {
    z <- qnorm(1 - alpha / 2)
    k <- 0:n
    p_hat <- k / n
    condition <- sqrt(n) * abs(p_hat - p) / sqrt(p_hat * (1 - p_hat)) <= z
    # Sumowanie iteracji
    co <- sum(dbinom(k, n, p) * condition)
    return(co)
}


p <- 0.5
alpha <- 0.05
n_values <- 10:100
co_probs <- sapply(n_values, function(n) wald_co_prob(n, p, alpha))

result <- data.frame(n = n_values, p_n = round(co_probs, 3))
print(result)

plot(n_values, co_probs, type = "l",)
abline(h = 1 - alpha, col = "red", lty = 2)

library(binom)

wilson_co_prob <- function(n, p, alpha) {
    k <- 0:n
    int <- binom.wilson(k, n, conf.level = 1 - alpha)
    co <- sum(dbinom(k, n, p) * (int$lower <= p & p <= int$upper))
    return(co)
}

ac_co_prob <- function(n, p, alpha) {
    k <- 0:n
    int <- binom.confint(k, n, conf.level = 1 - alpha, method = "ac")
    co <- sum(dbinom(k, n, p) * (int$lower <= p & p <= int$upper))
    return(co)
}

lrt_co_prob <- function(n, p, alpha) {
    k <- 0:n
    int <- binom.lrt(k, n, conf.level = 1 - alpha)
    co <- sum(dbinom(k, n, p) * (int$lower <= p & p <= int$upper))
    return(co)
}

co_probs <- sapply(n_values, function(n) wilson_co_prob(n, p, alpha))
ac_co_probs <- sapply(n_values, function(n) ac_co_prob(n, p, alpha))
lrt_co_probs <- sapply(n_values, function(n) lrt_co_prob(n, p, alpha))

result <- data.frame(n = n_values, p_n = round(co_probs, 3))
result_ac <- data.frame(n = n_values, p_n = round(ac_co_probs, 3))
result_lrt <- data.frame(n = n_values, p_n = round(lrt_co_probs, 3))

# Wykres dla Wilsona
print(result)
plot(n_values, co_probs, type = "l")
abline(h = 1 - alpha, col = "red", lty = 2)

# Wykres dla Agresti-Coull
print(result_ac)
plot(n_values, ac_co_probs, type = "l")
abline(h = 1 - alpha, col = "red", lty = 2)

# Wykres dla ilorazu wiarygodnoœci
plot(n_values, lrt_co_probs, type = "l",)
abline(h = 1 - alpha, col = "red", lty = 2)
print(result_lrt)
