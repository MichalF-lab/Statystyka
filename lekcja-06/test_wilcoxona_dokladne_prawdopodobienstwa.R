library(gtools)

calculate_W2 <- function(m, n, co) {
    N <- m + n
    W <- sum(co)
    W2 <- (W - 0.5 * n * (N + 1)) / sqrt(m * n * (N + 1) / 12)
    return(W2)
}

calculate_prob <- function(m, n, alpha) {
    N <- m + n
    combi <- combinations(N, n)
    # Dla ka¿dego wiersza wy³owaj calculate_W2
    W2s <- apply(combi, 1, function(comb) calculate_W2(m, n, comb))
    z_alpha <- qnorm(1 - alpha)
    prob <- mean(W2s >= z_alpha)
    return(prob)
}

# Obliczenia dla ró¿nych (m, n) i alpha
mn <- list(c(5, 5), c(8, 5), c(10, 5))
alphas <- c(0.1, 0.05, 0.01, 0.005)

ext <- matrix(nrow = length(mn), ncol = length(alphas))

for (i in 1:length(mn)) {
    m <- mn[[i]][1]
    n <- mn[[i]][2]
    for (j in 1:length(alphas)) {
        ext[i, j] <- calculate_prob(m, n, alphas[j])
    }
}

# Wyœwietlenie wyników
rownames(ext) <- paste("(", sapply(mn, paste, collapse = ","), ")", sep = "")
colnames(ext) <- paste("alpha =", alphas)
print(ext)



calculate_alpha0 <- function(m, n, w) {
    N <- m + n
    combi <- combinations(N, n)
    W_values <- rowSums(combi)
    prob <- mean(W_values >= w)
    return(prob)
}

calculate_alpha1 <- function(m, n, w) {
    N <- m + n
    W2 <- (w - 0.5 * n * (N + 1)) / sqrt(m * n * (N + 1) / 12)
    prob <- 1 - pnorm(W2)
    return(prob)
}

calculate_alpha2 <- function(m, n, w) {
    N <- m + n
    W2 <- (w - 0.5 - 0.5 * n * (N + 1)) / sqrt(m * n * (N + 1) / 12)
    prob <- 1 - pnorm(W2)
    return(prob)
}

# Obliczenia dla m = 6, n = 3
m <- 6
n <- 3
w_values <- c(9, 12, 15, 18, 21)

ext1 <- matrix(nrow = 3, ncol = length(w_values))

for (i in 1:length(w_values)) {
    ext1[1, i] <- calculate_alpha0(m, n, w_values[i])
    ext1[2, i] <- calculate_alpha1(m, n, w_values[i])
    ext1[3, i] <- calculate_alpha2(m, n, w_values[i])
}

# Obliczenia dla m = 6, n = 6
m <- 6
n <- 6
w_values <- c(27, 33, 39, 45, 51)

ext2 <- matrix(nrow = 3, ncol = length(w_values))
for (i in 1:length(w_values)) {
    ext2[1, i] <- calculate_alpha0(m, n, w_values[i])
    ext2[2, i] <- calculate_alpha1(m, n, w_values[i])
    ext2[3, i] <- calculate_alpha2(m, n, w_values[i])
}

# Wyœwietlenie wyników
rownames(ext1) <- c("alpha0", "alpha1", "alpha2")
colnames(ext1) <- paste("w =", w_values)
print("m = 6, n = 3")
print(ext1)

rownames(ext2) <- c("alpha0", "alpha1", "alpha2")
colnames(ext2) <- paste("w =", w_values)
print("m = 6, n = 6")
print(ext2)