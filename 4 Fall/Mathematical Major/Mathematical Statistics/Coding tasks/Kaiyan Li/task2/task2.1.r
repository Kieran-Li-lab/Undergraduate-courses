## Q1: mixture 0.4 * N(1, 3^2) + 0.6 * Laplace(2, b = 1/3)
set.seed(42)
n  <- 10000
p1 <- 0.4
p2 <- 0.6

## ---------- Laplace helpers ----------
dlap <- function(x, beta = 2, b = 1/3) (1/(2*b)) * exp(-abs(x - beta) / b)
plap <- function(x, beta = 2, b = 1/3){
  y <- x - beta
  ifelse(y < 0, 0.5 * exp(y/b), 1 - 0.5 * exp(-y/b))
}
qlap <- function(u, beta = 2, b = 1/3){
  ifelse(u < 0.5, beta + b * log(2*u), beta - b * log(2*(1-u)))
}

## ---------- sub-pdf samplers ----------
r_sub1 <- function(m) rnorm(m, mean = 1, sd = 3)            # Normal(1, 3^2)
r_sub2 <- function(m) qlap(runif(m), beta = 2, b = 1/3)     # Laplace(2, 1/3)

## ---------- target pdf / cdf ----------
pdf_target <- function(x) p1*dnorm(x,1,3) + p2*dlap(x,2,1/3)
cdf_target <- function(x) p1*pnorm(x,1,3) + p2*plap(x,2,1/3)

## ---------- theoretical moments ----------
mu1 <- 1; var1 <- 3^2
mu2 <- 2; var2 <- 2*(1/3)^2
mu_th  <- p1*mu1 + p2*mu2
var_th <- p1*(var1 + mu1^2) + p2*(var2 + mu2^2) - mu_th^2

## =====================================================================
## Method 1: per-sample categorical classification (sample)
## =====================================================================
t1 <- system.time({
  ind1 <- sample(1:2, size = n, replace = TRUE, prob = c(p1, p2))
  n1   <- sum(ind1 == 1)
  n2   <- sum(ind1 == 2)

  x1 <- numeric(n)
  x1[ind1 == 1] <- r_sub1(n1)   # uses fresh runif inside r_sub1/r_sub2
  x1[ind1 == 2] <- r_sub2(n2)

  m1  <- mean(x1)
  v1e <- var(x1)
})
cat(sprintf("Q1-Method1(per-sample cat): N1=%d, N2=%d, time=%.3fs\n", n1, n2, t1["elapsed"]))

## =====================================================================
## Method 2: U ~ Unif[0,1], threshold + reuse U in conditional inverse
## =====================================================================
t2 <- system.time({
  U  <- runif(n)
  x2 <- numeric(n)

  idx1 <- (U < p1)
  idx2 <- !idx1

  if (any(idx1)) {
    u1 <- U[idx1] / p1                     # map [0, p1) -> [0,1]
    x2[idx1] <- qnorm(u1, mean = 1, sd = 3)
  }
  if (any(idx2)) {
    u2 <- (U[idx2] - p1) / p2              # map [p1, 1] -> [0,1]
    x2[idx2] <- qlap(u2, beta = 2, b = 1/3)
  }

  n1b <- sum(idx1); n2b <- sum(idx2)
  m2  <- mean(x2)
  v2e <- var(x2)
})
cat(sprintf("Q1-Method2(U->Fi^-1): N1=%d, N2=%d, time=%.3fs\n\n", n1b, n2b, t2["elapsed"]))

## ---------- print comparison ----------
cat("=== Q1: Theoretical vs Empirical ===\n")
cat(sprintf("Theory : mean=%.6f  var=%.6f\n", mu_th, var_th))
cat(sprintf("Method1: mean=%.6f  var=%.6f\n", m1, v1e))
cat(sprintf("Method2: mean=%.6f  var=%.6f\n\n", m2, v2e))

## ---------- plots ----------
png("q1_density_compare.png", 1600, 800, res = 150)
par(mfrow = c(1,2), mar = c(4,4,3,1))
hist(x1, breaks = "FD", freq = FALSE, col = "grey85", border = "white",
     main = "Q1 — Method 1: Density", xlab = "x")
curve(pdf_target(x), add = TRUE, lwd = 2)

hist(x2, breaks = "FD", freq = FALSE, col = "lightblue", border = "white",
     main = "Q1 — Method 2: Density", xlab = "x")
curve(pdf_target(x), add = TRUE, lwd = 2)
dev.off()

png("q1_cdf_compare.png", 1600, 800, res = 150)
par(mfrow = c(1,2), mar = c(4,4,3,1))
plot(ecdf(x1), main = "Q1 — Method 1: ECDF vs CDF", xlab = "x", ylab = "F", col = "blue")
curve(cdf_target(x), add = TRUE, lwd = 2, lty = 2)

plot(ecdf(x2), main = "Q1 — Method 2: ECDF vs CDF", xlab = "x", ylab = "F", col = "blue")
curve(cdf_target(x), add = TRUE, lwd = 2, lty = 2)
dev.off()
