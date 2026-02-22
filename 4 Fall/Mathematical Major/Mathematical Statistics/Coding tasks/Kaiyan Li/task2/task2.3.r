## Q3: three segments
## f(x) = (x+1)/8 on [-1,1),
##      = 1/4      on [1,2),
##      = 0.5*exp(-(x-2)) on [2,inf)

set.seed(42)
n  <- 1000000
p1 <- 1/4
p2 <- 1/4
p3 <- 1/2

## ---------- full pdf / cdf ----------
pdf_full <- function(x){
  y <- numeric(length(x))
  y[x >= -1 & x < 1] <- (x[x >= -1 & x < 1] + 1)/8
  y[x >= 1  & x < 2] <- 1/4
  y[x >= 2]          <- 0.5 * exp(-(x[x >= 2] - 2))
  y
}
cdf_full <- function(x){
  res <- numeric(length(x))
  res[x < -1] <- 0
  s1 <- x >= -1 & x < 1; res[s1] <- ((x[s1] + 1)^2)/16
  s2 <- x >= 1  & x < 2; res[s2] <- x[s2]/4
  s3 <- x >= 2;          res[s3] <- 1 - 0.5*exp(-(x[s3] - 2))
  res
}

## ---------- conditional inverses for sub-pdfs ----------
## segment 1: f1_cond(x) = (x+1)/2 on [-1,1) => F(x) = ((x+1)^2)/4 => x = -1 + 2*sqrt(u)
q1_cond <- function(u) -1 + 2 * sqrt(u)
## segment 2: Uniform[1,2)
q2_cond <- function(u) 1 + u
## segment 3: exp(-(x-2)) on [2,inf) => x = 2 - log(1-u)
q3_cond <- function(u) 2 - log(1 - u)

r_sub1 <- function(m) q1_cond(runif(m))
r_sub2 <- function(m) q2_cond(runif(m))
r_sub3 <- function(m) q3_cond(runif(m))

## ---------- theoretical moments ----------
mu1 <- 1/3; var1 <- 2/9
mu2 <- 1.5; var2 <- 1/12
mu3 <- 3;   var3 <- 1
mu_th  <- p1*mu1 + p2*mu2 + p3*mu3
var_th <- p1*(var1 + mu1^2) + p2*(var2 + mu2^2) + p3*(var3 + mu3^2) - mu_th^2

## =====================================================================
## Method 1: per-sample categorical classification (sample 1:3)
## =====================================================================
t1 <- system.time({
  ind1 <- sample(1:3, size = n, replace = TRUE, prob = c(p1, p2, p3))
  n1   <- sum(ind1 == 1)
  n2   <- sum(ind1 == 2)
  n3   <- sum(ind1 == 3)

  x1 <- numeric(n)
  x1[ind1 == 1] <- r_sub1(n1)
  x1[ind1 == 2] <- r_sub2(n2)
  x1[ind1 == 3] <- r_sub3(n3)

  m1  <- mean(x1)
  v1e <- var(x1)
})
cat(sprintf("Q3-Method1(per-sample cat): N1=%d, N2=%d, N3=%d, time=%.3fs\n", n1, n2, n3, t1["elapsed"]))

## =====================================================================
## Method 2: U ~ Unif[0,1], threshold + reuse U in conditional inverse
## =====================================================================
t2 <- system.time({
  U  <- runif(n)
  x2 <- numeric(n)

  idx1 <- (U < p1)
  idx2 <- (U >= p1 & U < p1 + p2)
  idx3 <- (!idx1 & !idx2)

  if (any(idx1)) {
    u1 <- U[idx1] / p1                          # [0, p1) -> [0,1]
    x2[idx1] <- q1_cond(u1)
  }
  if (any(idx2)) {
    u2 <- (U[idx2] - p1) / p2                   # [p1, p1+p2) -> [0,1]
    x2[idx2] <- q2_cond(u2)
  }
  if (any(idx3)) {
    u3 <- (U[idx3] - (p1 + p2)) / p3            # [p1+p2, 1] -> [0,1]
    x2[idx3] <- q3_cond(u3)
  }

  n1b <- sum(idx1); n2b <- sum(idx2); n3b <- sum(idx3)
  m2  <- mean(x2)
  v2e <- var(x2)
})
cat(sprintf("Q3-Method2(U->Fi^-1): N1=%d, N2=%d, N3=%d, time=%.3fs\n\n", n1b, n2b, n3b, t2["elapsed"]))

## ---------- print comparison ----------
cat("=== Q3: Theoretical vs Empirical ===\n")
cat(sprintf("Theory : mean=%.6f  var=%.6f\n", mu_th, var_th))
cat(sprintf("Method1: mean=%.6f  var=%.6f\n", m1, v1e))
cat(sprintf("Method2: mean=%.6f  var=%.6f\n\n", m2, v2e))

## ---------- plots ----------
png("q3_density_compare.png", 1600, 800, res = 150)
par(mfrow = c(1,2), mar = c(4,4,3,1))
hist(x1, breaks = "FD", freq = FALSE, col = "grey85", border = "white",
     main = "Q3 — Method 1: Density", xlab = "x")
curve(pdf_full(x), add = TRUE, lwd = 2)

hist(x2, breaks = "FD", freq = FALSE, col = "lightblue", border = "white",
     main = "Q3 — Method 2: Density", xlab = "x")
curve(pdf_full(x), add = TRUE, lwd = 2)
dev.off()

png("q3_cdf_compare.png", 1600, 800, res = 150)
par(mfrow = c(1,2), mar = c(4,4,3,1))
plot(ecdf(x1), main = "Q3 — Method 1: ECDF vs CDF", xlab = "x", ylab = "F", col = "blue")
curve(cdf_full(x), add = TRUE, lwd = 2, lty = 2)

plot(ecdf(x2), main = "Q3 — Method 2: ECDF vs CDF", xlab = "x", ylab = "F", col = "blue")
curve(cdf_full(x), add = TRUE, lwd = 2, lty = 2)
dev.off()
