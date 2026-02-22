## ---------- Q1: Normal(1,3^2) + Laplace(2, b=1/3) ----------
set.seed(42)
n  <- 100
p1 <- 0.4; p2 <- 0.6

## Laplace helpers
dlap <- function(x, beta=2, b=1/3) (1/(2*b))*exp(-abs(x-beta)/b)
plap <- function(x, beta=2, b=1/3){
  y <- x-beta
  ifelse(y < 0, 0.5*exp(y/b), 1 - 0.5*exp(-y/b))
}
qlap <- function(u, beta=2, b=1/3){
  ifelse(u < 0.5, beta + b*log(2*u), beta - b*log(2*(1-u)))
}

## target pdf/cdf (for plotting)
pdf_target <- function(x) p1*dnorm(x,1,3) + p2*dlap(x,2,1/3)
cdf_target <- function(x) p1*pnorm(x,1,3) + p2*plap(x,2,1/3)

## theoretical moments
mu1 <- 1; var1 <- 3^2
mu2 <- 2; var2 <- 2*(1/3)^2
mu_th  <- p1*mu1 + p2*mu2
var_th <- p1*(var1 + mu1^2) + p2*(var2 + mu2^2) - mu_th^2

## ---------------- Method 1: Binomial counts -> sub-pdf采样 ----------------
t1 <- system.time({
  N1 <- rbinom(1, n, p1); N2 <- n - N1
  x1 <- c(rnorm(N1, 1, 3), qlap(runif(N2), 2, 1/3))
  m1 <- mean(x1); v1e <- var(x1)
})
cat(sprintf("Q1-Method1 counts: N1=%d, N2=%d\n", N1, N2))

## ---------------- Method 2: U ~ Unif[0,1] -> 直接分段反解 -----------------
t2 <- system.time({
  U  <- runif(n)
  x2 <- numeric(n)

  idx1 <- (U < p1)
  if (any(idx1)) {
    u1 <- U[idx1] / p1                 # [0,p1) -> [0,1]
    x2[idx1] <- qnorm(u1, mean=1, sd=3)
  }
  idx2 <- !idx1
  if (any(idx2)) {
    u2 <- (U[idx2] - p1) / p2          # [p1,1] -> [0,1]
    x2[idx2] <- qlap(u2, beta=2, b=1/3)
  }

  N1b <- sum(idx1); N2b <- sum(idx2)   # 仅用于报告，不参与采样逻辑
  m2 <- mean(x2); v2e <- var(x2)
})
cat(sprintf("Q1-Method2(U->Fi^-1) counts: N1=%d, N2=%d\n\n", N1b, N2b))

## ---------------- Print & plots --------------------------------------------
cat("=== Q1: Theoretical vs Empirical ===\n")
cat(sprintf("Theory : mean=%.6f  var=%.6f\n", mu_th, var_th))
cat(sprintf("Method1(counts->subpdf RNG): mean=%.6f  var=%.6f | time=%.3fs\n",
            m1, v1e, t1["elapsed"]))
cat(sprintf("Method2(U->Fi^-1)         : mean=%.6f  var=%.6f | time=%.3fs\n\n",
            m2, v2e, t2["elapsed"]))

png("q1_density_compare.png", 1600, 800, res=150)
par(mfrow=c(1,2), mar=c(4,4,3,1))
hist(x1, breaks="FD", freq=FALSE, col="lightgray", border="white",
     main="Q1 — Method 1: Density", xlab="x")
curve(pdf_target(x), add=TRUE, lwd=2)
hist(x2, breaks="FD", freq=FALSE, col="lightblue", border="white",
     main="Q1 — Method 2: Density", xlab="x")
curve(pdf_target(x), add=TRUE, lwd=2)
dev.off()

png("q1_cdf_compare.png", 1600, 800, res=150)
par(mfrow=c(1,2), mar=c(4,4,3,1))
plot(ecdf(x1), main="Q1 — Method 1: ECDF vs CDF", xlab="x", ylab="F", col="blue")
curve(cdf_target(x), add=TRUE, lwd=2, lty=2)
plot(ecdf(x2), main="Q1 — Method 2: ECDF vs CDF", xlab="x", ylab="F", col="blue")
curve(cdf_target(x), add=TRUE, lwd=2, lty=2)
dev.off()
