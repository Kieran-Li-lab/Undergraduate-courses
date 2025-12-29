## ---------- Q1: Normal(1,3^2) + Laplace(2, b=1/3); two methods with explicit counts ----------
set.seed(42)
n  <- 10000
w1 <- 0.4; w2 <- 0.6

## Laplace CDF/pdf
plap <- function(x, beta=2, b=1/3){
  y <- x - beta
  ifelse(y < 0, 0.5*exp(y/b), 1 - 0.5*exp(-y/b))
}
dlap <- function(x, beta=2, b=1/3) (1/(2*(1/3)))*exp(-abs(x-beta)/(1/3))

## Global target CDF & PDF
F_mix <- function(x) w1*pnorm(x, mean=1, sd=3) + w2*plap(x, beta=2, b=1/3)
f_mix <- function(x) w1*dnorm(x, mean=1, sd=3) + w2*dlap(x, beta=2, b=1/3)

## Global inverse CDF by root-finding
Finv <- function(u, lo=-100, hi=100, tol=1e-9){
  sapply(u, function(uu) uniroot(function(x) F_mix(x)-uu, c(lo,hi), tol=tol)$root)
}

## Theoretical moments (mixture)
mu1 <- 1;  v1 <- 3^2
mu2 <- 2;  v2 <- 2*(1/3)^2
mu_th  <- w1*mu1 + w2*mu2
var_th <- w1*(v1 + mu1^2) + w2*(v2 + mu2^2) - mu_th^2

## ---------------- Method 1: counts -> U in segment intervals -> Finv ----------------
t1 <- system.time({
  N1 <- rbinom(1, n, w1); N2 <- n - N1
  U  <- numeric(n)
  if (N1 > 0) U[1:N1]     <- runif(N1, 0, w1)  # segment 1: [0, w1)
  if (N2 > 0) U[(N1+1):n] <- runif(N2, w1, 1)  # segment 2: [w1, 1)
  x1 <- Finv(U)
  m1 <- mean(x1); v1e <- var(x1)
})
cat(sprintf("Q1-Method1 counts: N1=%d, N2=%d\n", N1, N2))

## ---------------- Method 2: global U -> explicit counts -> Finv ---------------------
t2 <- system.time({
  U2  <- runif(n)                       # draw once globally
  idx1 <- (U2 < w1); idx2 <- !idx1
  N1b <- sum(idx1); N2b <- sum(idx2)    # explicit counts
  x2  <- numeric(n)
  x2[idx1] <- Finv(U2[idx1])            # use the same U in its segment
  x2[idx2] <- Finv(U2[idx2])
  m2 <- mean(x2); v2e <- var(x2)
})
cat(sprintf("Q1-Method2 counts: N1=%d, N2=%d\n\n", N1b, N2b))

## ---------------- Print comparison ----------------
cat("=== Q1: Theoretical vs Empirical ===\n")
cat(sprintf("Theory : mean=%.6f  var=%.6f\n", mu_th, var_th))
cat(sprintf("Method1(counts->Useg->Finv): mean=%.6f  var=%.6f | time=%.3fs\n", m1, v1e, t1["elapsed"]))
cat(sprintf("Method2(U->counts->Finv)  : mean=%.6f  var=%.6f | time=%.3fs\n\n", m2, v2e, t2["elapsed"]))
cat(sprintf("RelErr-1: mean=%.3f%% var=%.3f%%\n", 100*abs(m1-mu_th)/mu_th, 100*abs(v1e-var_th)/var_th))
cat(sprintf("RelErr-2: mean=%.3f%% var=%.3f%%\n\n", 100*abs(m2-mu_th)/mu_th, 100*abs(v2e-var_th)/var_th))

## ---------------- Save plots ----------------
png("q1_density_compare.png", 1600, 800, res=150)
par(mfrow=c(1,2), mar=c(4,4,3,1))
hist(x1, breaks="FD", freq=FALSE, col="lightgray", border="white",
     main="Q1 — Method 1: Density", xlab="x"); curve(f_mix(x), add=TRUE, lwd=2)
hist(x2, breaks="FD", freq=FALSE, col="lightblue", border="white",
     main="Q1 — Method 2: Density", xlab="x"); curve(f_mix(x), add=TRUE, lwd=2)
dev.off()

png("q1_cdf_compare.png", 1600, 800, res=150)
par(mfrow=c(1,2), mar=c(4,4,3,1))
plot(ecdf(x1), main="Q1 — Method 1: ECDF vs CDF", xlab="x", ylab="F", col="blue")
curve(F_mix(x), add=TRUE, lwd=2, lty=2)
plot(ecdf(x2), main="Q1 — Method 2: ECDF vs CDF", xlab="x", ylab="F", col="blue")
curve(F_mix(x), add=TRUE, lwd=2, lty=2)
dev.off()
