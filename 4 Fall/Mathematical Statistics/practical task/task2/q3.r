## ---------- Q3: three segments ----------
## f(x) = (x+1)/8 on [-1,1), 1/4 on [1,2), 0.5 e^{-(x-2)} on [2,inf)
set.seed(42)
n <- 100
p1 <- 1/4; p2 <- 1/4; p3 <- 1/2

## full pdf/cdf (for plotting)
pdf_full <- function(x){
  y <- numeric(length(x))
  y[x >= -1 & x < 1] <- (x[x >= -1 & x < 1] + 1)/8
  y[x >= 1  & x < 2] <- 1/4
  y[x >= 2]          <- 0.5*exp(-(x[x >= 2]-2))
  y
}
cdf_full <- function(x){
  res <- numeric(length(x))
  res[x < -1] <- 0
  s1 <- x >= -1 & x < 1; res[s1] <- ((x[s1] + 1)^2)/16
  s2 <- x >= 1  & x < 2; res[s2] <- x[s2]/4
  s3 <- x >= 2;          res[s3] <- 1 - 0.5*exp(-(x[s3]-2))
  res
}

## conditional inverses for sub-pdfs
q1_cond <- function(u) -1 + 2*sqrt(u)   # (x+1)/2 on [-1,1)
q2_cond <- function(u) 1 + u            # Uniform[1,2)
q3_cond <- function(u) 2 - log(1 - u)   # e^{-(x-2)} on [2,inf)

## theoretical moments via conditional expectations
mu1 <- 1/3; var1 <- 2/9
mu2 <- 1.5; var2 <- 1/12
mu3 <- 3;   var3 <- 1
mu_th  <- p1*mu1 + p2*mu2 + p3*mu3
var_th <- p1*(var1 + mu1^2) + p2*(var2 + mu2^2) + p3*(var3 + mu3^2) - mu_th^2

## ---------------- Method 1: Multinomial counts -> sub-pdf采样 ----------------
t1 <- system.time({
  Ns <- as.vector(rmultinom(1, n, prob=c(p1,p2,p3)))
  x1 <- c(q1_cond(runif(Ns[1])),
          q2_cond(runif(Ns[2])),
          q3_cond(runif(Ns[3])))
  m1 <- mean(x1); v1e <- var(x1)
})
cat(sprintf("Q3-Method1 counts: N1=%d, N2=%d, N3=%d\n", Ns[1], Ns[2], Ns[3]))

## ---------------- Method 2: U ~ Unif[0,1] -> 直接分段反解 -----------------
t2 <- system.time({
  U  <- runif(n)
  x2 <- numeric(n)

  idx1 <- (U < p1)
  if (any(idx1)) {
    u1 <- U[idx1] / p1                    # [0,p1)->[0,1]
    x2[idx1] <- q1_cond(u1)
  }

  idx2 <- (U >= p1 & U < p1 + p2)
  if (any(idx2)) {
    u2 <- (U[idx2] - p1) / p2             # [p1,p1+p2)->[0,1]
    x2[idx2] <- q2_cond(u2)
  }

  idx3 <- !idx1 & !idx2
  if (any(idx3)) {
    u3 <- (U[idx3] - (p1 + p2)) / p3      # [p1+p2,1]->[0,1]
    x2[idx3] <- q3_cond(u3)
  }

  N1b <- sum(idx1); N2b <- sum(idx2); N3b <- sum(idx3)  # 仅用于报告
  m2 <- mean(x2); v2e <- var(x2)
})
cat(sprintf("Q3-Method2(U->Fi^-1) counts: N1=%d, N2=%d, N3=%d\n\n", N1b, N2b, N3b))

## ---------------- Print & plots --------------------------------------------
cat("=== Q3: Theoretical vs Empirical ===\n")
cat(sprintf("Theory : mean=%.6f  var=%.6f\n", mu_th, var_th))
cat(sprintf("Method1(counts->subpdf RNG): mean=%.6f  var=%.6f | time=%.3fs\n",
            m1, v1e, t1["elapsed"]))
cat(sprintf("Method2(U->Fi^-1)         : mean=%.6f  var=%.6f | time=%.3fs\n\n",
            m2, v2e, t2["elapsed"]))

png("q3_density_compare.png", 1600, 800, res=150)
par(mfrow=c(1,2), mar=c(4,4,3,1))
hist(x1, breaks="FD", freq=FALSE, col="gray85", border="white",
     main="Q3 — Method 1: Density", xlab="x")
curve(pdf_full(x), add=TRUE, lwd=2)
hist(x2, breaks="FD", freq=FALSE, col="lightblue", border="white",
     main="Q3 — Method 2: Density", xlab="x")
curve(pdf_full(x), add=TRUE, lwd=2)
dev.off()

png("q3_cdf_compare.png", 1600, 800, res=150)
par(mfrow=c(1,2), mar=c(4,4,3,1))
plot(ecdf(x1), main="Q3 — Method 1: ECDF vs CDF", xlab="x", ylab="F", col="blue")
curve(cdf_full(x), add=TRUE, lwd=2, lty=2)
plot(ecdf(x2), main="Q3 — Method 2: ECDF vs CDF", xlab="x", ylab="F", col="blue")
curve(cdf_full(x), add=TRUE, lwd=2, lty=2)
dev.off()
