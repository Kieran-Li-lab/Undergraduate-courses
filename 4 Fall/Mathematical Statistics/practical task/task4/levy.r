############################################################
## Levy(mu, c) sampling – 4 methods
## Method 1: Normal transform
## Method 2: Rejection with Pareto proposal
## Method 3: Rejection with shifted exponential proposal
## Method 4: Rejection with heavy–tailed Pareto proposal (self-designed)
############################################################

## -------------------------- 1. Global settings --------------------------
mu <- 1      # Levy location
c  <- 4      # Levy scale
N  <- 10000  # target sample size
set.seed(123)

## For numerical sup_x f/g, restrict to [mu+eps, Xmax]
eps  <- 0.01
Xmax <- 50


## -------------------------- 2. Levy pdf --------------------------
levy_pdf <- function(x, mu, c) {
  ifelse(
    x <= mu,
    0,
    sqrt(c / (2 * pi)) * exp(-c / (2 * (x - mu))) / ((x - mu)^(3/2))
  )
}


## -------------------------- 3. Method 1: Normal transform --------------------------
## X = mu + c / Z^2, Z ~ N(0,1)

levy_normal_sampling <- function(mu, c, n) {
  Z <- rnorm(n)
  ## avoid extremely small Z to prevent division overflow
  while (any(abs(Z) < 1e-10)) {
    idx <- which(abs(Z) < 1e-10)
    Z[idx] <- rnorm(length(idx))
  }
  X <- mu + c / (Z^2)
  X[1:n]
}

t1 <- system.time({
  x1 <- levy_normal_sampling(mu, c, N)
})

res1_mean <- mean(x1)
res1_var  <- var(x1)


## -------------------------- 4. Generic rejection sampler --------------------------
## f_pdf: target density f
## g_pdf: proposal density g (normalized)
## r_g  : sampler from g
## M    : constant s.t. f(x) <= M g(x) on [support_min, support_max]

rejection_sampler <- function(n, f_pdf, g_pdf, r_g,
                              M, support_min, support_max) {
  x_acc   <- numeric(n)
  k       <- 0L
  n_trial <- 0L

  t <- system.time({
    while (k < n) {
      x_prop <- r_g(1)
      n_trial <- n_trial + 1L

      if (x_prop <= support_min || x_prop >= support_max) next

      fx <- f_pdf(x_prop)
      gx <- g_pdf(x_prop)

      if (!is.finite(fx) || !is.finite(gx) || gx <= 0) next

      alpha <- fx / (M * gx)
      if (alpha > 1) alpha <- 1  # numerical guard

      if (runif(1) <= alpha) {
        k <- k + 1L
        x_acc[k] <- x_prop
      }
    }
  })

  acc_rate <- n / n_trial
  rej_rate <- 1 - acc_rate

  list(
    samples     = x_acc,
    accept_rate = acc_rate,
    reject_rate = rej_rate,
    time        = as.numeric(t["elapsed"]),
    mean        = mean(x_acc),
    var         = var(x_acc),
    total_trial = n_trial,
    M           = M
  )
}


## -------------------------- 5. Method 2: Pareto proposal --------------------------
## g2: Pareto(xm=1, shape=a2)

pareto_pdf <- function(x, a, xm = 1) {
  ifelse(
    x < xm,
    0,
    a * (xm^a) / (x^(a + 1))
  )
}

r_pareto <- function(n, a, xm = 1) {
  u <- runif(n)
  xm / (u^(1 / a))
}

a2 <- 2.0                      # moderate tail
g2_pdf <- function(x) pareto_pdf(x, a2, xm = 1)
r_g2   <- function(n) r_pareto(n, a2, xm = 1)

x_grid <- seq(mu + eps, Xmax, length.out = 2000)
f_vals <- levy_pdf(x_grid, mu, c)
g_vals <- g2_pdf(x_grid)
ratio  <- f_vals / g_vals
M2     <- max(ratio[is.finite(ratio) & ratio > 0])  # numerical sup_x f/g

res2 <- rejection_sampler(
  n           = N,
  f_pdf       = function(x) levy_pdf(x, mu, c),
  g_pdf       = g2_pdf,
  r_g         = r_g2,
  M           = M2,
  support_min = mu,
  support_max = Xmax
)


## -------------------------- 6. Method 3: shifted exponential proposal --------------------------
## g3(x) = b3 * exp(-b3 (x - a3)), x >= a3

a3 <- 1           # shift
b3 <- 0.02        # small rate to avoid too light tail on [1,50]

shifted_exp_pdf <- function(x, b, a) {
  ifelse(
    x < a,
    0,
    b * exp(-b * (x - a))
  )
}

r_shifted_exp <- function(n, b, a) {
  u <- runif(n)
  a - log(1 - u) / b
}

g3_pdf <- function(x) shifted_exp_pdf(x, b3, a3)
r_g3   <- function(n) r_shifted_exp(n, b3, a3)

x_grid <- seq(mu + eps, Xmax, length.out = 2000)
f_vals <- levy_pdf(x_grid, mu, c)
g_vals <- g3_pdf(x_grid)
ratio  <- f_vals / g_vals
M3     <- max(ratio[is.finite(ratio) & ratio > 0])  # exact numerical sup

res3 <- rejection_sampler(
  n           = N,
  f_pdf       = function(x) levy_pdf(x, mu, c),
  g_pdf       = g3_pdf,
  r_g         = r_g3,
  M           = M3,
  support_min = mu,
  support_max = Xmax
)


## -------------------------- 7. Method 4: heavy–tailed Pareto proposal --------------------------
## g4: Pareto(xm=1, shape=a4) with a4 <= 0.5  (tail at least as heavy as Levy)
a4 <- 0.5

g4_pdf <- function(x) pareto_pdf(x, a4, xm = 1)
r_g4   <- function(n) r_pareto(n, a4, xm = 1)

x_grid <- seq(mu + eps, Xmax, length.out = 2000)
f_vals <- levy_pdf(x_grid, mu, c)
g_vals <- g4_pdf(x_grid)
ratio  <- f_vals / g_vals
M4     <- max(ratio[is.finite(ratio) & ratio > 0])  # numerical sup

res4 <- rejection_sampler(
  n           = N,
  f_pdf       = function(x) levy_pdf(x, mu, c),
  g_pdf       = g4_pdf,
  r_g         = r_g4,
  M           = M4,
  support_min = mu,
  support_max = Xmax
)


## -------------------------- 8. Summary output --------------------------
cat("=== Theoretical (Levy) ===\n")
cat("Mean: +Inf  (does not exist)\n")
cat("Var : +Inf  (does not exist)\n\n")

summary_table <- data.frame(
  Method      = c("Method1: Normal",
                  "Method2: Pareto(a2)",
                  "Method3: ShiftedExp",
                  "Method4: HeavyPareto(a4)"),
  M_used      = round(c(NA, res2$M, res3$M, res4$M), 4),
  AcceptRate  = round(c(NA,
                        res2$accept_rate,
                        res3$accept_rate,
                        res4$accept_rate), 4),
  Time_sec    = round(c(t1["elapsed"],
                        res2$time,
                        res3$time,
                        res4$time), 4),
  Emp_Mean    = round(c(res1_mean,
                        res2$mean,
                        res3$mean,
                        res4$mean), 4),
  Emp_Var     = round(c(res1_var,
                        res2$var,
                        res3$var,
                        res4$var), 4)
)

cat("=== Comparison of 4 methods (N =", N, ") ===\n")
print(summary_table, row.names = FALSE)
