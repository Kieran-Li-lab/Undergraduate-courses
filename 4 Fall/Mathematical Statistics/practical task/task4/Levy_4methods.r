############################################################
## Levy(mu, c) sampling -- four methods
## M1: Normal transform X = mu + c / Z^2
## M2: Rejection with Pareto proposal
## M3: Rejection with Uniform proposal
## M4: Rejection with modified Cauchy proposal
############################################################

## -------------------------- 1. Global settings --------------------------
mu <- 1 # Location parameter of Levy distribution
c <- 4 # Scale parameter of Levy distribution
N <- 10000 # Target sample size
set.seed(123) # Random seed


## -------------------------- 2. Target pdf: Levy(mu, c) --------------------------
levy_pdf <- function(x, mu, c) {
  ifelse(
    x <= mu,
    0,
    # Levy pdf formula: sqrt(c/(2*pi)) * exp(-c/(2*(x-mu))) / (x-mu)^(3/2)
    sqrt(c / (2 * pi)) * exp(-c / (2 * (x - mu))) / ((x - mu) ^ (3/2))
  )
}


## -------------------------- 3. Method 1: Normal transform --------------------------
levy_normal_sampling <- function(mu, c, n) {
  # Standard normal sample
  Z <- rnorm(n, mean = 0, sd = 1)
  # Remove possible zeros (avoid division by 0)
  Z <- Z[Z != 0]
  # Transformation X = mu + c / Z^2
  X <- mu + c / (Z ^ 2)
  # Ensure the output vector has length n
  X[1:n]
}

X_m1 <- levy_normal_sampling(mu, c, N)
m1_mean <- mean(X_m1)
m1_var <- var(X_m1)


## -------------------------- 4. Method 2: Rejection with Pareto proposal -----------

## Pareto pdf: g(x) = a * xm^a / x^(a+1), x >= xm
pareto_pdf <- function(x, a, xm = 1) {
  ifelse(
    x < xm,
    0,
    (a * (xm ^ a)) / (x ^ (a + 1))
  )
}

## Choose shape a by minimizing M = max_x f(x)/g(x) on a grid
find_pareto_a <- function(mu, c, xm = 1) {
  objective <- function(a) {
    x_range <- seq(mu + 0.01, mu + 100, length.out = 1000)
    f_vals <- sapply(x_range, levy_pdf, mu = mu, c = c)
    g_vals <- sapply(x_range, pareto_pdf, a = a, xm = xm)
    max(f_vals / g_vals)
  }
  optimize(objective, interval = c(0.1, 5.0), maximum = FALSE)$minimum
}

a_hat <- find_pareto_a(mu, c)

rejection_pareto <- function(mu, c, a, xm = 1, n) {
  samples <- numeric(0)
  total_proposed <- 0L

  ## M = sup_x f(x)/g(x) approximated on a grid
  x_range <- seq(mu + 0.01, mu + 100, length.out = 1000)
  M <- max(sapply(
    x_range,
    function(x) levy_pdf(x, mu, c) / pareto_pdf(x, a, xm)
  ))

  while (length(samples) < n) {
    ## Inverse CDF of Pareto: X = xm / U^(1/a)
    u <- runif(1)
    x_prop <- xm / (u ^ (1 / a))
    total_proposed <- total_proposed + 1L

    num <- levy_pdf(x_prop, mu, c)
    den <- pareto_pdf(x_prop, a, xm)
    if (den <= 0) next

    alpha <- num / (M * den)
    if (runif(1) <= alpha) {
      samples <- c(samples, x_prop)
    }
  }

  list(
    x = samples,
    mean = mean(samples),
    var = var(samples),
    M = M,
    total_proposed = total_proposed,
    reject_rate = 1 - length(samples) / total_proposed
  )
}

res_m2 <- rejection_pareto(mu, c, a_hat, n = N)


## --------------- 5. Method 3: Rejection with Uniform proposal ---------------
#=========================== Modification Start: Uniform Proposal [1.1, 100] #===========================

## Uniform pdf: g(x) = 1/(b - a), x in [a, b].
a_unif <- 1.1 # Lower bound for uniform proposal: a > mu=1
b_unif <- 10 # Upper bound for uniform proposal

uniform_pdf <- function(x, a, b) {
  # Uniform density is constant within the support [a, b]
  ifelse(
    x < a | x > b,
    0,
    1 / (b - a)
  )
}

rejection_unif <- function(mu, c, a, b, n) {
  samples <- numeric(0)
  total_proposed <- 0L

  ## M = sup_x f(x)/g(x) approximated on a grid
  x_range <- seq(a, b, length.out = 1000)
  # M = (b-a) * max(f(x))
  M <- max(sapply(
    x_range,
    function(x) levy_pdf(x, mu, c) / uniform_pdf(x, a, b)
  ))

  while (length(samples) < n) {
    ## Sample from Uniform: X ~ U[a, b]
    x_prop <- runif(1, min = a, max = b)
    total_proposed <- total_proposed + 1L

    num <- levy_pdf(x_prop, mu, c)
    den <- uniform_pdf(x_prop, a, b)
    
    # den > 0 is guaranteed by sampling from U[a, b]
    alpha <- num / (M * den)
    
    if (runif(1) <= alpha) {
      samples <- c(samples, x_prop)
    }
  }

  list(
    x = samples,
    mean = mean(samples),
    var = var(samples),
    M = M,
    total_proposed = total_proposed,
    reject_rate = 1 - length(samples) / total_proposed
  )
}

res_m3 <- rejection_unif(mu, c, a_unif, b_unif, n = N)

#=========================== Modification End: Uniform Proposal [1.1, 100] #===========================


## --------------- 6. Method 4: Rejection with modified Cauchy proposal ----------

## Proposal pdf g(x) = 2 / (pi * (1 + (x-1)^2)), x >= 1; 0 otherwise
modified_cauchy_pdf <- function(x) {
  ifelse(
    x < 1,
    0,
    2 / (pi * (1 + (x - 1)^2))
  )
}

## Rejection sampler: proposal X = 1 + |C|, C ~ Cauchy(0,1)
rejection_cauchy <- function(mu, c, n) {
  samples <- numeric(0)
  total_proposed <- 0L

  ## M = sup_x f(x)/g(x) approximated on a grid
  x_range <- seq(mu + 0.01, mu + 100, length.out = 2000)
  ratio_fg <- sapply(
    x_range,
    function(x) levy_pdf(x, mu, c) / modified_cauchy_pdf(x)
  )
  M <- max(ratio_fg[is.finite(ratio_fg)])

  while (length(samples) < n) {
    # Sample from standard Cauchy, then transform
    cauchy_sample <- rcauchy(1, 0, 1)
    x_prop <- 1 + abs(cauchy_sample)
    total_proposed <- total_proposed + 1L

    num <- levy_pdf(x_prop, mu, c)
    den <- modified_cauchy_pdf(x_prop)
    if (den <= 0) next

    alpha <- num / (M * den)
    if (runif(1) <= alpha) {
      samples <- c(samples, x_prop)
    }
  }

  list(
    x = samples,
    mean = mean(samples),
    var = var(samples),
    M = M,
    total_proposed = total_proposed,
    reject_rate = 1 - length(samples) / total_proposed
  )
}

res_m4 <- rejection_cauchy(mu, c, n = N)


## -------------------------- 7. Summary output --------------------------

cat("=========== Summary over 4 methods (N =", N, ") ===========\n")

summary_table <- data.frame(
  Method = c("Normal transform",
            "Rejection - Pareto",
            "Rejection - Uniform", # Method 3 is now Uniform
            "Rejection - Mod. Cauchy"),
  EmpMean = round(c(m1_mean,
                    res_m2$mean,
                    res_m3$mean,
                    res_m4$mean), 4),
  EmpVar = round(c(m1_var,
                   res_m2$var,
                   res_m3$var,
                   res_m4$var), 4),
  RejectRate = c(NA,
                 round(res_m2$reject_rate, 4),
                 round(res_m3$reject_rate, 4),
                 round(res_m4$reject_rate, 4)),
  M = c(NA,
        round(res_m2$M, 4),
        round(res_m3$M, 4),
        round(res_m4$M, 4)),
  TotalProposed = c(N,
                    res_m2$total_proposed,
                    res_m3$total_proposed,
                    res_m4$total_proposed)
)

print(summary_table, row.names = FALSE)

cat("\nNote: theoretical mean and variance of the Levy distribution are +Inf,\n",
    "so the empirical moments are only finite-sample summaries.\n", sep = "")