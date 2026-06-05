.h_s_eair_diff_scalar <- function(x1, 
                                  n1, 
                                  x2, 
                                  n2, 
                                  conf_type = c("wald", "waldcc", "mn", "scas"), 
                                  conf_level = 0.95) {
  conf_type <- match.arg(conf_type)
  diff_est <- switch(conf_type,
    waldcc = h_s_eair_diff_wald(x1, n1, x2, n2, conf_level, cc = TRUE),
    wald   = h_s_eair_diff_wald(x1, n1, x2, n2, conf_level, cc = FALSE),
    mn     = h_s_eair_diff_mn(x1, n1, x2, n2, conf_level, skew = FALSE),
    scas   = h_s_eair_diff_mn(x1, n1, x2, n2, conf_level, skew = TRUE)
  )
  list(diff_est = diff_est$est, diff_lcl = diff_est$ci[1], diff_ucl = diff_est$ci[2])
}

h_s_eair_diff <- function(x1,
                          n1,
                          x2,
                          n2,
                          conf_type,
                          conf_level = 0.95) {
  results <- mapply(
    .h_s_eair_diff_scalar,
    x1, n1, x2, n2,
    MoreArgs = list(conf_type = conf_type, conf_level = conf_level),
    SIMPLIFY = FALSE
  )
  data.frame(
    diff_est = sapply(results, `[[`, "diff_est"),
    diff_lcl = sapply(results, `[[`, "diff_lcl"),
    diff_ucl = sapply(results, `[[`, "diff_ucl")
  )
}

h_s_eair_diff_wald <- function(x1, n1, x2, n2, conf_level, cc = TRUE) {
  alpha <- 1 - conf_level
  est <- x1 / n1 - x2 / n2
  se <- sqrt(x1 / n1^2 + x2 / n2^2)

  coeff <- stats::qnorm(1 - alpha / 2)
  term2 <- coeff * se
  if (cc) {
    term2 <- term2 + 0.5 * (1/n1 + 1/n2)
  }
  lcl <- est - term2
  ucl <- est + term2

  eair_diff <- list()
  eair_diff$est <- est
  eair_diff$ci <- c(lcl, ucl)
  eair_diff
}

h_s_eair_diff_mn <- function(x1, n1, x2, n2, conf_level, skew = FALSE) {
  alpha <- 1 - conf_level
  z <- stats::qnorm(1 - alpha / 2)

  est <- x1 / n1 - x2 / n2

  lcl <- .conf_poi_rd(x1, n1, x2, n2, z, lower = TRUE, skew = skew, level = conf_level)
  ucl <- .conf_poi_rd(x1, n1, x2, n2, z, lower = FALSE, skew = skew, level = conf_level)

  eair_diff <- list()
  eair_diff$est <- est
  eair_diff$ci <- c(lcl, ucl)
  eair_diff
}

.score_poi_rd <- function(x1, n1, x2, n2, theta, skew = FALSE, level = 0.95) {
  alpha <- 1 - level
  p1hat <- x1 / n1
  p2hat <- x2 / n2
  Stheta <- (p1hat - p2hat) - theta
  if (isTRUE(abs(Stheta) == 0)) {
    return(0)
  }
  x <- x1 + x2
  N <- n1 + n2
  A <- N
  B <- N * theta - x
  C_ <- -x2 * theta
  num <- (-B + sqrt(B^2 - 4 * A * C_))
  p2d <- ifelse(isTRUE(num == 0), 0, num / (2 * A))
  p1d <- p2d + theta
  V <- p1d / n1 + p2d / n2
  score1 <- Stheta / sqrt(V)

  if (!skew) {
    return(score1)
  }

  # Skewness correction (SCAS)
  mu3 <- p1d / (n1^2) - p2d / (n2^2)
  scterm <- mu3 / (6 * V^(3 / 2))
  if (isTRUE(scterm == 0)) {
    return(score1)
  }

  A_q <- scterm
  B_q <- 1
  C_q <- -(score1 + scterm)
  dsct <- B_q^2 - 4 * A_q * C_q
  if (dsct < 0) {
    # Fallback to simplified skewness correction
    qtnorm <- stats::qnorm(1 - alpha / 2)
    score1 <- score1 - (qtnorm^2 - 1) * scterm
  } else {
    score1 <- (-B_q + sqrt(dsct)) / (2 * A_q)
  }

  return(score1)
}

.conf_poi_rd <- function(x1, n1, x2, n2, z, lower = FALSE,
                         skew = FALSE, level = 0.95) {
  p1 <- x1 / n1
  p2 <- x2 / n2
  p_hat <- p1 - p2
  dp <- 1 + ifelse(lower, 1, -1) * p_hat
  i <- 1
  while (i <= 50) {
    dp <- 0.5 * dp
    y <- p_hat + ifelse(lower, -1, 1) * dp
    score <- .score_poi_rd(x1, n1, x2, n2, y, skew = skew, level = level)
    # For lower bound: score decreases as y moves left, find where score = +z
    #   -> move y further left if score > z (still too close to estimate)
    # For upper bound: score decreases as y moves right, find where score = -z
    #   -> move y further right if score > -z (still too close to estimate)
    if (lower) {
      if (score < z) p_hat <- y
    } else {
      if (score > -z) p_hat <- y
    }
    if ((dp < 1e-07) || (abs(score - ifelse(lower, z, -z)) < 1e-06)) {
      break
    } else {
      i <- i + 1
    }
  }
  return(y)
}
