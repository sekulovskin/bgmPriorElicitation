# function to estimate beta-binomial parameters

estimate_beta_binomial <- function(x, n, method = c("mle", "mom"), force_mom = FALSE) {
  method <- match.arg(method)
  if (any(x < 0) || any(x > n)) {
    stop("All values of x must be between 0 and n.")
  }
  
  if (method == "mom") {
    m <- mean(x)
    v <- var(x)
    mu <- m / n
    
    numerator <- mu * (1 - mu) * n - v
    denominator <- v - mu * (1 - mu)
    
    if (denominator <= 0 || numerator <= 0) {
      warning("Invalid MoM estimates (possibly due to low variance). Falling back to MLE.")
      method <- "mle"
    } else {
      alpha <- (mu * numerator) / denominator
      beta <- ((1 - mu) * numerator) / denominator
      
      if ((alpha < 0.5 || beta < 0.5) && !force_mom) {
        warning("MoM estimates are small or unreliable. Falling back to MLE.")
        method <- "mle"
      } else {
        return(list(method = "mom", alpha = alpha, beta = beta))
      }
    }
  }
  
  if (method == "mle") {
    nll <- function(par) {
      alpha <- exp(par[1])
      beta <- exp(par[2])
      -sum(VGAM::dbetabinom.ab(x, size = n, shape1 = alpha, shape2 = beta, log = TRUE))
    }
    
    init <- log(c(mean(x) + 1, n - mean(x) + 1))  # reasonable starting point
    fit <- optim(init, nll, method = "L-BFGS-B")
    alpha_mle <- exp(fit$par[1])
    beta_mle  <- exp(fit$par[2])
    
    return(list(method = method, alpha = alpha_mle, beta = beta_mle))
  }
}


# function from Giuseppe: 

#estimate_beta_binomial <- function(x,MLE=TRUE){
#  mean_x <- mean(x)
#  var_x <- var(x)
#  mm_x <- ((mean_x*(1-mean_x))/var_x)-1
#  alpha <- mean_x*mm_x
#  beta <- (1-mean_x)*mm_x
#  
#  beta_deriv <- function(pars,data){
#    m <- length(data)
#    sum_log_x <- sum(log(data))
#    sum_log_1_x <- sum(log(1-data))
#    alpha <- pars[1]
#    beta <- pars[2]
#    
#    # value of the loglikelihood calculated at pars
#    value <- (alpha-1)*sum_log_x + (beta-1)*sum_log_1_x-m*lbeta(alpha,beta)
#    
#    # digamma and trigamma functions used in first and second derivatives
#    psi_alpha <- digamma(alpha)
#    psi_beta <- digamma(beta)
#    psi_ab <- digamma(alpha + beta)
#    trigamma_alpha <- trigamma(alpha)
#    trigamma_beta <- trigamma(beta)
#    trigamma_ab <- trigamma(alpha + beta)
#    
#    # Score function
#    gradient <- rep(0.0,2)
#    gradient[1] <- sum_log_x - m * (psi_alpha - psi_ab)
#    gradient[2] <- sum_log_1_x - m * (psi_beta - psi_ab)
#    
#    # Hessian matrix
#    hessian <- matrix(0.0,2,2)
#    hessian[1,1] <- -m * (trigamma_alpha - trigamma_ab)
#    hessian[2,2] <- -m * (trigamma_beta - trigamma_ab)
#    hessian[1,2] <- -m * (-trigamma_ab)
#    hessian[2,1] <- hessian[1,2]
#    
#    return(list(value = -value, gradient = -gradient, hessian = -hessian)) # return negative because negative loglikelihood
#  } 
#  
#  mle <- c(NA,NA)
#  if(MLE){
#    opt <- trust::trust(objfun = beta_deriv, parinit = c(alpha,beta), data = x, rinit = 0.1, rmax = 10.0) # starting at the MM
#    mle <- opt$argument
#  }
#  
#  return(rbind(c(alphaMM = alpha,betaMM = beta),c(alphaMLE = mle[1],betaMLE=mle[2])))
#}
#
#

# Calculate the beta-binomial parameters for the llm object
library(dplyr)
library(stringr)  # we will have to think about these dependencies when making
# this a package 

betaBinParameters <- function(llmobject, 
                              method = c("mle", "mom"), 
                              force_mom = FALSE) {
  
  # check the number of permutations and give a warning messahe
  if (length(unique(llmobject$raw_LLM$permutation)) > 10) { # we should discuss this 
    warning("Consider using more permutations, in order to be able to properly estimate the parameters of the Beta distribution")
  }
  
  p <- length(llmobject$arguments$variable_list)
  no_edges <- p*(p-1)/2
  
  # check the class of the llm object
  if (inherits(llmobject, "llmPriorElicit") ||
      inherits(llmobject, "llmPriorElicitSimple")) {
    df <- llmobject$raw_LLM
    sum_ones_vector <- df %>%
      group_by(iteration) %>%
      summarize(sum_ones = sum(ifelse(content == "I", 1, 0))) %>%
      pull(sum_ones)
    bb <- estimate_beta_binomial(sum_ones_vector, n = no_edges, method = "mom")
  }
  
  
  else{
    df <- llmobject$raw_LLM
    sum_ones_vector <- df %>%
      group_by(permutation) %>%
      filter(pair_order == max(pair_order)) %>%
      mutate(
        full_sequence = str_match(content, "FULL SEQUENCE: ([IE]+)")[,2],
        sum_ones = sum(str_count(full_sequence, "I"), na.rm = TRUE)
      ) %>%
      pull(sum_ones)
    bb <- estimate_beta_binomial(sum_ones_vector, n = no_edges, method = "mom")
  }
  return(list(
    method = bb$method,
    alpha = bb$alpha,
    beta = bb$beta
  ))
}  # end of function 


