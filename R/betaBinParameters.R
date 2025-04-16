# function to estimate beta-binomial parameters

estimate_beta_binomial <- function(x, n, method = c("mle", "mom"), force_mom = FALSE) {
  if (any(x < 0) || any(x > n)) {
    stop("All values of x must be between 0 and n.")
  }
  
  # alpha and beta estimated using method of moments (mom)
  m1 <- mean(x)/n
  m2 <- mean(x^2)/n
  denominator <- (n*(m2/m1-m1-1)+m1)
  alpha <- (n*m1-m2)/denominator
  beta <- ((n-m1)*(n-m2/m1))/denominator
  init <- c(0,0)
  if (alpha <= 0 || beta <= 0) {
    warning("Invalid MoM estimates (possibly due to low variance). Falling back to MLE.")
    init <- log(c(mean(x) + 1, n - mean(x) + 1))  # reasonable starting point
  }
  else{
    init <- log(c(alpha,beta))
  }
  
  # negative loglikelihood
  beta_binom_fun <- function(pars,x,n){
    alpha <- exp(pars[1])
    beta <- exp(pars[2])
    
    # value of the loglikelihood calculated at pars (excluding log(choose(n,x))
    value <- sum(lgamma(x+alpha)+lgamma(n-x+beta)-lgamma(n+alpha+beta)-lgamma(alpha)-lgamma(beta)+lgamma(alpha+beta))
    
    # digamma and trigamma functions used in first and second 
    # perhaps computing them once here and then use them later?

    # Score function
    gradient <- rep(0.0,2)
    gradient[1] <- sum(digamma(x+alpha) - digamma(n+alpha+beta) - digamma(alpha) + digamma(alpha+beta))
    gradient[2] <- sum(digamma(n-x+beta) - digamma(n+alpha+beta) - digamma(beta) + digamma(alpha+beta))
    
    # Hessian matrix
    hessian <- matrix(0.0,2,2)
    hessian[1,1] <- sum(trigamma(x+alpha) - trigamma(n+alpha+beta) - trigamma(alpha) + trigamma(alpha+beta))
    hessian[2,2] <- sum(trigamma(n-x+beta) - trigamma(n+alpha+beta) - trigamma(beta) + trigamma(alpha+beta))
    hessian[1,2] <- sum(trigamma(alpha+beta) - trigamma(n+alpha+beta))
    hessian[2,1] <- hessian[1,2]
    
    return(list(value = -value, gradient = -gradient, hessian = -hessian)) # return negative because negative loglikelihood
  }

  fit <- trust::trust(objfun = beta_binom_fun, parinit = init, x = x, n = n, rinit = 0.1, rmax = 10.0)
  alpha_mle <- exp(fit$argument[1])
  beta_mle  <- exp(fit$argument[2])
    
  return(list(mle = c("alpha" = alpha_mle, "beta" = beta_mle), mom = c("alpha" = alpha, "beta" = beta)))
}

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
    bb <- estimate_beta_binomial(sum_ones_vector, n = no_edges)
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
    bb <- estimate_beta_binomial(sum_ones_vector, n = no_edges)
  }
  return(bb)
}  # end of function 


