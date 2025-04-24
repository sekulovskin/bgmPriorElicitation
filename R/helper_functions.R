# Some of these functions are copied from the theoraizer package 
# https://github.com/MeikeWaaijers/theoraizer

# logprob helper function

LLM_logprobs <- function(raw_content,
                         LLM_model = LLM_model) {
  
  length <- length(raw_content$choices[[1]]$logprobs$content)
  logprobs_dfs <- vector("list", length = length)
  
  for (j in seq_along(logprobs_dfs)) {
    logprobs_list <- raw_content$choices[[1]]$logprobs$content[[j]]$top_logprobs
    
    top5_tokens <- vector("list", length = length(logprobs_list))
    top5_logprobs <- numeric(length(logprobs_list))
    top5_probabilities <- numeric(length(logprobs_list))
    
    for (i in seq_along(logprobs_list)) {
      top5_tokens[[i]] <- logprobs_list[[i]]$token
      top5_logprobs[i] <- logprobs_list[[i]]$logprob
      
      top5_probabilities[i] <- round(exp(top5_logprobs[i]), 5)
    }
    
    logprobs_dfs[[j]] <- data.frame(top5_tokens = unlist(top5_tokens),
                                    logprob = top5_logprobs,
                                    probability = top5_probabilities)
  }
  
  return(logprobs_dfs)
}


# LLM helper function

get_api_key <- function(service_name, update_key = FALSE) {
  # Check if running on shinyapps.io
  shinyapps <- Sys.getenv("R_CONFIG_ACTIVE") == "shinyapps"
  # Check if running shiny app
  if (shinyapps) {
    # Attempt to retrieve the API key from environment variable
    api_key <- Sys.getenv("OPENAI_API_KEY")
    
    if (nzchar(api_key)) {
      return(api_key)
    }
  }
  
  # Check if running in a CI environment
  ci <- nzchar(Sys.getenv("CI"))
  
  if (ci) {
    # Attempt to retrieve the API key from environment variable
    api_key <- Sys.getenv("OPENAI_API_KEY")
    
    if (nzchar(api_key)) {
      return(api_key)
    }
  }
  
  # If not found in environment variable and not in CI, attempt to retrieve from keyring
  if (!ci && !shinyapps && (update_key || nrow(keyring::key_list(service = service_name)) == 0)) {
    cat("To use this functionality, an API key needs to be set.\n")
    cat("Please follow these steps to resolve the issue:\n")
    if (service_name == "huggingface") {
      cat("1. Create an API key on https://huggingface.co/settings/tokens \n")
    } else if (service_name == "openai") {
      cat("1. Create an API key on https://platform.openai.com/account/api-keys \n")
    }
    cat("2. Please enter your API key below to add/update it.")
    answer <- readline("API key = ")
    keyring::key_set_with_value(service = service_name, username = "user", password = answer)
  }
  
  return(keyring::key_get(service = service_name, username = "user"))
}

## the main LLM function

LLM <- function(prompt = prompt,
                LLM_model = "gpt-4o",
                max_tokens = 2000,
                temperature = 0,
                # suffix = NULL,
                top_p = 1,
                logprobs = TRUE,
                top_logprobs = 5,
                # stop = NULL,
                # presence_penalty = 0,
                # frequency_penalty = 0,
                timeout_sec = 60,
                system_prompt = NULL,
                raw_output = TRUE,
                update_key = update_key){
  
  api_key <- get_api_key("openai",
                         update_key = update_key)
  
  # API endpoint
  endpoint <- "https://api.openai.com/v1/chat/completions"
  
  if (logprobs == TRUE) {
    # Request body
    request_body <- list(
      model = LLM_model,
      max_tokens = max_tokens,
      temperature = temperature,
      # n = 1,
      # suffix = suffix,
      # top_p = top_p,
      top_logprobs = top_logprobs,
      logprobs = logprobs,
      # stop = stop,
      # presence_penalty = presence_penalty,
      # frequency_penalty = frequency_penalty,
      messages = list(
        list(role = "system", content = system_prompt),
        list(role = "user", content = prompt)
      )
    )
    
  } else if (logprobs == FALSE) {
    # Request body
    request_body <- list(
      model = LLM_model,
      max_tokens = max_tokens,
      temperature = temperature,
      # n = 1,
      # suffix = suffix,
      # top_p = top_p,
      logprobs = logprobs,
      # stop = stop,
      # presence_penalty = presence_penalty,
      # frequency_penalty = frequency_penalty,
      messages = list(
        list(role = "system", content = system_prompt),
        list(role = "user", content = prompt)
      )
    )
    
  }
  
  # Make the API request
  request <- httr::RETRY(verb = "POST",
                         url = endpoint,
                         body = request_body,
                         httr::add_headers(Authorization = paste("Bearer",
                                                                 api_key)),
                         encode = "json",
                         times = 5,
                         httr::timeout(timeout_sec))
  
  # Extract the response
  raw_content <- content <- httr::content(request)
  output <- content$choices[[1]]$message$content
  
  
  
  if (raw_output == TRUE && logprobs == TRUE){
    top5_logprobs <- LLM_logprobs(raw_content = raw_content,
                                  LLM_model = LLM_model)
    output <- list(raw_content = list(LLM_model = raw_content$model,
                                      content = raw_content$choices[[1]]$message$content,
                                      finish_reason = raw_content$choices[[1]]$finish_reason,
                                      prompt_tokens = raw_content$usage$prompt_tokens,
                                      answer_tokens = raw_content$usage$completion_tokens,
                                      total_tokens = raw_content$usage$total_tokens,
                                      error = raw_content$error),
                   top5_tokens = top5_logprobs,
                   output = output)
    
    
  } else if (raw_output == TRUE && logprobs == FALSE) {
    output <- list(raw_content = list(LLM_model = raw_content$model,
                                      content = raw_content$choices[[1]]$message$content,
                                      finish_reason = raw_content$choices[[1]]$finish_reason,
                                      prompt_tokens = raw_content$usage$prompt_tokens,
                                      answer_tokens = raw_content$usage$completion_tokens,
                                      total_tokens = raw_content$usage$total_tokens,
                                      error = raw_content$error),
                   output = output)
    
  } else if (raw_output == FALSE && logprobs == TRUE) {
    top5_logprobs <- LLM_logprobs(raw_content = raw_content,
                                  LLM_model = LLM_model)
    output <- list(top5_tokens = top5_logprobs,
                   output = output)
    
  }
  
  return(output)
}

# Helper function to parse decision from LLM output

parse_decision <- function(content) {
  # Simple parsing - look for "I" or "E" in the output
  content <- tolower(trimws(content))
  if (grepl("i", content)) {
    return("I")
  } else if (grepl("e", content)) {
    return("E")
  } else {
    return(NA)  # Handle cases where output doesn't contain I/E
  }
}



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


