#' Elicit Prior Edge Inclusion Probabilities Using an LLM
#' by explicitly taking into account the LLMs decision for the remaining 
#' variables
#'
#' This function queries a large language model (LLM) to elicit prior probabilites
#' about conditional associations (edges) between variable pairs in a Markov random field,
#' given optional context and permutations of variable pair order.
#' 
#' The function iteratively evaluates each variable pair, updating a decision context
#' that is passed to the LLM to consider previous decisions. It returns edge probabilities
#' averaged across sampled permutations of pair orders.
#'
#' @param context Optional character string providing background information or study context 
#'   to be incorporated into the LLM prompt. Defaults to `NULL`.
#' @param variable_list A character vector of variable names. Must contain at least three variables.
#' @param LLM_model Character string indicating which LLM to use. Options include: 
#'   `"gpt-4o"`, `"gpt-4"`, `"gpt-4-turbo"`, `"gpt-3.5-turbo"`, `"mixtral"`, or `"llama-3"`.
#' @param max_tokens Integer specifying the maximum number of tokens to generate. 
#'   The allowed maximum varies by model: 6000 for GPT-4 variants, 3000 for GPT-3.5.
#' @param update_key Logical. If `TRUE`, updates the API key used for the LLM call. 
#'   Only the first call will use the updated key. Default is `FALSE`.
#' @param n_perm Integer or `NULL`. Number of random permutations of pair orders to evaluate. 
#'   If `NULL`, all permutations are used (can be slow for large variable sets).
#' @param seed Integer. Random seed for permutation reproducibility. Default is `123`.
#' @param prompt_specs Optional list containing user/system prompt overrides. Each entry must 
#'   contain `user` and `system` strings. If `NULL`, a default network prompt is used.
#'
#' @return A list of class `"llmPriorElicitRelations"` with the following elements:
#' \describe{
#'   \item{`relation_df`}{A data frame with columns `var1`, `var2`, and `prob`, representing
#'     the prior probability of a conditional association between variable pairs.}
#'   \item{`raw_LLM`}{A data frame with raw responses and prompt metadata for each pair across permutations.}
#'   \item{`arguments`}{A list with the function call arguments for reproducibility.}
#' }
#'
#' @details
#' Each pair is evaluated in the context of previously judged pairs, based on the current permutation.
#' The returned probabilities are averaged across permutations.
#'
#' @examples
#' \dontrun{
#' result <- llmPriorElicitRelations(
#'   context = "This study investigates the relationship between anxiety, sleep, and concentration.",
#'   variable_list = c("Anxiety", "Sleep", "Concentration"),
#'   LLM_model = "gpt-4o",
#'   n_perm = 10
#' )
#' print(result$relation_df)
#' }
#'
#' @import gtools   # this will make sense when we turn it into a package 
#' @export


llmPriorElicitRelations <- function(context,
                                    variable_list,
                                    LLM_model = "gpt-4o",
                                    max_tokens = 2000,
                                    update_key = FALSE,
                                    n_perm = NULL,
                                    seed = 123,
                                    prompt_specs = NULL) {
  
  # Validate input (same as original)
  stopifnot("'context' should be a character string or NULL." = is.character(context) | is.null(context))
  stopifnot("'variable_list' should be a vector containing more than one variables." = is.vector(variable_list) && length(variable_list) > 1)
  stopifnot("All entries in 'variable_list' should be character strings." =
              all(sapply(variable_list, is.character)))
  stopifnot("'LLM_model' should be 'gpt-4o', 'gpt-4', 'gpt-4-turbo', 'gpt-3.5-turbo', 'mixtral', or 'llama-3'." =
              LLM_model %in% c("mixtral", "gpt-4o", "gpt-4", "gpt-4-turbo", "gpt-3.5-turbo", "llama-3"))
  stopifnot("For 'gpt-4o', 'max_tokens' should be a whole number above 0, and not higher than 6000." =
              !(LLM_model == "gpt-4o") || (is.numeric(max_tokens) && max_tokens == floor(max_tokens) && max_tokens >= 0 && max_tokens <= 6000))
  stopifnot("For 'gpt-4', 'max_tokens' should be a whole number above 0, and not higher than 6000." =
              !(LLM_model == "gpt-4") || (is.numeric(max_tokens) && max_tokens == floor(max_tokens) && max_tokens >= 0 && max_tokens <= 6000))
  stopifnot("For 'gpt-4-turbo', 'max_tokens' should be a whole number above 0, and not higher than 6000." =
              !(LLM_model == "gpt-4-turbo") || (is.numeric(max_tokens) && max_tokens == floor(max_tokens) && max_tokens >= 0 && max_tokens <= 6000))
  stopifnot("For 'gpt-3.5-turbo', 'max_tokens' should be a whole number above 0, and not higher than 3000." =
              !(LLM_model == "gpt-3.5-turbo") || (is.numeric(max_tokens) && max_tokens == floor(max_tokens) && max_tokens >= 0 && max_tokens <= 3000))
  if (length(variable_list) < 3) {
    stop("The number of variables should be at least 3.")
  }
  # messages with permutations
  
  # Generate all unique combinations
  pairs_df <- data.frame(var1 = character(), var2 = character())
  for(i in 1:(length(variable_list)-1)) {
    for(j in (i+1):length(variable_list)) {
      pairs_df <- rbind(pairs_df, data.frame(var1 = variable_list[[i]], var2 = variable_list[[j]]))
    }
  }
  
  n_pairs <- nrow(pairs_df)
  
  if (missing(n_perm)) {
    n_perm <- factorial(n_pairs) 
    if (n_pairs >= 4) {  # check why it does not print the warning!! 
      message(
        "n_perm not specified. Generating all ", n_perm, " permutations of variable pairs.\n", "This may be slow for large number of variables. Specify `n_perm` to limit the number."
      )
    }
  }
  
  # if the supplied exceeds the number of possible permutations 
  if (n_perm > factorial(n_pairs)) {
    stop(
      "Requested `n_perm` (", n_perm, ") exceeds maximum possible permutations (", factorial(length(variable_list)), ").\n", "Reduce `n_perm` or set to NULL."
    )
  }
  # Define the prompts (modified to include previous decisions)
  
  if (!is.null(prompt_specs)) {
    # ensure at least two rows
    specs <- prompt_specs
    if (length(specs) == 1) specs <- rep(specs, 2)
    
    bern_prompts <- do.call(rbind, lapply(specs, function(spec) {
      data.frame(
        Function            = "bernoulli",
        Function.Part       = "bernoulli",
        context             = if (grepl("\\(context\\)", spec$user)) "y" else "n",
        Variation.Prompt    = "override",
        Variation.Sys.Prompt= "override",
        Prompt              = spec$user,
        Sys.Prompt          = spec$system,
        stringsAsFactors    = FALSE
      )
    }))
  } else {
  bern_prompts <- data.frame(
    Function = rep("bernoulli", 2),
    Function.Part = rep("bernoulli", 2),
    context = c("n", "y"),
    Variation.Prompt = rep("Prompt1", 2),
    Variation.Sys.Prompt = rep("Prompt1", 2),
    Prompt = c(
      # Without context
      "Establish whether there is a conditional association between the variables x and y. If a conditional association exists, it means that the variables remain related even after accounting for the relationships between the other variables in the network. However, if the other variables explain away the relation between x and y, then an edge should be absent. Your output should be either 'I' for included edges, meaning there is a conditional association between the variables, or 'E' for excluded edges, meaning the association is fully explained by the other variables in the network. Consider all previous decisions when evaluating subsequent pairs.\n\nCurrent target pair: '(pairs_df[i, 2])' & '(pairs_df[i, 1])'\n\nPrevious decisions in this network:\n(previous_decisions)\n\nRemaining variables to consider: (remaining_vars)\n\nOutput format:\n'Var1' & 'Var2': I/E",
      
      # With context
      "Establish whether there is a conditional association between the variables x and y. If a conditional association exists, it means that the variables remain related even after accounting for the relationships between the other variables in the network. However, if the other variables explain away the relation between x and y, then an edge should be absent. Your output should be either 'I' for included edges, meaning there is a conditional association between the variables, or 'E' for excluded edges, meaning the association is fully explained by the other variables in the network. Consider all previous decisions when evaluating subsequent pairs.\n\nCurrent target pair: '(pairs_df[i, 2])' & '(pairs_df[i, 1])'\n\nPrevious decisions in this network:\n(previous_decisions)\n\nRemaining variables to consider: (remaining_vars)\n\nContext to consider: '(context)'\n\nOutput format:\n'Var1' & 'Var2': I/E"
    ),
    Sys.Prompt = rep("You are an expert in using graphical models to study psychological constructs. You will be asked to classify whether there is a conditional relationship between pairs of variables in a Markov random field grapical model, applied to psychological research. You must use your vast prior knowledge of the relationships between the variables to make informed decisions. Evaluate conditional associations between variable pairs in sequence, considering all previous decisions. For each pair, output 'I' if there's a direct association after accounting for other variables, or 'E' if the association is explained by other variables. Only output 'I' or 'E' with no additional text.", 2)
  )
 }
  # Initialize output objects
  raw_LLM <- list()
  logprobs_LLM <- list()
  prob_relation_df <- NULL
  
  
  # Generate permutations of pairs order
  perms <- gtools::permutations(n_pairs, n_pairs)
  # sample rows from perms
  set.seed(seed) # For reproducibility
  # Randomly sample n_perm rows from perms
  perms <- perms[sample(1:nrow(perms), n_perm), ]
  
  # Main evaluation loop - now over permutations
  for (perm_idx in 1:n_perm) {
    current_order <- perms[perm_idx, ]
    previous_decisions <- list()
    raw_LLM_perm <- list()
    logprobs_LLM_perm <- list()
    
    for (pair_order in 1:n_pairs) {
      i <- current_order[pair_order]
      var1 <- pairs_df[i, 1]
      var2 <- pairs_df[i, 2]
      remaining_vars <- setdiff(variable_list, c(var1, var2))
      
      # Format previous decisions string
      prev_decisions_str <- if (length(previous_decisions) > 0) {
        paste(sapply(previous_decisions, function(x) {
          sprintf("'%s' & '%s': %s", x$var1, x$var2, x$decision)
        }), collapse = "\n")
      } else {
        "No previous decisions"
      }
      
      print(paste0("Processing permutation ", perm_idx, ", pair ", pair_order, "/", n_pairs, ": ", var1, " - ", var2))
      
      # Select and format prompt depending on context 
      if (is.null(context)) {
        prompt <- gsub("\\(pairs_df\\[i, 1\\]\\)", var1,
                       gsub("\\(pairs_df\\[i, 2\\]\\)", var2,
                            gsub("\\(previous_decisions\\)", prev_decisions_str,
                                 gsub("\\(remaining_vars\\)", paste(remaining_vars, collapse = ", "),
                                      bern_prompts$Prompt[1]))))
      } else {
        prompt <- gsub("\\(pairs_df\\[i, 1\\]\\)", var1,
                       gsub("\\(pairs_df\\[i, 2\\]\\)", var2,
                            gsub("\\(context\\)", context,
                                 gsub("\\(previous_decisions\\)", prev_decisions_str,
                                      gsub("\\(remaining_vars\\)", paste(remaining_vars, collapse = ", "),
                                           bern_prompts$Prompt[2])))))
      }
      system_prompt <- bern_prompts$Sys.Prompt[1]
      
      # LLM call
      LLM_output <- LLM(prompt = prompt,
                        LLM_model = LLM_model,
                        max_tokens = max_tokens,
                        temperature = 0,
                        logprobs = TRUE,
                        raw_output = TRUE,
                        system_prompt = system_prompt,
                        update_key = update_key)
      
      update_key <- FALSE # make sure api key is only updated once
      
      # Store raw output for this permutation
      raw_LLM_perm[[pair_order]] <- c(prompt = prompt, system_prompt = system_prompt, LLM_output$raw_content)
      logprobs_LLM_perm[[pair_order]] <- LLM_output$top5_tokens
      
      # Parse decision and store for next iterations
      decision <- parse_decision(LLM_output$raw_content$content)
      previous_decisions[[pair_order]] <- list(var1 = var1, var2 = var2, decision = decision)
    }
    
    raw_LLM[[perm_idx]] <- raw_LLM_perm
    logprobs_LLM[[perm_idx]] <- logprobs_LLM_perm
  }
  
  # Process results (similar to original but accounting for permutations)
  tryCatch({
    if (LLM_model == "mixtral" | LLM_model == "llama-3"){
      last_token <- list()
      for (perm_idx in 1:length(logprobs_LLM)) {
        last_token_perm <- list()
        for (pair_order in 1:length(logprobs_LLM[[perm_idx]])) {
          last_token_perm[[pair_order]] <- logprobs_LLM[[perm_idx]][[pair_order]][[1]]
          last_token_perm[[pair_order]]$top5_tokens <- trimws(tolower(last_token_perm[[pair_order]]$top5_tokens))
        }
        last_token[[perm_idx]] <- last_token_perm
      }
    } else {
      last_token <- list()
      for (perm_idx in 1:length(logprobs_LLM)) {
        last_token_perm <- list()
        for (pair_order in 1:length(logprobs_LLM[[perm_idx]])) {
          last_token_perm[[pair_order]] <- logprobs_LLM[[perm_idx]][[pair_order]][[length(logprobs_LLM[[perm_idx]][[pair_order]])]]
          last_token_perm[[pair_order]]$top5_tokens <- trimws(tolower(last_token_perm[[pair_order]]$top5_tokens))
        }
        last_token[[perm_idx]] <- last_token_perm
      }
    }
    
    # Initialize probability matrix
    prob_matrix <- matrix(NA, nrow = n_pairs, ncol = n_perm)
    
    # Extract probabilities for each pair in each permutation
    valid_tokens <- c("i", "e")
    
    for (perm_idx in 1:n_perm) {
      for (pair_order in 1:n_pairs) {
        pair_idx <- perms[perm_idx, pair_order]
        token_data <- last_token[[perm_idx]][[pair_order]]
        
        prob_i <- 0
        prob_e <- 0
        
        for (m in 1:nrow(token_data)) {
          token <- trimws(tolower(token_data$top5_tokens[m]))
          if (token %in% valid_tokens) {
            if (token == "i") prob_i <- prob_i + as.numeric(token_data$probability[m])
            if (token == "e") prob_e <- prob_e + as.numeric(token_data$probability[m])
          }
        }
        
        # Calculate probability for this pair in this permutation
        if (prob_i + prob_e > 0) {
          prob_matrix[pair_idx, perm_idx] <- prob_i / (prob_i + prob_e)
        } else {
          prob_matrix[pair_idx, perm_idx] <- 0.5  # Default if no valid tokens
        }
      }
    }
    
    # Average probabilities across permutations
    avg_probs <- rowMeans(prob_matrix, na.rm = TRUE)
    
    prob_relation_df <- data.frame(
      var1 = pairs_df[, 1], 
      var2 = pairs_df[, 2], 
      prob = avg_probs, 
      row.names = NULL
    )
    
  }, error = function(e) {
    cat(paste0("Warning: Unable to process LLM output -> ", e$message, "."),
        "Only part of the output is returned.", sep = "\n")
  })
  
  # Prepare output
  output <- list()
  
  # Flatten raw_LLM output
  tryCatch({
    flattened_df_raw_LLM <- data.frame(
      permutation = integer(),
      pair_order = integer(),
      pair_index = integer(),
      var1 = character(),
      var2 = character(),
      LLM_model = character(),
      prompt = character(),
      system_prompt = character(),
      content = character(),
      finish_reason = character(),
      prompt_tokens = numeric(),
      answer_tokens = numeric(),
      total_tokens = numeric(),
      error = character(),
      first_token_prob = numeric(),
      stringsAsFactors = FALSE
    )
    
    for (perm_idx in 1:length(raw_LLM)) {
      # Extract the sequence of I/E decisions for this permutation
      ie_sequence <- sapply(raw_LLM[[perm_idx]], function(x) {
        substr(x$content, nchar(x$content), nchar(x$content))  # Gets last character
      })
      
      for (pair_order in 1:length(raw_LLM[[perm_idx]])) {
        pair_idx <- perms[perm_idx, pair_order]
        temp <- raw_LLM[[perm_idx]][[pair_order]]
        
        first_token_info <- if (!is.null(logprobs_LLM[[perm_idx]][[pair_order]]) && 
                                length(logprobs_LLM[[perm_idx]][[pair_order]]) > 0) {
          first_token_data <- logprobs_LLM[[perm_idx]][[pair_order]][[1]]
          data.frame(
            first_token_prob = as.numeric(first_token_data$probability[1]),
            stringsAsFactors = FALSE
          )
        } else {
          data.frame(
            first_token_prob = NA_real_,
            stringsAsFactors = FALSE
          )
        }
        
        # Store the full I/E sequence in the content column for the LAST ROW only
        current_content <- if (pair_order == length(raw_LLM[[perm_idx]])) {
          paste0(temp$content, "\n\nFULL SEQUENCE: ", paste(ie_sequence, collapse = ""))
        } else {
          temp$content
        }
        
        flattened_df_raw_LLM <- rbind(
          flattened_df_raw_LLM,
          data.frame(
            permutation = perm_idx,
            pair_order = pair_order,
            pair_index = pair_idx,
            var1 = pairs_df[pair_idx, 1],
            var2 = pairs_df[pair_idx, 2],
            LLM_model = temp$LLM_model,
            prompt = temp$prompt,
            system_prompt = temp$system_prompt,
            content = current_content,  # Modified to include sequence for last row
            finish_reason = temp$finish_reason,
            prompt_tokens = temp$prompt_tokens,
            answer_tokens = temp$answer_tokens,
            total_tokens = temp$total_tokens,
            error = ifelse(is.null(temp$error), NA, temp$error),
            first_token_prob = first_token_info$first_token_prob,
            stringsAsFactors = FALSE
          )
        )
      }
    }
    output$raw_LLM <- flattened_df_raw_LLM
  }, error = function(e) {
    cat(paste0("Warning: Unable to return raw LLM output -> ", e$message, "."),
        "Only part of the output is returned.", sep = "\n")
  })
  
  output$relation_df <- prob_relation_df
  
  print(paste0("Total of LLM prompts: ", n_perm * n_pairs))
  
  # save the arguments in the output
  output$arguments <- list(
    context = context,
    variable_list = variable_list,
    LLM_model = LLM_model,
    max_tokens = max_tokens,
    update_key = update_key,
    n_perm = n_perm,
    seed = seed
  )

  if (length(output) == 0) {
    for (perm_idx in 1:length(raw_LLM)) {
      for (pair_order in 1:length(raw_LLM[[perm_idx]])) {
        if (!is.null(raw_LLM[[perm_idx]][[pair_order]]$error$message)) {
          stop(raw_LLM[[perm_idx]][[pair_order]]$error$message)
        }
      }
    }
  }
  class(output) <- "llmPriorElicitRelations"
  return(output)
}

