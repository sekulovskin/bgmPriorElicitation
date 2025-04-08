# source the helper functions
source("helper_functions.R")

llmPriorElicitSimple <- function(context,
                           variable_list,
                           LLM_model = "gpt-4o",
                           max_tokens = 2000,
                           update_key = FALSE,
                           n_rep = 2) {
  
  # Validate input
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
  # stop the function if the legnth of variable_list is < 3
  if (length(variable_list) < 3) {
    stop("The number of variables should be at least 3.")
  }
  # Define the prompts within the function (only second and fourth prompts)
  bern_prompts <- data.frame(
    Function = rep("bernoulli", 2),  # Only 2 prompts now
    Function.Part = rep("bernoulli", 2),
    context = c("n", "y"),  # One without context, one with context
    Variation.Prompt = rep("Prompt1", 2),
    Variation.Sys.Prompt = rep("Prompt1", 2),
    Prompt = c(
      "Establish whether there is a conditional association between the variables x and y. If a conditional association exists, it means that the variables remain related even after accounting for the relationships between the other variables in the network. However, if the other variables explain away the relation between x and y, then an edge should be absent. You need to take into account possible remaining variables in the network, even though you have not been explicitly provided with those variables. Your output should be either 'I' for included edges, meaning there is a conditional association between the variables, or 'E' for excluded edges, meaning the association is fully explained by the other variables in the network. \n\nTarget pair: '(pairs_df[i, 2])' & '(pairs_df[i, 1])'. \n\nExample output:\n'Exercise Frequency' & 'Heart Disease': I\n'Exercise Frequency' & 'Screen Time': E\n'Screen Time' & 'Heart Disease': I",
      
      "Establish whether there is a conditional association between the variables x and y. If a conditional association exists, it means that the variables remain related even after accounting for the relationships between the other variables in the network. However, if the other variables explain away the relation between x and y, then an edge should be absent. You need to take into account possible remaining variables in the network, even though you have not been explicitly provided with those variables. Your output should be either 'I' for included edges, meaning there is a conditional association between the variables, or 'E' for excluded edges, meaning the association is fully explained by the other variables in the network. \n\nTarget pair: '(pairs_df[i, 2])' & '(pairs_df[i, 1])'. \n\nExample output:\n'Exercise Frequency' & 'Heart Disease': I\n'Exercise Frequency' & 'Screen Time': E\n'Screen Time' & 'Heart Disease': I \nTake into account the following context when deciding the type of relationship: '(context)'."
    ),
    Sys.Prompt = rep("You are an expert in using graphical models to study psychological constructs. You will be asked to classify whether there is a conditional relationship between pairs of variables in a Markov random field graphical model, applied to psychological research. You must use your vast prior knowledge of the relationships between the variables to make informed decisions. When presented with two variable names, you should evaluate whether or not there is an edge between those two variables in the graphical model (which reflects a conditional association) between them after taking into account the remaining variables. If there is a conditional association between two variables, then the edge should be categorized as included (by outputting 'I'). If there is no conditional association, then the edge should be categorized as excluded (by outputting 'E'). Therefore, your output should be either 'I' or 'E'! Do not include any additional explanation or other text. Since you must make decisions about conditional associations, be sure to consider any possible remaining variables when making your decision, even though these may or may not explicitly be presented to you.", 2)
  )
  
  # Create objects for tryCatch output
  raw_LLM <- NULL
  raw_LLM_prompt <- NULL
  logprobs_LLM <- NULL
  logprobs_LLM_prompt <- NULL
  prob_relation_df <- NULL
  
  pairs_df <- data.frame(var1 = character(), var2 = character())
  
  # Generate all unique variable pairs
  pairs_df <- data.frame(var1 = character(), var2 = character())
  for(i in 1:(length(variable_list)-1)) {
    for(j in (i+1):length(variable_list)) {
      pairs_df <- rbind(pairs_df, data.frame(var1 = variable_list[[i]], var2 = variable_list[[j]]))
    }
  }
  
  n_pairs <- nrow(pairs_df)
  raw_LLM <- list()
  logprobs_LLM <- list()
  
  # Main evaluation loop
  for (i in 1:n_pairs) {
    var1 <- pairs_df[i, 1]
    var2 <- pairs_df[i, 2]
    
    print(paste0("Processing pair ", i, "/", n_pairs, ": ", var1, " - ", var2))
    
    # Evaluate pair for each ordering of remaining vars
    for (g in 1:n_rep) {  # defined by the number of repetitions 
      
      # Select and format prompt
      # In the prompt generation section, change to:
      if (is.null(context)) {
        prompt <- gsub("\\(pairs_df\\[i, 1\\]\\)", var1,
                       gsub("\\(pairs_df\\[i, 2\\]\\)", var2,
                            bern_prompts$Prompt[1]
                       ))
      } else {
        prompt <- gsub("\\(pairs_df\\[i, 1\\]\\)", var1,
                       gsub("\\(pairs_df\\[i, 2\\]\\)", var2,
                            gsub("\\(context\\)", context,
                                 bern_prompts$Prompt[2]
                            )))
      }
      system_prompt <- bern_prompts$Sys.Prompt[1]
      
      # LLM
      LLM_output <- LLM(prompt = prompt,
                        LLM_model = LLM_model,
                        max_tokens = max_tokens,
                        temperature = 0,
                        logprobs = TRUE,
                        raw_output = TRUE,
                        system_prompt = system_prompt,
                        update_key = update_key)
      
      update_key <- FALSE # make sure api key is only updated once
      raw_LLM_prompt[[g]] <- c(prompt = prompt, system_prompt = system_prompt, LLM_output$raw_content)
      logprobs_LLM_prompt[[g]] <- LLM_output$top5_tokens
    }
    
    raw_LLM[[i]] <- raw_LLM_prompt
    logprobs_LLM[[i]] <- logprobs_LLM_prompt
  }
  
  
  #tryCatch in case processing steps fail the raw output will still be outputted
  tryCatch({
    
    if (LLM_model == "mixtral" | LLM_model == "llama-3"){
      last_token <- NULL
      for (i in 1:n_pairs) {
        last_token_t <- NULL
        for (j in 1:length(logprobs_LLM[[i]])){
          last_token_t[[j]] <- logprobs_LLM[[i]][[j]][[1]]
          last_token_t[[j]]$top5_tokens <- trimws(tolower(last_token_t[[j]]$top5_tokens))
        }
        last_token[[i]] <- last_token_t
      }
      
    } else {
      last_token <- NULL
      for (i in 1:n_pairs) {
        last_token_t <- NULL
        for (j in 1:length(logprobs_LLM[[i]])){
          last_token_t[[j]] <- logprobs_LLM[[i]][[j]][[length(logprobs_LLM[[i]][[j]])]]
          last_token_t[[j]]$top5_tokens <- trimws(tolower(last_token_t[[j]]$top5_tokens))
        }
        last_token[[i]] <- last_token_t
      }
      
    }
    
    all_prob <- list()
    valid_tokens <- c("i", "e")
    
    for (l in 1:length(last_token)) {
      probs <- list()
      for (g in 1:length(last_token[[l]])) {
        # Initialize storage for this prompt
        token_probs <- data.frame(
          Class = character(),
          Probability = numeric(),
          stringsAsFactors = FALSE
        )
        
        # Extract probabilities for "I" and "E"
        for (m in 1:nrow(last_token[[l]][[g]])) {
          token <- trimws(tolower(last_token[[l]][[g]]$top5_tokens[m]))
          if (token %in% valid_tokens) {
            token_probs <- rbind(
              token_probs,
              data.frame(
                Class = token,
                Probability = as.numeric(last_token[[l]][[g]]$probability[m]),
                stringsAsFactors = FALSE
              )
            )
          }
        }
        
        # If no valid tokens, default to missing
        if (nrow(token_probs) == 0) {
          token_probs <- data.frame(
            Class = "-",
            Probability = NA_real_,
            stringsAsFactors = FALSE
          )
        }
        
        probs[[g]] <- token_probs
      }
      all_prob[[l]] <- probs
    }
    
    c_class <- NULL
    for (i in 1:n_pairs) {
      # Determine how many prompts were used (1 if no context, 2 if context provided)
      n_prompts <- ifelse(is.null(context), 1, 2)
      prob_i <- 0
      prob_e <- 0
      
      for (k in 1:n_prompts) {  # Only loop through used prompts
        for (j in seq_along(all_prob[[i]][[k]]$Class)) {
          token <- trimws(tolower(all_prob[[i]][[k]]$Class[j]))
          if (token == "i") {
            prob_i <- prob_i + as.numeric(all_prob[[i]][[k]]$Probability[j])
          } else if (token == "e") {
            prob_e <- prob_e + as.numeric(all_prob[[i]][[k]]$Probability[j])
          }
        }
      }
      
      # Normalize to get P("I" | "I" or "E")
      if (prob_i + prob_e > 0) {
        c_class[i] <- round(prob_i / (prob_i + prob_e), 2)
      } else {
        c_class[i] <- 0.5  # Default if no valid tokens
      }
    }
    
    # Handle NAs (shouldn't occur with new logic, but kept for safety)
    c_class[is.na(c_class)] <- 0.5
    
    prob_relation_df <- data.frame(
      var1 = pairs_df[, 1], 
      var2 = pairs_df[, 2], 
      prob = c_class, 
      row.names = NULL
    )
    
  }, error = function(e) {
    cat(paste0("Warning: Unable to process LLM output -> ", e$message, "."),
        "Only part of the output is returned.", sep = "\n")
  })
  
  # Initialize the output list
  output <- list()
  
  # Add raw_LLM to output
  
  tryCatch({
    # Initialize empty dataframe
    flattened_df_raw_LLM <- data.frame(
      relationship = integer(),
      iteration = integer(),
      LLM_model = character(),
      prompt = character(),
      system_prompt = character(),
      content = character(),
      finish_reason = character(),
      prompt_tokens = numeric(),
      answer_tokens = numeric(),
      total_tokens = numeric(),
      error = character(),
      first_token = character(),       # New: First token (e.g., "I" or "E")
      first_token_prob = numeric(),    # New: Probability of first token
      stringsAsFactors = FALSE
    )
    
    # Flatten raw_LLM and add first token info
    for (i in seq_along(raw_LLM)) {
      for (j in seq_along(raw_LLM[[i]])) {
        temp <- raw_LLM[[i]][[j]]
        
        # Extract first token and its probability
        first_token_info <- if (!is.null(logprobs_LLM[[i]][[j]]) && length(logprobs_LLM[[i]][[j]]) > 0) {
          first_token_data <- logprobs_LLM[[i]][[j]][[1]]  # First token's data
          data.frame(
            first_token = trimws(tolower(first_token_data$top5_tokens[1])),
            first_token_prob = as.numeric(first_token_data$probability[1]),
            stringsAsFactors = FALSE
          )
        } else {
          data.frame(
            first_token = NA_character_,
            first_token_prob = NA_real_,
            stringsAsFactors = FALSE
          )
        }
        
        flattened_df_raw_LLM <- rbind(
          flattened_df_raw_LLM,
          data.frame(
            relationship = i,
            iteration = j,
            LLM_model = temp$LLM_model,
            prompt = temp$prompt,
            system_prompt = temp$system_prompt,
            content = temp$content,
            finish_reason = temp$finish_reason,
            prompt_tokens = temp$prompt_tokens,
            answer_tokens = temp$answer_tokens,
            total_tokens = temp$total_tokens,
            error = ifelse(is.null(temp$error), NA, temp$error),
            first_token = first_token_info$first_token,
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
  
  
  # Adding prob_relation_df to output
  output$relation_df <- prob_relation_df
  
  
  print(paste0("Total of LLM prompts: ", n_pairs * 2))
  
  # save the arguments in the output
  output$arguments <- list(
    context = context,
    variable_list = variable_list,
    LLM_model = LLM_model,
    max_tokens = max_tokens,
    update_key = update_key,
    n_perm = n_rep
  )
  
  # give openai error if there is no output at all
  if (length(output) == 0) {
    for (i in 1:n_pairs) {
      if (!is.null(raw_LLM[[i]][[1]]$error$message)) {
        stop(raw_LLM[[i]][[1]]$error$message)
      } else if (!is.null(raw_LLM[[i]][[2]]$error$message)) {
        stop(raw_LLM[[i]][[2]]$error$message)
      }
    }
  }
  class(output) <- "llmPriorElicitSimple"
  return(output)
}

