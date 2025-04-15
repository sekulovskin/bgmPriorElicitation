# This script is intended for testing the different functions

# Sorce the functions: 

source("llmPriorElicit.R") # function that takes into account the remaining variables

source("llmPriorElicitRelations.R") # function that takes into account the remaining variables
                                    # as well as the decisions on the relations of those variables

source("llmPriorElicitSimple.R")  # function that only evaluates the marginal relations without taking
                                  # the remaining variables into account. However, the prompt still explicitly
                                  # mentions that these are conditional associations and that it should take into
                                  # account any possible remaining variables that could influence the association

source("betaBinParameters.R") # function that calculates the beta-binomial parameters for the llm object


# Example

context <- "Depression and Anxiety have been shown to co-occur."

variable_list <- c("anxiety", "depression", 
                   "hair color", "number of cigarettes smoked",
                   "ocd")



# Test 1: llmPriorElicit (1:without context and 2:with context)

test1.1  <- llmPriorElicit(NULL,
                        variable_list,
                        LLM_model = "gpt-4o",
                        max_tokens = 2000,
                        update_key = FALSE,
                        n_perm = 3)

test1.2  <- llmPriorElicit(context,
                         variable_list,
                         LLM_model = "gpt-4o",
                         max_tokens = 2000,
                         update_key = FALSE,
                         n_perm = 3) # there is no default 

# Test 2: llmPriorElicitRelations (1:without context and 2:with context)

test2.1  <- llmPriorElicitRelations(NULL,
                                    variable_list,
                                    LLM_model = "gpt-4o",
                                    max_tokens = 2000,
                                    update_key = FALSE,
                                    n_perm = 20)

test2.2  <- llmPriorElicitRelations(context,
                                    variable_list,
                                    LLM_model = "gpt-4o",
                                    max_tokens = 2000,
                                    update_key = FALSE,
                                    n_perm = 2) # there is no default


# Test 3: llmPriorElicitSimple (1:without context and 2:with context)

test3.1  <- llmPriorElicitSimple(NULL,
                                 variable_list,
                                 LLM_model = "gpt-4o",
                                 max_tokens = 2000,
                                 update_key = FALSE,
                                 n_rep = 3) # default is 2

test3.2  <- llmPriorElicitSimple(context,
                                 variable_list,
                                 LLM_model = "gpt-4o",
                                 max_tokens = 2000,
                                 update_key = FALSE,
                                 n_rep = 3)

# calculate Beta-binomial parameters
# These need to be estimated with much more repetitions 

bb1.1 <- betaBinParameters(test1.1)
bb1.2 <- betaBinParameters(test1.2)

bb2.1 <- betaBinParameters(test2.1)
bb2.2 <- betaBinParameters(test2.2)

bb3.1 <- betaBinParameters(test3.1)
bb3.2 <- betaBinParameters(test3.2)

