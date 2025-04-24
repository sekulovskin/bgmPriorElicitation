#' Estimate Beta-Binomial Parameters from LLM Output
#'
#' This function computes the parameters of a Beta-Binomial distribution from the edge
#' inclusion outputs of an LLM-based prior elicitation object. The estimated parameters
#' describe the distribution of edge counts across permutations or repetitions.
#'
#' @param llmobject An object of class `"llmPriorElicit"`, `"llmPriorElicitSimple"`, or 
#'   `"llmPriorElicitRelations"` as returned by LLM-based prior elicitation functions.
#' @param method Estimation method. One of `"mle"` (maximum likelihood) or `"mom"` (method of moments).
#'   Default is `"mle"`.
#' @param force_mom Logical. If `TRUE`, forces method of moments estimation even if `"mle"` is requested.
#'   Default is `FALSE`.
#'
#' @return A list containing the estimated `alpha` and `beta` parameters of the Beta-Binomial distribution.
#'
#' @details
#' The function extracts the number of included edges (`"I"`) for each permutation or repetition
#' from the LLM output and fits a Beta-Binomial distribution to these counts. This gives a probabilistic
#' description of edge inclusion uncertainty across network realizations.
#'
#' For `llmPriorElicit` and `llmPriorElicitSimple` objects, edge inclusion is determined from
#' the raw content per iteration. For `llmPriorElicitRelations` objects, edge inclusion is extracted
#' from the full I/E sequence in the final output for each permutation.
#'
#' A warning is issued if fewer than 10 permutations are detected, as parameter estimation
#' may be unreliable in such cases.
#'
#' @examples
#' \dontrun{
#' llm_out <- llmPriorElicitSimple(
#'   context = "Exploring cognitive symptoms and mood in depression",
#'   variable_list = c("Concentration", "Sadness", "Sleep"),
#'   n_rep = 3
#' )
#' beta_params <- betaBinParameters(llm_out)
#' print(beta_params)
#' }
#'
#' @import dplyr
#' @import stringr   FixHub Bakkerstraat 15 3511
#' @export

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


