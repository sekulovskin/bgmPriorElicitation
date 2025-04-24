#' Compare Prompt Variants for LLM-Based Prior Elicitation
#'
#' This function evaluates multiple prompt variants by repeatedly calling a given LLM-based 
#' prior elicitation function (e.g., `llmPriorElicitSimple`, `llmPriorElicit`, or `llmPriorElicitRelations`)
#' using different user/system prompt pairs. It returns a summary of mean and standard deviation
#' in the estimated edge inclusion probabilities across prompt variants.
#'
#' @param target_fun A function used for prior elicitation, such as `llmPriorElicitSimple`, 
#'   `llmPriorElicit`, or `llmPriorElicitRelations`.
#' @param prompt_specs A list of prompt specifications. Each element must be a list containing
#'   `system` and `user` character strings used to construct prompts.
#' @param ... Additional arguments passed to the target function. These typically include
#'   `context`, `variable_list`, `LLM_model`, `n_rep` or `n_perm`, and other settings.
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{`summary`}{A data frame summarizing the results across prompt variants, including
#'     the mean and standard deviation of the prior probabilities for each variable pair.}
#'   \item{`raw_outputs`}{A list of full outputs returned by the target function for each prompt variant.}
#'   \item{`prompts`}{The list of prompt specifications used.}
#' }
#'
#' @details
#' Each prompt variant is evaluated independently by calling the selected prior elicitation
#' function once with that prompt. The resulting edge probabilities are aggregated across
#' prompt variants and summarized with means and standard deviations.
#'
#' This function is useful for testing the sensitivity of prior elicitation to different 
#' LLM prompt formulations.
#'
#' @examples
#' \dontrun{
#' prompt_variants <- list(
#'   list(system = "You are an expert...", user = "Decide if x and y are conditionally related."),
#'   list(system = "You are a statistical analyst...", user = "Should x and y have an edge?")
#' )
#' 
#' result <- callPriorElicitVariations(
#'   target_fun = llmPriorElicitSimple,
#'   prompt_specs = prompt_variants,
#'   context = "Study on well-being and lifestyle",
#'   variable_list = c("Sleep", "Mood", "Exercise"),
#'   n_rep = 2
#' )
#'
#' print(result$summary)
#' }
#'
#' @import dplyr    # this will make sense when we turn it into a package 
#' @export


callPriorElicitVariations <- function(
    target_fun,      # e.g. llmPriorElicitSimple, llmPriorElicitRelations, llmPriorElicit…
    prompt_specs,    # list of list(system=..., user=...)
    ...              # all the usual args (context, variable_list, LLM_model, etc.)
) {
  # sanity check
  stopifnot(
    is.list(prompt_specs),
    all(sapply(prompt_specs, function(x)
      is.list(x) && is.character(x$system) && is.character(x$user)))
  )
  
  # For each spec, invoke the target and collect its full raw output
  raw_outputs <- lapply(prompt_specs, function(spec) {
    do.call(
      target_fun,
      args = c(
        list(prompt_specs = list(spec)),  # pass *only* this one spec
        list(...)
      )
    )
  })
  
  # Extract and tag each relation_df
  library(dplyr)
  summary <- bind_rows(
    lapply(seq_along(raw_outputs), function(i) {
      df <- raw_outputs[[i]]$relation_df
      df$prompt_id <- i
      df
    })
  ) %>%
    group_by(var1, var2) %>%
    summarize(
      mean_prob = mean(prob, na.rm = TRUE),
      sd_prob   = sd(prob,   na.rm = TRUE),
      n_runs    = n(),
      .groups   = "drop"
    )
  
  list(
    summary     = summary,
    raw_outputs = raw_outputs,
    prompts     = prompt_specs
  )
}





