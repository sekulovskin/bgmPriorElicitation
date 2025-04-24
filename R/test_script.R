# This script is intended for testing the different functions

# Sorce the functions: 
source("R/helper_functions.R") # helper functions for the main functions

source("R/llmPriorElicit.R") # function that takes into account the remaining variables

source("R/llmPriorElicitRelations.R") # function that takes into account the remaining variables
                                    # as well as the decisions on the relations of those variables

source("R/llmPriorElicitSimple.R")  # function that only evaluates the marginal relations without taking
                                  # the remaining variables into account. However, the prompt still explicitly
                                  # mentions that these are conditional associations and that it should take into
                                  # account any possible remaining variables that could influence the association

source("R/betaBinParameters.R") # function that calculates the beta-binomial parameters for the llm object


source("R/callPriorElicitVariations.R")

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
bb1.1
bb1.2 <- betaBinParameters(test1.2)
bb1.2

bb2.1 <- betaBinParameters(test2.1)
bb2.1 
bb2.2 <- betaBinParameters(test2.2)
bb2.2

bb3.1 <- betaBinParameters(test3.1)
bb3.1
bb3.2 <- betaBinParameters(test3.2)
bb3.2 


# Test the prompt variation function: 

# test -------------------------------------------------------------------------
# llmPriorElicitSimple 
prompt_bank <- list(
  list(
    system = "Act as an expert in graphical models for psychological constructs. You’ll receive pairs of variable names and must decide whether they share a conditional association in a Markov random field after accounting for all other variables. If their relationship endures once every other node is controlled for, output ‘I’ (included edge). If it disappears because the remaining variables explain it away, output ‘E’ (excluded edge). Consider any unobserved or implicit variables. Do not add explanations—only ‘I’ or ‘E’.",
    user   = "Determine whether a conditional association exists between x and y—that is, whether their relationship persists once all other network variables are taken into account. If the link between x and y remains even after adjusting for the rest, include an edge (‘I’). If the correlation vanishes because the other variables fully account for it, omit the edge (‘E’). Note that you must implicitly consider any additional, unlisted variables within the network. Target pair: '(pairs_df[i, 2])' & '(pairs_df[i, 1])'. Example output: 'Exercise Frequency' & 'Heart Disease': I 'Exercise Frequency' & 'Screen Time': E 'Screen Time' & 'Heart Disease': I \nConsider the following context: '(context)'."
  ),
  list(  # these are the original prmpts 
    system = "You are an expert in using graphical models to study psychological constructs. You will be asked to classify whether there is a conditional relationship between pairs of variables in a Markov random field graphical model, applied to psychological research. You must use your vast prior knowledge of the relationships between the variables to make informed decisions. When presented with two variable names, you should evaluate whether or not there is an edge between those two variables in the graphical model (which reflects a conditional association) between them after taking into account the remaining variables. If there is a conditional association between two variables, then the edge should be categorized as included (by outputting 'I'). If there is no conditional association, then the edge should be categorized as excluded (by outputting 'E'). Therefore, your output should be either 'I' or 'E'! Do not include any additional explanation or other text. Since you must make decisions about conditional associations, be sure to consider any possible remaining variables when making your decision, even though these may or may not explicitly be presented to you.",
    user   = "Establish whether there is a conditional association between the variables x and y. If a conditional association exists, it means that the variables remain related even after accounting for the relationships between the other variables in the network. However, if the other variables explain away the relation between x and y, then an edge should be absent. You need to take into account possible remaining variables in the network, even though you have not been explicitly provided with those variables. Your output should be either 'I' for included edges, meaning there is a conditional association between the variables, or 'E' for excluded edges, meaning the association is fully explained by the other variables in the network. \n\nTarget pair: '(pairs_df[i, 2])' & '(pairs_df[i, 1])'. \n\nExample output:\n'Exercise Frequency' & 'Heart Disease': I\n'Exercise Frequency' & 'Screen Time': E\n'Screen Time' & 'Heart Disease': I \nTake into account the following context when deciding the type of relationship: '(context)'."
  )
)

res1 <- callPriorElicitVariations(
  target_fun    = llmPriorElicitSimple,
  prompt_specs  = prompt_bank,
  context       = "clinical adolescents",
  variable_list = variable_list,
  LLM_model     = "gpt-4o",
  max_tokens    = 1000,
  update_key    = FALSE,
  n_rep         = 2
)

print(res1$summary)


# llmPriorElicit

prompt_bank <- list(
  list(
    system = "Act as an expert in graphical models for psychological constructs. You’ll receive pairs of variable names and must decide whether they share a conditional association in a Markov random field after accounting for all other variables. If their relationship endures once every other node is controlled for, output ‘I’ (included edge). If it disappears because the remaining variables explain it away, output ‘E’ (excluded edge). Consider any unobserved or implicit variables. Do not add explanations—only ‘I’ or ‘E’. When determining conditional associations, remember to account for the other variables in your evaluation.",
    user   = "Determine whether a conditional association exists between x and y—that is, whether their relationship persists once you’ve accounted for all other network variables (including those in (remaining_vars)). If x and y remain related after adjustment, output ‘I’ (included edge). If the other variables explain away their relation, output ‘E’ (excluded edge). Target pair: '(pairs_df[i, 2])' & '(pairs_df[i, 1])'. Consider the following remaining variables in the network when evaluating the relation: (remaining_vars). Example output:'Exercise Frequency' & 'Heart Disease': I 'Exercise Frequency' & 'Screen Time': E 'Screen Time' & 'Heart Disease': I."
  ),
  list(  # these are the original prmpts 
    system = "You are an expert in using graphical models to study psychological constructs. You will be asked to classify whether there is a conditional relationship between pairs of variables in a Markov random field graphical model, applied to psychological research. You must use your vast prior knowledge of the relationships between the variables to make informed decisions. When presented with two variable names, you should evaluate whether or not there is an edge between those two variables in the graphical model (which reflects a conditional association) between them after taking into account the remaining variables. If there is a conditional association between two variables, then the edge should be categorized as included (by outputting 'I'). If there is no conditional association, then the edge should be categorized as excluded (by outputting 'E'). Therefore, output should be either 'I' or 'E.  Do not include any additional explanation or other text. Since you must make decisions about conditional associations, be sure to consider the remaining variables when making your decision.",
    user   = "Establish whether there is a conditional association between the variables x and y. If a conditional association exists, it means that the variables remain related even after accounting for the relationships between the other variables in the network. However, if the other variables explain away the relation between x and y, then an edge should be absent. Your output should be either 'I' for included edges, meaning there is a conditional association between the variables, or 'E' for excluded edges, meaning the association is fully explained by the other variables in the network. \n\nTarget pair: '(pairs_df[i, 2])' & '(pairs_df[i, 1])' Consider the following remaining variables in the network when evaluating the presence of a relation in the target pair: (remaining_vars). \n\nExample output:\n'Exercise Frequency' & 'Heart Disease': I\n'Exercise Frequency' & 'Screen Time': E\n'Screen Time' & 'Heart Disease': I \nTake into account the following context when deciding the type of relationship: '(context)'."
  )
)


res2 <- callPriorElicitVariations(
  target_fun    = llmPriorElicit,
  prompt_specs  = prompt_bank,
  context       = "clinical adolescents",
  variable_list = variable_list,
  LLM_model     = "gpt-4o",
  max_tokens    = 1000,
  update_key    = FALSE,
  n_perm        = 2,
  seed          = 2025
)

print(res2$summary)



# llmPriorElicitrerlations 

prompt_bank <- list(
  list(
    system = "Act as an expert in graphical models for psychological constructs: for each variable pair, sequentially decide whether a conditional association exists in a Markov random field after accounting for all other variables and prior decisions, then output ‘I’ if a direct link remains or ‘E’ if it’s explained away—no additional text.",
    user   = "Determine whether a conditional association exists between x and y after accounting for other network variables (including (remaining_vars) and all previous decisions); output ‘I’ if their relation persists or ‘E’ if it’s explained away. Current target pair: '(pairs_df[i, 2])' & '(pairs_df[i, 1])'; Previous decisions: (previous_decisions); Remaining variables: (remaining_vars); Context: '(context)'. Output format: 'Var1' & 'Var2': I/E"
  ),
  list(  # these are the original prmpts 
    system = "You are an expert in using graphical models to study psychological constructs. You will be asked to classify whether there is a conditional relationship between pairs of variables in a Markov random field grapical model, applied to psychological research. You must use your vast prior knowledge of the relationships between the variables to make informed decisions. Evaluate conditional associations between variable pairs in sequence, considering all previous decisions. For each pair, output 'I' if there's a direct association after accounting for other variables, or 'E' if the association is explained by other variables. Only output 'I' or 'E' with no additional text.",
    user   = "Establish whether there is a conditional association between the variables x and y. If a conditional association exists, it means that the variables remain related even after accounting for the relationships between the other variables in the network. However, if the other variables explain away the relation between x and y, then an edge should be absent. Your output should be either 'I' for included edges, meaning there is a conditional association between the variables, or 'E' for excluded edges, meaning the association is fully explained by the other variables in the network. \n\nTarget pair: '(pairs_df[i, 2])' & '(pairs_df[i, 1])' Consider the following remaining variables in the network when evaluating the presence of a relation in the target pair: (remaining_vars). \n\nExample output:\n'Exercise Frequency' & 'Heart Disease': I\n'Exercise Frequency' & 'Screen Time': E\n'Screen Time' & 'Heart Disease': I \nTake into account the following context when deciding the type of relationship: '(context)'."
  )
)

res3 <- callPriorElicitVariations(
  target_fun    = llmPriorElicitRelations,
  prompt_specs  = prompt_bank,
  context       = "clinical adolescents",
  variable_list = variable_list,
  LLM_model     = "gpt-4o",
  max_tokens    = 1000,
  update_key    = FALSE,
  n_perm        = 2,
  seed          = 2025
)
print(res3$summary)

