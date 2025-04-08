# bgmPriorElicitation

# `llmPriorElicit`

This function evaluates conditional dependencies between variable pairs using an LLM, with permutation-based conditioning on remaining variables (but not their decided relations).

Key Features:
Handles Permutations of Remaining Variables.

For each pair, generates permutations of remaining variables.

Uses `n_perm` to control permutation count (default: all permutations for ≤4 variables, with warnings).


# `llmPriorElicitRelations`
This function uses an LLM to iterativeley evaluate conditional dependencies between pairs of variables in a graphical model.

Key Features:
In each permutation, takes into account the decision for the remaining nodes.

Generates all possible variable pair orders (or a subset via the argument `n_perm`) and evaluates each permutation separaty.

For each permutation, decisions are made sequentially, with later pairs conditioned on prior decisions ("I" for included edges, "E" for excluded). Therefore it takes into account its decision for the previous pairs.
In each iteration, for the first pair the LLM is asked to decide on the relation by beong only given the remaining variables but not their relation (similar to the `llmPriorElicit` function). For the second pair, the LLM is asked to decide on the relation by being given the remaining variables and their relation (e.g., "I" or "E"). This is repeated for all pairs in the permutation.

## `llmPriorElicitSimple`

This function evaluates conditional dependencies between variable pairs using an LLM, but without taking the remaining variables (or their relations) explicitely into account.

Evaluates each variable pair independently (`n_rep` times per pair) rather than in sequences.

No conditioning on prior decisions—each pair is assessed in isolation.


Outputs:

All three functions 

- a dataframe (relation_df) with averaged probabilities of edges across permutations.

- the raw LLM outputs (raw_LLM) with full interaction history.
For the `llmPriorElicitRelations` this includes the the final "I/E" sequence for each permutation.

# `betaBinParameter` [still in development]

Takes the output of one of the three llm function and returns the estimated parameters of the Beta-Binomial distribution.
Ideally this should be used when `n_perm` or `n_rep` is set to a value greater than at least 10. 



# Averaging to get the final probabilities: 


For each variable pair (e.g., ("A", "B")):

- `llmPriorElicit` generates  multiple orderings of the remaining variables (e.g., ["C", "D"] vs. ["D", "C"]);
- `llmPriorElicitRelations` generates all possible orderings of the variable pairs (e.g., ["A", "B", "C", "D"], ["B", "A", "C", "D"], etc.);
- `llmPriorElicitSimple` generates multiple repetitions of the same variable pair (e.g., ("A", "B") vs. ("A", "B")).


Each if these is treated as a separate "iteration".

Probability Calculation:

For each iteration, the LLM is queried once (with or without context).

The probabilities of "I" and "E" are summed across all iterations

The final probability is normalized as:

prob = prob(I)/(prob(I) + prob(E))

where prob(.) is the sum of the probabilities for that relation ("I" or "E") across all iterations.
 
If no valid tokens ("I"/"E") are found, it defaults to  probability of 0.5. 