# Suggest follow-up research questions for a topic

Projects a short, deterministic list of user-facing research questions
from a topic and whether prior conversation context is present.
Session-bound suggestions use the session's authoritative
`next_question` program instead.

## Usage

``` r
tempest_suggest_questions(topic, context = NULL, n = 4)
```

## Arguments

- topic:

  The research topic.

- context:

  Optional character string with the recent conversation. When nonempty,
  the first question asks about evidence missing from the current
  discussion.

- n:

  Maximum number of questions to return.

## Value

A character vector of at most `n` questions (possibly empty).

## Examples

``` r
tempest_suggest_questions("History of jazz", n = 4)
#> [1] "What evidence best establishes the key claims about History of jazz?"          
#> [2] "Which uncertainty or tradeoff matters most for understanding History of jazz?" 
#> [3] "What contrasting perspective could change the view of History of jazz?"        
#> [4] "How could the strongest claim about History of jazz be independently verified?"
```
