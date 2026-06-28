# Suggest follow-up research questions for a topic

Generates a short list of concise, user-facing questions to ask next
about a topic, optionally conditioned on the conversation so far. The
tempest Shiny app uses this to render clickable suggestion cards. The
call is a single tool-free structured completion: any error yields an
empty result.

## Usage

``` r
tempest_suggest_questions(
  topic,
  context = NULL,
  n = 4,
  chat = NULL,
  config = tempest_config()
)
```

## Arguments

- topic:

  The research topic.

- context:

  Optional character string with the recent conversation. When `NULL`,
  questions are generated from the topic alone.

- n:

  Maximum number of questions to return.

- chat:

  Optional ellmer Chat to use. When `NULL`, a chat is created from
  `config` using the coordinator model; the structured call does not
  invoke tools.

- config:

  A `TempestConfig` object.

## Value

A character vector of at most `n` questions (possibly empty).

## Examples

``` r
if (FALSE) { # \dontrun{
tempest_suggest_questions("History of jazz", n = 4)
} # }
```
