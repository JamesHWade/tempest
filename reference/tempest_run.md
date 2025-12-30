# Run the STORM pipeline

This is a scripted workflow that:

1.  discovers perspectives and research questions,

2.  runs a multi-perspective research loop (search/fetch + expert
    synthesis),

3.  creates an outline,

4.  writes a cited report in Markdown.

## Usage

``` r
tempest_run(
  topic,
  config = tempest_config(),
  retriever = NULL,
  n_experts = 3,
  research_strategy = c("key_questions", "conversation"),
  max_rounds = 3,
  max_questions_per_perspective = 3,
  steps = c("perspectives", "research", "outline", "write", "polish"),
  verbose = TRUE
)
```

## Arguments

- topic:

  Research topic or question.

- config:

  A `TempestConfig`.

- retriever:

  Optional `TempestRetriever`. If `NULL`, created from `config`.

- n_experts:

  Number of expert personas to use (default 3).

- research_strategy:

  Either "key_questions" (default, faster) or "conversation" (more
  thorough but slower). Key questions uses predefined questions;
  conversation dynamically generates follow-up questions.

- max_rounds:

  Maximum rounds per perspective for "conversation" strategy (default
  3).

- max_questions_per_perspective:

  Maximum questions per perspective for "key_questions" strategy
  (default 3).

- steps:

  Character vector controlling which steps to run. Defaults to all.

- verbose:

  If `TRUE`, prints progress messages.

## Value

A list with `title`, `outline`, `draft_md`, `report_md`, and `store`.
