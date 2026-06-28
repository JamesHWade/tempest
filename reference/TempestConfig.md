# TempestConfig (S7)

Holds configuration for STORM / Co-STORM sessions: LLM models, prompts,
retrieval settings, cache paths, and citation policy.

## Usage

``` r
TempestConfig(
  models = list(),
  params = list(),
  chat_fn = function() NULL,
  tools = NULL,
  embed_fn = function() NULL,
  ragnar_store = NULL,
  search_provider = "native",
  cache_dir = NA_character_,
  max_search_results = 8,
  max_search_queries_per_turn = 3L,
  retrieve_top_k = 25L,
  max_sources = 24,
  user_agent = "tempest (R; +https://github.com/JamesHWade/tempest)",
  node_expansion_trigger_count = NULL,
  enable_discourse_manager = FALSE,
  max_active_experts = 5L,
  enable_unseen_surfacing = FALSE,
  citation_policy = "source_attributed",
  min_support_score = 0.7,
  on_unsupported_claim = "flag"
)
```
