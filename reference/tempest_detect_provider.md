# Detect provider from model name

Parses model names like "openai/gpt-4o-mini" or
"anthropic/claude-sonnet" to extract the provider.

## Usage

``` r
tempest_detect_provider(model)
```

## Arguments

- model:

  Model name string

## Value

Provider name (e.g., "openai", "anthropic", "google") or NULL if unknown
