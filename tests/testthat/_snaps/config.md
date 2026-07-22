# tempest_config reports classed provider errors

    Code
      tempest_config(search_provider = "not-a-provider")
    Condition
      Error in `tempest_normalize_search_provider()`:
      ! Unknown search provider: "not-a-provider"
      i Available providers: "native", "wikipedia", "you", "bing", "serper", "brave", "duckduckgo", "tavily", "searxng", "google", and "azure_ai_search"

# tempest_config validates tempest.chat

    Code
      tempest_config()
    Condition
      Error in `tempest_config_abort()`:
      ! The tempest.chat option must be an ellmer Chat or a provider/model string.
      i For example, use `options(tempest.chat = "anthropic/claude-sonnet-4-20250514")`.

