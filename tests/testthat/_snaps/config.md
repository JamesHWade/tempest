# tempest_config reports classed provider errors

    Code
      tempest_config(search_provider = "not-a-provider")
    Condition
      Error in `tempest_normalize_search_provider()`:
      ! Unknown search provider: "not-a-provider"
      i Available providers: "native", "wikipedia", "you", "bing", "serper", "brave", "duckduckgo", "tavily", "searxng", "google", and "azure_ai_search"

