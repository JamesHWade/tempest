# Register default tools on a chat

Registers web search and source/fact management tools on a chat object.
When search_provider is "native" and the provider supports it, uses the
provider's built-in web search. Otherwise uses custom tempest tools.

## Usage

``` r
tempest_register_default_tools(
  chat,
  retriever,
  model = NULL,
  search_provider = "native"
)
```

## Arguments

- chat:

  An ellmer chat object

- retriever:

  A TempestRetriever object

- model:

  Model name (used to detect provider for native tools)

- search_provider:

  Search provider setting from config

## Value

The chat object (invisibly)
