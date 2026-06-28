# Register default tools on a chat

Registers web search and source/claim management tools on a chat object.
When search_provider is "native" and the provider supports it, uses the
provider's built-in web search. Otherwise uses custom tempest tools.

## Usage

``` r
tempest_register_default_tools(
  chat,
  retriever,
  model = NULL,
  search_provider = "native",
  allow_claim_writes = TRUE,
  claim_provenance = list()
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

- allow_claim_writes:

  Whether to register claim-writing tools. Read-heavy roles can set this
  to `FALSE` and still inspect sources, claims, evidence, and
  unsupported claims.

- claim_provenance:

  Optional provenance recorded on claims written via `add_claim`.

## Value

The chat object (invisibly)
