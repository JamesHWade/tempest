# Get provider-native web tools

Returns the appropriate web search/fetch tools for a given provider.
These are ellmer's built-in tools that use provider-native APIs.

## Usage

``` r
tempest_get_native_web_tools(provider)
```

## Arguments

- provider:

  Provider name ("openai", "anthropic", "google")

## Value

List of ellmer tools, or NULL if provider doesn
