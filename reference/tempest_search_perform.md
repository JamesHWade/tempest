# Perform a search-provider request with a timeout and transient-error retry

Search providers are the hottest network path in the pipeline. Without a
timeout a hung connection stalls the whole run, and without retries a
transient 429/5xx aborts it. This wraps
[`httr2::req_perform()`](https://httr2.r-lib.org/reference/req_perform.html)
with both.

## Usage

``` r
tempest_search_perform(req, timeout_s = 30L)
```
