# Perform a bounded HTTP fetch

Perform a bounded HTTP fetch

## Usage

``` r
tempest_http_get(
  url,
  user_agent = NULL,
  timeout_s = getOption("tempest.fetch_timeout_s", 20),
  max_bytes = getOption("tempest.fetch_max_bytes", 10 * 1024^2),
  max_redirects = getOption("tempest.fetch_max_redirects", 5L),
  perform = httr2::req_perform,
  validate = tempest_validate_fetch_url
)
```

## Arguments

- url:

  URL to fetch.

- user_agent:

  Optional HTTP user agent.

- timeout_s:

  Connect and total request timeout in seconds.

- max_bytes:

  Maximum response size in bytes.

- max_redirects:

  Maximum redirects to follow.

- perform:

  Request executor used by deterministic tests.

- validate:

  URL validator used before every request.

## Value

An `httr2_response`.
