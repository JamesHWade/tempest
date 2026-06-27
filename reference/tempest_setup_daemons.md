# Ensure mirai daemons are available

Returns `TRUE` if daemons are ready, with a `started` attribute
recording whether this call created them (so the caller can tear them
down). Returns `FALSE` if mirai daemons could not be started.

## Usage

``` r
tempest_setup_daemons(n)
```
