# Save a generic Tempest run bundle

**\[experimental\]**

## Usage

``` r
tempest_run_save(run, path, overwrite = FALSE)
```

## Arguments

- run:

  A `TempestRun`.

- path:

  Destination directory.

- overwrite:

  Whether to atomically replace an existing recognized Tempest run
  bundle.

## Value

The normalized bundle directory, invisibly.

## Details

The bundle contains one canonical run snapshot plus a checksummed
manifest written last. Runtime operations, clients, connection bindings,
policy adapters, callbacks, credentials, and other executable values are
excluded.
