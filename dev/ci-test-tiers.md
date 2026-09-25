# Test tiers

Tempest runs an explicit offline unit tier on every pull request. The unit
allowlist lives in `tools/test-tier.R`; every other `tests/testthat/test-*.R`
file belongs to the full tier. The selector validates its inventory and checks
that the testthat filter ran exactly the allowed files, so renames and new
files cannot silently change the unit set.

Run the tiers from the repository root:

```sh
Rscript tools/test-tier.R validate
Rscript tools/test-tier.R list
Rscript tools/test-tier.R unit
```

The full suite runs after pushes to `main`. A weekly or manually dispatched
`R-CMD-check.yaml` run also executes it on all three release platforms. Add
the `full-tests` label to a pull request to run the release-platform checks,
four full-suite shards, and pinned ecosystem contracts before merge. Removing
the label reruns the ordinary unit gate. The `required` check evaluates the
jobs selected for each event.

The ecosystem workflow uses immutable Deputy, dsprrr, and Graft revisions
from `DESCRIPTION` for reproducible checks. Weekly and manual runs separately
resolve current upstream `main` revisions, record each pin and head SHA in the
job summary, and run the same contracts against those heads. A floating-head
failure is a compatibility investigation; update a pin only after verifying
the new revision.

Only add a file to the unit allowlist after verifying that every test in it
runs without a live provider, network access, or a Deputy-backed research
session. Keep product baseline, persistence, and cross-repository integration
tests in the full tier. Run the full suite locally when changing these paths.
