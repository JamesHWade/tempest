---
name: verify-tempest-workflow
description: Verify or review a custom Tempest R workflow for correctness, least privilege, approval integrity, typed artifact provenance, failure handling, cancellation, deterministic tests, and safe persistence or restoration. Use when an agent is asked to audit, test, review, harden, debug, or declare ready a workflow built with `tempest_run_workflow()`, especially after adding custom operations, capabilities, connections, retries, approvals, exporters, artifact codecs, Shiny adapters, or saved-run support.
---

# Verify a Tempest workflow

Treat workflow verification as a contract and security audit, not merely a
happy-path test run. Inspect the actual specifications, runtime factories,
tests, and host integration before reporting readiness.

Read
[references/verification-matrix.md](references/verification-matrix.md) and
select every applicable row before running checks.

## Verification workflow

1. Establish scope and evidence. Identify the workflow spec, operation/runtime
   factory, expert pool, deliverables, policy adapter, connection bindings,
   artifact store/codecs, persistence path, and host UI involved.
2. Validate durable contracts. Check stable IDs and versions, JSON-compatible
   metadata, acyclic dependencies, artifact producer/consumer relationships,
   exact expert assignments, capability references, and completion promises.
3. Verify runtime preflight. Missing operations, wrong kinds, wrong versions,
   absent required capabilities, or invalid expert assignments must fail before
   unauthorized work or side effects.
4. Exercise approvals adversarially. Test pre-step checkpoints and
   post-generation artifact approvals separately. Reject stale, retargeted,
   cross-run, or inconsistent approval decisions.
5. Exercise permission boundaries. Test allowed, denied, retired, missing, and
   over-broad connection cases. Confirm capability factories run only after
   authorization and secrets never enter durable state or events.
6. Inspect artifact integrity. Verify validation results, status, checksum,
   media type, run/step/expert ownership, evidence lineage, parent artifacts,
   and exporter immutability.
7. Exercise retries, failures, and cancellation. Preserve attempt history and
   diagnostics; stop according to failure policy; prevent subsequent side
   effects after cancellation.
8. Exercise persistence. Save a complete bundle, verify inventory and
   checksums, restore with an explicitly rebuilt runtime, narrow permissions,
   inspect before resuming, and test corrupt or partial states.
9. Verify the actual host surface when behavior depends on Shiny reactivity,
   background work, approval UI, downloads, external storage, or authenticated
   connections. Focused unit tests alone do not prove those integrations.
10. Run formatting, focused tests, the relevant broader suite, documentation
    checks, and package checks in proportion to the change.

## Readiness standard

Declare the workflow ready only when:

- every declared output is produced and validated;
- terminal status agrees with approvals and requested deliverables;
- ordered events and public accessors expose enough state for the host;
- authorization and approval failures are explicit and classed;
- no runtime function, client, credential, or secret is serialized;
- save/restore cannot broaden permissions or accept mismatched state;
- tests require no API keys, network, or live provider responses;
- any required live host path has been exercised.

## Report findings

Lead with the outcome. For each defect, report severity, exact file and line,
reproduction, violated contract, and the smallest safe fix. Separate confirmed
defects from unverified risks. If no defect is found, list the checks and
evidence used rather than giving an unsupported approval.

Do not fix findings unless the user requested implementation. When fixes are
in scope, make them and rerun the affected matrix rows before reporting them
resolved.
