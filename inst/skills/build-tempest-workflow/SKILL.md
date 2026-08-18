---
name: build-tempest-workflow
description: "Unavailable in Tempest 0.2; retained only as deletion inventory until T8."
---

# Generic workflow construction is unavailable

Tempest 0.2 has no application-neutral workflow-construction API. Do not use
the retired generic kernel or its references. Use `use-tempest-research` with
`tempest_run()` for scripted STORM research or `tempest_session()` for
interactive Co-STORM research.

This directory remains only so T8 can remove the frozen inventory physically.
