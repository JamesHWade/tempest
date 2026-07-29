---
type: Attested Computation
title: Resin margin
description: Net selling price less variable cost.
status: stable
runtime: duckdb
parameters:
- name: segment
  type: string
  required: true
executor:
  resource: references/run-margin.md
  receipt:
  - executed_sql
  - result
attester:
  resource: references/attest-margin.R
generated:
  by: process:finance-nightly
  at: '2026-06-21T02:00:00Z'
---
# Resin margin

## Computation

```sql
SELECT net_price - variable_cost AS resin_margin
FROM segment_margin
WHERE segment = $segment
```
