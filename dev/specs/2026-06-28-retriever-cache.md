# Retriever cache design

## Context

Tempest repeatedly searches and fetches the same web sources across STORM
research, Co-STORM warmup, expert tools, report generation, and Shiny sessions.
Repeated network calls waste latency, quota, and tokens. The Python STORM
upstream also treats cache as a first-class runtime concern: its `knowledge_storm`
LLM and encoder layers configure a disk-backed LiteLLM cache under
`.storm_local_cache`, and STORM persists source/reference artifacts such as
`url_to_info.json` for reuse across article stages.

Tempest already had an RDS cache directory and used it in
`TempestRetriever$search()` and `$fetch()`, but it lacked explicit package
controls, expiration, force-refresh search semantics, and host-observable hit
rate counters.

## Model

- Cache entries are local RDS files keyed by deterministic hashes.
- Search keys include the logical provider, normalized query, requested result
  count, and non-secret provider options that alter results, such as DuckDuckGo
  region or Bing market/language.
- Fetch keys include the normalized URL and user agent. This avoids reusing
  source content fetched with a different HTTP identity.
- Cache freshness uses file modification time. This keeps existing raw RDS cache
  entries readable and avoids a migration from the previous cache payload shape.
- `cache_ttl = Inf` keeps entries valid until explicit invalidation.
- `cache_enabled = FALSE` bypasses reads and writes while keeping the configured
  cache directory available for ragnar or host storage.
- Search and fetch force refreshes bypass reads but still write the refreshed
  value back to cache.

## Host controls

- `tempest_config(cache_enabled = , cache_ttl = )` controls retriever cache
  behavior for STORM, Co-STORM, and Shiny host apps.
- `TempestRetriever$cache_stats(reset = FALSE)` reports per-retriever search and
  fetch counters: hits, misses, expired entries, read errors, bypasses, and
  writes. Host apps can display hit rates without external telemetry.
- `tempest_cache_clear(cache_dir = NULL, max_age = NULL)` lets users and host
  apps clear a cache directory entirely or prune only stale RDS entries.

## Boundaries

- The cache stores search result tables and source objects, not prompts, model
  responses, API keys, or full session transcripts.
- Cache keys include configuration that changes returned data but never include
  provider API keys.
- The cache does not replace `SourceStore`; cached fetches are re-upserted into
  the live `SourceStore` so citation/source IDs remain stable.
- External observability is still tracked separately. Cache stats provide a
  local telemetry bridge for that future work.

## Verification

- Cache helper tests verify TTL-based lookup and stale-only clearing.
- Retriever tests verify repeated search/fetch hits, force refreshes, disabled
  cache behavior, TTL expiration, and provider-option cache keys.
- Full package checks remain network-free because tests mock search and fetch
  internals.
