# Kata to GitHub migration

GitHub Issues is the active backlog for `JamesHWade/tempest` as of
September 19, 2026. Graft and scans already used GitHub and required no
tracker migration.

## Preserved records

- 98 issues: 2 open and 96 closed at import.
- 93 comments, embedded in each issue's expandable original-history section.
- Original authors, timestamps, labels, ownership names, issue IDs, and closure
  or reopening evidence. GitHub creation dates identify the import time.
- 50 parent links and 14 blocking links represented natively in GitHub.
- All 136 original relationships, including 72 related links, recorded as
  clickable references in the migrated issue bodies.

Historical text is preserved; it can describe superseded plans and APIs.
The state column below records the source state at import. Follow the GitHub
issue for its current state and acceptance criteria.

The original Kata project is retained as an archived audit record. The
[machine-readable map](kata-to-github.json) records the source export digest
and the complete issue-ID mapping. Nothing was deleted or marked complete
merely because it moved trackers.

## Issue map

| Kata ID | GitHub issue | State at import |
| --- | --- | --- |
| `03d5` | [#174: Validate Tempest against current Graft consumer contracts](https://github.com/JamesHWade/tempest/issues/174) | closed |
| `04zh` | [#164: T1: add the research manifest and ResearchWorkspace](https://github.com/JamesHWade/tempest/issues/164) | closed |
| `0mpk` | [#173: Make the daily briefing change signal structural against the pinned Graft snapshot](https://github.com/JamesHWade/tempest/issues/173) | closed |
| `0wh9` | [#86: Improve Tempest Shiny chat controls, observability, and caching](https://github.com/JamesHWade/tempest/issues/86) | closed |
| `16dv` | [#139: Isolate Shiny session persistence from the server filesystem](https://github.com/JamesHWade/tempest/issues/139) | closed |
| `1fxn` | [#121: Fix STORM progress streaming: nothing shows until the run finishes](https://github.com/JamesHWade/tempest/issues/121) | closed |
| `2nzw` | [#101: Clarify Tempest exported API lifecycle and tidyverse naming contracts](https://github.com/JamesHWade/tempest/issues/101) | closed |
| `2zbg` | [#112: Show Co-STORM progress icons during warmup, not only after it finishes](https://github.com/JamesHWade/tempest/issues/112) | closed |
| `4303` | [#142: Make tempest_run_async genuinely non-blocking](https://github.com/JamesHWade/tempest/issues/142) | closed |
| `4cc0` | [#94: Add integration tests for Shiny Co-STORM workflows](https://github.com/JamesHWade/tempest/issues/94) | closed |
| `4fn5` | [#104: Instrument scripted STORM stages with progress events](https://github.com/JamesHWade/tempest/issues/104) | closed |
| `5ag4` | [#126: Design session persistence and resume for Tempest sessions](https://github.com/JamesHWade/tempest/issues/126) | closed |
| `5gfh` | [#117: Design evidence-oriented tool calls for STORM and Co-STORM agents](https://github.com/JamesHWade/tempest/issues/117) | closed |
| `67h9` | [#141: Enforce SourceStore referential and index integrity](https://github.com/JamesHWade/tempest/issues/141) | closed |
| `7f9q` | [#108: Add a host-neutral workflow progress state reducer](https://github.com/JamesHWade/tempest/issues/108) | closed |
| `7st5` | [#168: Remove redundant release-triggered CI runs](https://github.com/JamesHWade/tempest/issues/168) | closed |
| `9c6a` | [#100: Modernize Tempest errors, warnings, and messages with cli and rlang](https://github.com/JamesHWade/tempest/issues/100) | closed |
| `a3rg` | [#113: Redesign Co-STORM progress phases around the actual session workflow](https://github.com/JamesHWade/tempest/issues/113) | closed |
| `an7p` | [#166: Make STORM and Co-STORM product paths authoritative in T7](https://github.com/JamesHWade/tempest/issues/166) | closed |
| `arqh` | [#140: Make session bundle replacement atomic and manifest-verified](https://github.com/JamesHWade/tempest/issues/140) | closed |
| `b2cg` | [#152: Build an application-neutral Tempest workflow kernel](https://github.com/JamesHWade/tempest/issues/152) | closed |
| `b77g` | [#111: Fix Co-STORM facts and sources population regression](https://github.com/JamesHWade/tempest/issues/111) | closed |
| `b8wy` | [#136: Stop pinning shinychat to merged chat-server branch](https://github.com/JamesHWade/tempest/issues/136) | closed |
| `bavj` | [#90: Add OpenTelemetry or local Logfire observability](https://github.com/JamesHWade/tempest/issues/90) | closed |
| `c3dw` | [#95: Make Tempest capabilities reusable across host apps](https://github.com/JamesHWade/tempest/issues/95) | closed |
| `c8jk` | [#118: Make dsprrr claim extraction source-context aware](https://github.com/JamesHWade/tempest/issues/118) | closed |
| `cax6` | [#150: Make retriever caching atomic and retry transient failures](https://github.com/JamesHWade/tempest/issues/150) | closed |
| `cheg` | [#159: Ship agent skills for custom Tempest workflows](https://github.com/JamesHWade/tempest/issues/159) | closed |
| `cxpv` | [#170: Demonstrate a cohesive daily briefing and repair public API documentation](https://github.com/JamesHWade/tempest/issues/170) | closed |
| `da4c` | [#138: Harden Shiny rendering against untrusted HTML and URL injection](https://github.com/JamesHWade/tempest/issues/138) | closed |
| `df36` | [#93: Add placeholders for empty Shiny cards](https://github.com/JamesHWade/tempest/issues/93) | closed |
| `dq0v` | [#125: Link inline citations and references with hover preview and navigation](https://github.com/JamesHWade/tempest/issues/125) | closed |
| `dq90` | [#175: Reconcile subscription authentication with ellmer 0.5 and current Graft](https://github.com/JamesHWade/tempest/issues/175) | closed |
| `dqyv` | [#167: Fix Shiny settings drawer keyboard accessibility](https://github.com/JamesHWade/tempest/issues/167) | closed |
| `e08d` | [#107: Render STORM and Co-STORM workflow progress in Shiny from events](https://github.com/JamesHWade/tempest/issues/107) | closed |
| `e9kn` | [#143: Make strict citation actions operate on unsupported claims](https://github.com/JamesHWade/tempest/issues/143) | closed |
| `ec43` | [#102: Normalize Tempest code style, dependency usage, and package tests](https://github.com/JamesHWade/tempest/issues/102) | closed |
| `eq7b` | [#124: Add an interactive references panel synced to cited sources](https://github.com/JamesHWade/tempest/issues/124) | closed |
| `fmbv` | [#132: Populate facts support score column](https://github.com/JamesHWade/tempest/issues/132) | closed |
| `fp34` | [#169: Start the STORM test file first in parallel CI](https://github.com/JamesHWade/tempest/issues/169) | closed |
| `fr54` | [#156: Generalize evidence resources and host adapters](https://github.com/JamesHWade/tempest/issues/156) | closed |
| `fwmt` | [#178: Accept Graft artifact consumer contracts 0.7 and 0.8](https://github.com/JamesHWade/tempest/issues/178) | closed |
| `g7wt` | [#103: Define a reusable Tempest workflow progress event contract](https://github.com/JamesHWade/tempest/issues/103) | closed |
| `gcg1` | [#151: Add an accessible text and keyboard view for the mind map](https://github.com/JamesHWade/tempest/issues/151) | closed |
| `hfcd` | [#160: Improve pkgdown landing lists and document Agent Skills](https://github.com/JamesHWade/tempest/issues/160) | closed |
| `jj3y` | [#172: Adopt Deputy drop-in Chat for governed daily briefings](https://github.com/JamesHWade/tempest/issues/172) | closed |
| `jj5e` | [#128: Add TempestSession snapshot and restore helpers](https://github.com/JamesHWade/tempest/issues/128) | closed |
| `k0hh` | [#99: Align Tempest with tidyverse package design and style conventions](https://github.com/JamesHWade/tempest/issues/99) | closed |
| `k483` | [#158: Add a package getting-started vignette](https://github.com/JamesHWade/tempest/issues/158) | closed |
| `k67p` | [#122: Improve reference and inline citation display and interaction](https://github.com/JamesHWade/tempest/issues/122) | closed |
| `kqgx` | [#149: Add deterministic contract coverage for exported and provider adapters](https://github.com/JamesHWade/tempest/issues/149) | closed |
| `msg3` | [#119: Rename agent-facing fact tools to claim tools](https://github.com/JamesHWade/tempest/issues/119) | closed |
| `my3y` | [#120: Add evidence review tools for sources, claims, and support spans](https://github.com/JamesHWade/tempest/issues/120) | closed |
| `n64q` | [#131: Add bundled Shiny session save, load, and autosave](https://github.com/JamesHWade/tempest/issues/131) | closed |
| `pgp9` | [#123: Render inline citation markers as formatted numbered citations](https://github.com/JamesHWade/tempest/issues/123) | closed |
| `pjsd` | [#83: Simplify Tempest external API surface](https://github.com/JamesHWade/tempest/issues/83) | open |
| `pkd5` | [#89: Add rich chat footer controls and runtime status](https://github.com/JamesHWade/tempest/issues/89) | closed |
| `pyxm` | [#148: Move Co-STORM enrichment and report work off the Shiny main loop](https://github.com/JamesHWade/tempest/issues/148) | closed |
| `q8zc` | [#109: Replace the default chat robot with a Tempest app icon](https://github.com/JamesHWade/tempest/issues/109) | closed |
| `qngb` | [#106: Instrument Co-STORM session and expert activity with progress events](https://github.com/JamesHWade/tempest/issues/106) | closed |
| `qx4q` | [#144: Quarantine or cancel timed-out Co-STORM warmup chats](https://github.com/JamesHWade/tempest/issues/144) | closed |
| `ryfx` | [#91: Cache web searches and fetched sources like STORM](https://github.com/JamesHWade/tempest/issues/91) | closed |
| `s1r4` | [#165: Prove four-package provenance in T6 shadow mode](https://github.com/JamesHWade/tempest/issues/165) | closed |
| `serz` | [#161: Add configurable default chat clients](https://github.com/JamesHWade/tempest/issues/161) | closed |
| `sfda` | [#116: Fix STORM Shiny worker failing on tempest_progress_collector export](https://github.com/JamesHWade/tempest/issues/116) | closed |
| `srvc` | [#180: Adopt shared Graft artifacts and decisions in research admission](https://github.com/JamesHWade/tempest/issues/180) | closed |
| `svyx` | [#114: Tune Co-STORM answer endings and suggested next steps for the session purpose](https://github.com/JamesHWade/tempest/issues/114) | closed |
| `swyb` | [#135: Add tooltips to Shiny chat footer actions](https://github.com/JamesHWade/tempest/issues/135) | closed |
| `t593` | [#145: Cancel STORM workers when users stop or disconnect](https://github.com/JamesHWade/tempest/issues/145) | closed |
| `t5zn` | [#87: Populate Tempest chat slash commands](https://github.com/JamesHWade/tempest/issues/87) | closed |
| `tc8q` | [#97: Expose extension points for Tempest experts, retrievers, and artifact stores](https://github.com/JamesHWade/tempest/issues/97) | closed |
| `tpqx` | [#127: Add SourceStore snapshot and restore helpers](https://github.com/JamesHWade/tempest/issues/127) | closed |
| `ttjw` | [#157: Document reusable Tempest workflows for package users](https://github.com/JamesHWade/tempest/issues/157) | closed |
| `v2tz` | [#162: Migrate Tempest to the 0.2 scientific research product architecture](https://github.com/JamesHWade/tempest/issues/162) | closed |
| `v659` | [#163: T0: Freeze Tempest architecture and capture deterministic baseline](https://github.com/JamesHWade/tempest/issues/163) | closed |
| `v6kb` | [#134: Evaluate tidyverse data-dict for Tempest schemas](https://github.com/JamesHWade/tempest/issues/134) | closed |
| `v8wn` | [#177: Preserve complete accepted briefing evidence across restart and correction](https://github.com/JamesHWade/tempest/issues/177) | closed |
| `vbg7` | [#137: Close SSRF and unbounded-fetch gaps in URL retrieval](https://github.com/JamesHWade/tempest/issues/137) | closed |
| `vtvt` | [#153: Add generic Tempest workflow and run state](https://github.com/JamesHWade/tempest/issues/153) | closed |
| `vtz9` | [#115: Audit session_id propagation for Co-STORM expert tool calls](https://github.com/JamesHWade/tempest/issues/115) | closed |
| `w3fm` | [#133: Populate source snippet and context_text columns](https://github.com/JamesHWade/tempest/issues/133) | closed |
| `wedz` | [#96: Extract a host-agnostic Tempest run/event layer](https://github.com/JamesHWade/tempest/issues/96) | closed |
| `wgr8` | [#154: Add expert skills and scoped capability resolution](https://github.com/JamesHWade/tempest/issues/154) | closed |
| `wpt9` | [#105: Add host-neutral progress sinks and replay helpers](https://github.com/JamesHWade/tempest/issues/105) | closed |
| `wwt0` | [#85: Explore deputy-backed expert orchestration for Co-STORM](https://github.com/JamesHWade/tempest/issues/85) | closed |
| `x3wk` | [#179: Support the tested Graft 1.0 consumer contract](https://github.com/JamesHWade/tempest/issues/179) | closed |
| `xbwy` | [#155: Add objective, deliverable, and typed artifact contracts](https://github.com/JamesHWade/tempest/issues/155) | closed |
| `xenk` | [#130: Persist and restore Tempest progress-event history](https://github.com/JamesHWade/tempest/issues/130) | closed |
| `xkrx` | [#176: Coordinate scans compatibility with the prerelease version reset](https://github.com/JamesHWade/tempest/issues/176) | closed |
| `y2kw` | [#147: Validate and enforce Tempest runtime budgets](https://github.com/JamesHWade/tempest/issues/147) | closed |
| `yb8s` | [#146: Preserve existing ragnar stores unless reset is explicit](https://github.com/JamesHWade/tempest/issues/146) | closed |
| `yf9v` | [#92: Fix sources and facts not populating in Co-STORM sessions](https://github.com/JamesHWade/tempest/issues/92) | closed |
| `yfwh` | [#88: Evaluate moving sidebar settings into a footer modal](https://github.com/JamesHWade/tempest/issues/88) | closed |
| `z0e0` | [#98: Add an embeddable Tempest app adapter and example host](https://github.com/JamesHWade/tempest/issues/98) | closed |
| `zb9y` | [#110: Show persona-specific icons for expert and deputy calls](https://github.com/JamesHWade/tempest/issues/110) | closed |
| `zf9y` | [#84: Prove contradictory research and retire native Graft integrations](https://github.com/JamesHWade/tempest/issues/84) | open |
| `zj62` | [#129: Add Tempest session save and resume bundle APIs](https://github.com/JamesHWade/tempest/issues/129) | closed |
| `zyas` | [#171: Migrate Tempest to Deputy constructor session IDs](https://github.com/JamesHWade/tempest/issues/171) | closed |
