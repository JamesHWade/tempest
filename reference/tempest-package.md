# tempest: STORM and Co-STORM Research and Writing Agents for R

Implements an R-native clone of the STORM and Co-STORM
research-and-writing workflows using 'ellmer' for LLM orchestration and
'shinychat' for an interactive Shiny UI. Includes web retrieval tools,
evidence tracking with citations, mind-map style topic planning, and
evaluation helpers built on 'vitals'.

## References

tempest ports Stanford's STORM and Co-STORM research workflows to R,
with optimizable modules built on DSPy (via dsprrr).

- STORM: Shao et al. (2024), *Assisting in Writing Wikipedia-like
  Articles From Scratch with Large Language Models*, NAACL 2024.
  <https://arxiv.org/abs/2402.14207>

- Co-STORM: Jiang et al. (2024), *Into the Unknown Unknowns: Engaged
  Human Learning through Participation in Language Model Agent
  Conversations*, EMNLP 2024. <https://arxiv.org/abs/2408.15232>

- DSPy: Khattab et al. (2024), *DSPy: Compiling Declarative Language
  Model Calls into Self-Improving Pipelines*, ICLR 2024.
  <https://arxiv.org/abs/2310.03714>

Upstream implementations that inspired this package:
[stanford-oval/storm](https://github.com/stanford-oval/storm),
[stanfordnlp/dspy](https://github.com/stanfordnlp/dspy), and
[dsprrr](https://github.com/JamesHWade/dsprrr).

## See also

Useful links:

- <https://jameshwade.github.io/tempest/>

- <https://github.com/JamesHWade/tempest>

- Report bugs at <https://github.com/JamesHWade/tempest/issues>

## Author

**Maintainer**: James Wade <github@jameshwade.com>

Authors:

- James Wade <github@jameshwade.com>

Other contributors:

- OpenAI \[copyright holder\]
