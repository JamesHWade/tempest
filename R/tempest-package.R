#' @keywords internal
#' @references
#' tempest ports Stanford's STORM and Co-STORM research workflows to R, with
#' optimizable modules built on DSPy (via dsprrr).
#'
#' - STORM: Shao et al. (2024), *Assisting in Writing Wikipedia-like Articles
#'   From Scratch with Large Language Models*, NAACL 2024.
#'   <https://arxiv.org/abs/2402.14207>
#' - Co-STORM: Jiang et al. (2024), *Into the Unknown Unknowns: Engaged Human
#'   Learning through Participation in Language Model Agent Conversations*,
#'   EMNLP 2024. <https://arxiv.org/abs/2408.15232>
#' - DSPy: Khattab et al. (2024), *DSPy: Compiling Declarative Language Model
#'   Calls into Self-Improving Pipelines*, ICLR 2024.
#'   <https://arxiv.org/abs/2310.03714>
#'
#' Upstream implementations that inspired this package:
#' [stanford-oval/storm](https://github.com/stanford-oval/storm),
#' [stanfordnlp/dspy](https://github.com/stanfordnlp/dspy), and
#' [dsprrr](https://github.com/JamesHWade/dsprrr).
"_PACKAGE"

## usethis namespace: start
#' @importFrom R6 R6Class
#' @importFrom rlang %||%
#' @importFrom stats runif setNames
#' @importFrom utils head read.csv
## usethis namespace: end
NULL
