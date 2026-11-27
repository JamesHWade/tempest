#' ResearchWorkspace (provisional scientific evidence ledger)
#'
#' A mutable, run-scoped workspace for retrieved resources, proposed claims,
#' evidence spans, disputes, and references to accepted graft knowledge. The
#' workspace never grants acceptance to proposed claims; acceptance remains an
#' explicit graft review and commit.
#'
#' @field retrieved_resources Read-only named-list snapshot of typed resources
#'   keyed by resource id.
#' @field proposed_claims Read-only named-list snapshot of provisional claim
#'   records keyed by claim id.
#' @field evidence_spans Read-only named-list snapshot of provisional
#'   evidence-span records.
#' @field claim_supports Read-only named-list snapshot of explicit
#'   claim-by-evidence-span support assessments.
#' @field disputes Read-only named-list snapshot of provisional dispute
#'   records.
#' @field artifact_selection Read-only exact artifact input and provenance.
#' @field citation_audit Read-only pair-level projection of the authoritative
#'   claim-support assessments, when available.
#' @field max_sources Maximum number of unique retrieved resources admitted.
#'   Accepted Graft knowledge records inserted by [tempest_artifact_knowledge()] are
#'   bounded separately and do not count.
#'
#' @keywords internal
ResearchWorkspace <- R6::R6Class(
  "ResearchWorkspace",
  public = list(
    #' @description Create a new provisional research workspace.
    #' @param artifact_selection Exact artifact input retained by the workspace.
    #' @param max_sources Maximum number of unique sources. New sources are
    #'   refused once the limit is reached.
    initialize = function(
      max_sources = Inf,

      artifact_selection = list()
    ) {
      private$mutation_state_value <- "open"
      private$publication_owner_token_value <- NULL
      private$resources_value <- new.env(parent = emptyenv())
      private$claims_value <- new.env(parent = emptyenv())
      private$evidence_spans_value <- new.env(parent = emptyenv())
      private$claim_supports_value <- new.env(parent = emptyenv())
      private$disputes_value <- new.env(parent = emptyenv())
      private$claim_reviews_value <- new.env(parent = emptyenv())
      private$citation_audit_value <- NULL
      private$claims_by_source <- new.env(parent = emptyenv())
      private$artifact_selection_value <- tempest_artifact_selection(
        artifact_selection,
        allow_empty = TRUE
      )
      self$set_max_sources(max_sources)
      invisible(self)
    },

    #' @description Set the maximum number of unique sources.
    #' @param max_sources A positive whole number or `Inf`.
    set_max_sources = function(max_sources) {
      private$assert_mutation_open()
      if (
        !is.numeric(max_sources) ||
          length(max_sources) != 1L ||
          is.na(max_sources) ||
          max_sources <= 0 ||
          (!is.infinite(max_sources) &&
            (!is.finite(max_sources) ||
              max_sources != trunc(max_sources) ||
              max_sources > .Machine$integer.max))
      ) {
        tempest_research_workspace_abort(
          "{.arg max_sources} must be a positive whole number or Inf."
        )
      }
      if (
        is.finite(max_sources) &&
          private$retrieved_source_count() > max_sources
      ) {
        tempest_research_workspace_abort(
          "{.arg max_sources} cannot be lower than the current source count."
        )
      }
      private$max_sources_value <- if (is.infinite(max_sources)) {
        Inf
      } else {
        as.integer(max_sources)
      }
      invisible(self)
    },

    #' @description Insert or update a retrieved typed evidence resource.
    #' @param resource A resource created by [tempest_resource()].
    upsert_retrieved_resource = function(resource) {
      private$assert_mutation_open()
      if (!tempest_is_exact_resource(resource)) {
        tempest_research_workspace_abort(
          "{.arg resource} must be an exact TempestResource record."
        )
      }
      resource <- private$validate_resource(resource)
      resource_id <- resource@resource_id
      previous <- private$resources_value[[resource_id]]
      is_new <- is.null(previous)
      # Accepted Graft records enter through tempest_artifact_knowledge()'s own bounded
      # allowlist and do not consume the retrieval-source budget.
      if (
        is_new &&
          !tempest_is_accepted_knowledge_resource(resource) &&
          private$retrieved_source_count() >= self$max_sources
      ) {
        tempest_research_workspace_abort(
          c(
            "ResearchWorkspace resource limit reached.",
            i = "Increase {.arg max_sources} to admit more resources."
          )
        )
      }
      if (
        !is.null(previous) &&
          (!identical(previous@resource_kind, resource@resource_kind) ||
            !identical(previous@locator, resource@locator))
      ) {
        tempest_research_workspace_abort(
          paste0(
            "A retrieved resource cannot change kind or locator while ",
            "retaining the same resource id."
          )
        )
      }
      resource <- tempest_research_workspace_copy(resource)
      resources_value <- tempest_research_workspace_environment_copy(
        private$resources_value
      )
      resources_value[[resource_id]] <- resource
      for (span_id in ls(private$evidence_spans_value, all.names = TRUE)) {
        private$validate_evidence_span(
          private$evidence_spans_value[[span_id]],
          resources_value = resources_value
        )
      }
      for (claim_id in ls(private$claims_value, all.names = TRUE)) {
        private$validate_proposed_claim(
          private$claims_value[[claim_id]],
          evidence_spans_value = private$evidence_spans_value,
          resources_value = resources_value
        )
      }
      previous_resources <- private$resources_value
      previous_claims <- private$claims_value
      previous_claim_supports <- private$claim_supports_value
      previous_audit <- private$citation_audit_value
      if (!identical(previous, resource)) {
        private$assert_verification_mutable()
      }
      tryCatch(
        {
          private$resources_value <- resources_value
          if (!identical(previous, resource)) {
            private$invalidate_verification_state()
          }
        },
        error = function(error) {
          private$resources_value <- previous_resources
          private$claims_value <- previous_claims
          private$claim_supports_value <- previous_claim_supports
          private$citation_audit_value <- previous_audit
          stop(error)
        }
      )
      invisible(resource_id)
    },

    #' @description Get a retrieved typed evidence resource by id.
    #' @param resource_id Resource id.
    get_retrieved_resource = function(resource_id) {
      resource <- private$resources_value[[resource_id]] %||% NULL
      if (is.null(resource)) {
        return(NULL)
      }
      tempest_research_workspace_copy(resource)
    },

    #' @description Get one retrieved resource as a built-in source view.
    #' @param resource_id Resource id.
    get_retrieved_source = function(resource_id) {
      resource <- self$get_retrieved_resource(resource_id)
      if (is.null(resource)) {
        return(NULL)
      }
      tempest_research_workspace_copy(tempest_resource_as_source(resource))
    },

    #' @description List all retrieved evidence as typed resources.
    list_retrieved_resources = function() {
      ids <- sort(ls(private$resources_value, all.names = TRUE))
      purrr::map(ids, self$get_retrieved_resource)
    },

    #' @description List retrieved resources as built-in source views.
    list_retrieved_sources = function() {
      ids <- sort(ls(private$resources_value, all.names = TRUE))
      purrr::map(ids, self$get_retrieved_source)
    },

    #' @description Add a proposed claim record to the workspace.
    #' @param claim A `tempest_claim` S7 record.
    add_proposed_claim = function(claim) {
      id <- self$add_proposed_claims(list(claim))[[1]]
      invisible(id)
    },

    #' @description Atomically add proposed claim records to the workspace.
    #' @param claims A list of `tempest_claim` S7 records.
    #' @param commit Optional zero-argument callback committed with the batch.
    add_proposed_claims = function(claims, commit = NULL) {
      private$assert_mutation_open()
      if (!is.list(claims) || is.data.frame(claims)) {
        tempest_research_workspace_abort(
          "{.arg claims} must be a list of {.cls tempest_claim} records."
        )
      }
      if (!is.null(commit) && !is.function(commit)) {
        tempest_research_workspace_abort(
          "{.arg commit} must be `NULL` or a function."
        )
      }
      claims <- lapply(claims, private$validate_proposed_claim)
      ids <- vapply(claims, \(claim) claim@claim_id, character(1))
      if (anyDuplicated(ids)) {
        duplicated_ids <- unique(ids[duplicated(ids)])
        tempest_research_workspace_abort(
          "{.arg claims} contains duplicate claim id{?s}: {.val {duplicated_ids}}."
        )
      }

      claims_value <- tempest_research_workspace_environment_copy(
        private$claims_value
      )
      claims_by_source <- tempest_research_workspace_environment_copy(
        private$claims_by_source
      )
      changed <- FALSE
      for (claim in claims) {
        id <- claim@claim_id
        previous <- claims_value[[id]]
        if (!is.null(previous)) {
          for (sid in previous@source_ids) {
            claims_by_source[[sid]] <- setdiff(
              claims_by_source[[sid]] %||% character(),
              id
            )
          }
        }
        claims_value[[id]] <- claim
        for (sid in claim@source_ids) {
          existing <- claims_by_source[[sid]] %||% character()
          claims_by_source[[sid]] <- unique(c(existing, id))
        }
        changed <- changed || !identical(previous, claim)
      }

      if (changed) {
        private$assert_verification_mutable()
        claims_value <- private$unverified_claim_state(claims_value)
        claim_supports_value <- new.env(parent = emptyenv())
        citation_audit <- NULL
      } else {
        claim_supports_value <-
          tempest_research_workspace_environment_copy(
            private$claim_supports_value
          )
        citation_audit <- private$citation_audit_value
      }
      private$commit_claim_state(
        claims_value,
        claims_by_source,
        claim_supports_value,
        citation_audit,
        commit = commit
      )
      invisible(ids)
    },

    #' @description Atomically add extracted evidence spans and claims.
    #' @param claims A list of `tempest_claim` S7 records.
    #' @param evidence_spans A list of `tempest_evidence_span` S7 records.
    #' @param commit Optional zero-argument callback committed with the batch.
    add_extracted_claim_batch = function(
      claims,
      evidence_spans = list(),
      commit = NULL
    ) {
      private$assert_mutation_open()
      if (
        !is.list(claims) ||
          is.data.frame(claims) ||
          !is.null(names(claims))
      ) {
        tempest_research_workspace_abort(
          "{.arg claims} must be an unnamed list of claim records."
        )
      }
      if (
        !is.list(evidence_spans) ||
          is.data.frame(evidence_spans) ||
          !is.null(names(evidence_spans))
      ) {
        tempest_research_workspace_abort(
          paste0(
            "{.arg evidence_spans} must be an unnamed list of evidence-span ",
            "records."
          )
        )
      }
      if (!is.null(commit) && !is.function(commit)) {
        tempest_research_workspace_abort(
          "{.arg commit} must be `NULL` or a function."
        )
      }
      evidence_spans <- lapply(
        evidence_spans,
        private$validate_evidence_span
      )
      span_ids <- vapply(
        evidence_spans,
        \(span) span@evidence_span_id,
        character(1)
      )
      if (anyDuplicated(span_ids)) {
        duplicated_ids <- unique(span_ids[duplicated(span_ids)])
        tempest_research_workspace_abort(
          paste0(
            "{.arg evidence_spans} contains duplicate span id{?s}: ",
            "{.val {duplicated_ids}}."
          )
        )
      }

      evidence_spans_value <- tempest_research_workspace_environment_copy(
        private$evidence_spans_value
      )
      spans_changed <- FALSE
      for (span in evidence_spans) {
        span_id <- span@evidence_span_id
        previous <- evidence_spans_value[[span_id]]
        if (!is.null(previous) && !identical(previous, span)) {
          tempest_research_workspace_abort(
            paste0(
              "Extraction cannot replace evidence span {.val {span_id}} ",
              "with different content."
            )
          )
        }
        evidence_spans_value[[span_id]] <- span
        spans_changed <- spans_changed || is.null(previous)
      }

      claims <- lapply(
        claims,
        private$validate_proposed_claim,
        evidence_spans_value = evidence_spans_value
      )
      claim_ids <- vapply(claims, \(claim) claim@claim_id, character(1))
      if (anyDuplicated(claim_ids)) {
        duplicated_ids <- unique(claim_ids[duplicated(claim_ids)])
        tempest_research_workspace_abort(
          "{.arg claims} contains duplicate claim id{?s}: {.val {duplicated_ids}}."
        )
      }

      claims_value <- tempest_research_workspace_environment_copy(
        private$claims_value
      )
      claims_by_source <- tempest_research_workspace_environment_copy(
        private$claims_by_source
      )
      claims_changed <- FALSE
      for (claim in claims) {
        claim_id <- claim@claim_id
        previous <- claims_value[[claim_id]]
        if (!is.null(previous)) {
          for (source_id in previous@source_ids) {
            claims_by_source[[source_id]] <- setdiff(
              claims_by_source[[source_id]] %||% character(),
              claim_id
            )
          }
        }
        claims_value[[claim_id]] <- claim
        for (source_id in claim@source_ids) {
          existing <- claims_by_source[[source_id]] %||% character()
          claims_by_source[[source_id]] <- unique(c(existing, claim_id))
        }
        claims_changed <- claims_changed || !identical(previous, claim)
      }
      referenced_span_ids <- unique(unlist(
        lapply(
          ls(claims_value, all.names = TRUE),
          \(claim_id) claims_value[[claim_id]]@evidence_span_ids
        ),
        use.names = FALSE
      ))
      unreferenced_span_ids <- setdiff(span_ids, referenced_span_ids)
      if (length(unreferenced_span_ids) > 0L) {
        tempest_research_workspace_abort(
          paste0(
            "Extracted evidence span{?s} must be linked to a claim: ",
            "{.val {unreferenced_span_ids}}."
          )
        )
      }

      if (spans_changed || claims_changed) {
        private$assert_verification_mutable()
        claims_value <- private$unverified_claim_state(claims_value)
        claim_supports_value <- new.env(parent = emptyenv())
        citation_audit <- NULL
      } else {
        claim_supports_value <-
          tempest_research_workspace_environment_copy(
            private$claim_supports_value
          )
        citation_audit <- private$citation_audit_value
      }
      private$commit_extraction_state(
        claims_value,
        claims_by_source,
        evidence_spans_value,
        claim_supports_value,
        citation_audit,
        commit = commit
      )
      invisible(claim_ids)
    },

    #' @description Atomically replace exact claim-by-span support assessments.
    #' @param claim_supports An unnamed complete list of
    #'   `tempest_claim_support` records.
    #' @param min_support_score Bound support threshold used to derive claim
    #'   summaries.
    #' @param verifier Bounded verifier-model identifier or `NA`.
    #' @param verified_at Exact canonical verification-batch timestamp bound
    #'   into every corresponding verification-stage record.
    #' @param .verification_owner_token Internal process-local session
    #'   capability. Standalone workspaces require `NULL`.
    #' @param commit Optional zero-argument callback committed with the batch.
    verify_proposed_claims_batch = function(
      claim_supports,
      verified_at,
      min_support_score = 0.7,
      verifier = NA_character_,
      .verification_owner_token = NULL,
      commit = NULL
    ) {
      private$assert_verification_write(.verification_owner_token)
      if (
        !is.list(claim_supports) ||
          is.data.frame(claim_supports) ||
          !is.null(names(claim_supports))
      ) {
        tempest_research_workspace_abort(
          paste0(
            "{.arg claim_supports} must be an unnamed list of exact ",
            "claim-support records."
          )
        )
      }
      if (!is.null(commit) && !is.function(commit)) {
        tempest_research_workspace_abort(
          "{.arg commit} must be `NULL` or a function."
        )
      }
      owner <- private$verification_owner_token_value
      if (
        (!is.null(owner) &&
          !identical(.verification_owner_token, owner)) ||
          (is.null(owner) && !is.null(.verification_owner_token))
      ) {
        tempest_research_workspace_abort(
          paste0(
            "Session-owned verification requires its exact process-local ",
            "ownership capability."
          )
        )
      }
      min_support_score <- tempest_normalize_min_support_score(
        min_support_score
      )
      if (!tempest_ledger_identifier_valid(verifier, optional = TRUE)) {
        tempest_research_workspace_abort(
          paste0(
            "{.arg verifier} must be `NA` or a bounded credential-free ",
            "identifier."
          )
        )
      }
      if (!tempest_ledger_timestamp_valid(verified_at)) {
        tempest_research_workspace_abort(
          paste0(
            "{.arg verified_at} must be one exact canonical UTC batch ",
            "timestamp."
          )
        )
      }
      claim_supports <- private$validate_claim_support_batch(
        claim_supports,
        min_support_score
      )
      claims_value <- private$verified_claim_state(
        private$claims_value,
        claim_supports,
        verifier,
        verified_at
      )
      claim_supports_value <- new.env(parent = emptyenv())
      for (support in claim_supports) {
        claim_supports_value[[support@claim_support_id]] <-
          tempest_research_workspace_copy(support)
      }
      citation_audit <- tempest_claim_supports_tibble(lapply(
        sort(vapply(
          claim_supports,
          \(support) support@claim_support_id,
          character(1)
        )),
        \(support_id) claim_supports_value[[support_id]]
      ))
      private$commit_verification_state(
        claims_value,
        claim_supports_value,
        citation_audit,
        commit = commit
      )
      invisible(self$citation_audit)
    },

    #' @description Get a claim-support assessment by id.
    #' @param claim_support_id Exact derived support identifier.
    get_claim_support = function(claim_support_id) {
      support <- private$claim_supports_value[[claim_support_id]] %||% NULL
      tempest_research_workspace_copy(support)
    },

    #' @description List claim-support assessments in deterministic id order.
    list_claim_supports = function() {
      ids <- sort(ls(private$claim_supports_value, all.names = TRUE))
      purrr::map(ids, self$get_claim_support)
    },

    #' @description Get a proposed claim by id.
    #' @param claim_id The claim id.
    get_proposed_claim = function(claim_id) {
      claim <- private$claims_value[[claim_id]] %||% NULL
      tempest_research_workspace_copy(claim)
    },

    #' @description List all proposed claims.
    list_proposed_claims = function() {
      ids <- sort(ls(private$claims_value, all.names = TRUE))
      purrr::map(ids, self$get_proposed_claim)
    },

    #' @description Proposed claims that cite a retrieved resource.
    #' @param resource_id Resource id.
    proposed_claims_for_resource = function(resource_id) {
      ids <- sort(private$claims_by_source[[resource_id]] %||% character())
      purrr::map(ids, self$get_proposed_claim)
    },

    #' @description Add an evidence span.
    #' @param span A `tempest_evidence_span` S7 record.
    add_evidence_span = function(span) {
      private$assert_mutation_open()
      span <- private$validate_evidence_span(span)
      id <- span@evidence_span_id
      previous <- private$evidence_spans_value[[id]]
      if (!is.null(previous) && !identical(previous, span)) {
        linked_claim_ids <- vapply(
          self$list_proposed_claims(),
          function(claim) {
            if (id %in% claim@evidence_span_ids) {
              claim@claim_id
            } else {
              NA_character_
            }
          },
          character(1)
        )
        linked_claim_ids <- linked_claim_ids[!is.na(linked_claim_ids)]
        if (length(linked_claim_ids) > 0L) {
          tempest_research_workspace_abort(
            paste0(
              "Cannot replace linked evidence span {.val {id}} with ",
              "different content; it is cited by claim{?s}: ",
              "{.val {linked_claim_ids}}."
            )
          )
        }
      }
      if (!identical(previous, span)) {
        private$assert_verification_mutable()
        private$evidence_spans_value[[id]] <- span
        private$invalidate_verification_state()
      }
      invisible(id)
    },

    #' @description Get an evidence span by id.
    #' @param span_id Evidence span id.
    get_evidence_span = function(span_id) {
      span <- private$evidence_spans_value[[span_id]] %||% NULL
      tempest_research_workspace_copy(span)
    },

    #' @description List all evidence spans in deterministic id order.
    list_evidence_spans = function() {
      ids <- sort(ls(private$evidence_spans_value, all.names = TRUE))
      purrr::map(ids, self$get_evidence_span)
    },

    #' @description Link an evidence span to a claim.
    #' @param claim_id Claim id.
    #' @param span_id Evidence span id.
    link_evidence_to_proposed_claim = function(claim_id, span_id) {
      private$assert_mutation_open()
      claim <- self$get_proposed_claim(claim_id)
      if (is.null(claim)) {
        tempest_research_workspace_abort("Unknown claim id: {.val {claim_id}}.")
      }
      span <- private$evidence_spans_value[[span_id]]
      if (is.null(span)) {
        tempest_research_workspace_abort(
          "Unknown evidence span id: {.val {span_id}}."
        )
      }
      if (is.null(self$get_retrieved_resource(span@source_id))) {
        tempest_research_workspace_abort(
          "Evidence span cites unknown source id: {.val {span@source_id}}."
        )
      }
      if (!span@source_id %in% claim@source_ids) {
        tempest_research_workspace_abort(
          "Evidence span source is not cited by claim {.val {claim_id}}."
        )
      }
      evidence_span_ids <- unique(c(claim@evidence_span_ids, span_id))
      if (!identical(evidence_span_ids, claim@evidence_span_ids)) {
        supporting_quotes <- lapply(
          evidence_span_ids,
          function(evidence_span_id) {
            private$evidence_spans_value[[evidence_span_id]]@quote
          }
        )
        supporting_quotes <- Filter(
          \(quote) !is.na(quote),
          supporting_quotes
        )
        updated <- S7::set_props(
          claim,
          evidence_span_ids = evidence_span_ids,
          supporting_quotes = unname(supporting_quotes)
        )
        updated <- private$validate_proposed_claim(updated)
        private$assert_verification_mutable()
        private$claims_value[[claim_id]] <- updated
        private$invalidate_verification_state()
      }
      invisible(claim_id)
    },

    #' @description Evidence spans linked to a claim.
    #' @param claim_id Claim id.
    get_evidence_for_proposed_claim = function(claim_id) {
      claim <- self$get_proposed_claim(claim_id)
      if (is.null(claim)) {
        return(list())
      }
      purrr::map(claim@evidence_span_ids, self$get_evidence_span)
    },

    #' @description Add a dispute.
    #' @param dispute A `tempest_dispute` S7 record.
    add_dispute = function(dispute) {
      private$assert_mutation_open()
      if (!S7::S7_inherits(dispute, tempest_dispute)) {
        tempest_research_workspace_abort(
          "{.arg dispute} must be a {.cls tempest_dispute} record."
        )
      }
      tryCatch(
        S7::validate(dispute),
        error = function(error) {
          tempest_research_workspace_abort(
            "The dispute failed live validation.",
            parent = error
          )
        }
      )
      missing_claims <- setdiff(
        dispute@claim_ids,
        ls(private$claims_value, all.names = TRUE)
      )
      if (length(missing_claims) > 0L) {
        tempest_research_workspace_abort(
          "Dispute cites unknown claim id{?s}: {.val {missing_claims}}."
        )
      }
      id <- dispute@dispute_id
      previous <- private$disputes_value[[id]]
      dispute <- tempest_research_workspace_copy(dispute)
      if (!identical(previous, dispute)) {
        private$assert_verification_mutable()
        private$disputes_value[[id]] <- dispute
        private$invalidate_verification_state()
      }
      invisible(id)
    },

    #' @description List all disputes.
    list_disputes = function() {
      ids <- sort(ls(private$disputes_value, all.names = TRUE))
      purrr::map(
        ids,
        \(id) tempest_research_workspace_copy(private$disputes_value[[id]])
      )
    },

    #' @description Replace the advisory claim-review records.
    #' @param reviews An unnamed list of exact `TempestClaimReview` values.
    #' @param .verification_owner_token Internal process-local workspace owner.
    #' @param .resources Unnamed exact resources staged with the reviews.
    replace_claim_reviews = function(
      reviews,
      .verification_owner_token = NULL,
      .resources = list()
    ) {
      private$assert_verification_write(.verification_owner_token)
      owner <- private$verification_owner_token_value
      if (
        (!is.null(owner) && !identical(.verification_owner_token, owner)) ||
          (is.null(owner) && !is.null(.verification_owner_token))
      ) {
        tempest_research_workspace_abort(
          "Claim-review mutation requires the exact workspace owner token."
        )
      }
      if (
        !is.list(reviews) ||
          is.data.frame(reviews) ||
          !is.null(names(reviews))
      ) {
        tempest_research_workspace_abort(
          "{.arg reviews} must be an unnamed list of exact TempestClaimReview values."
        )
      }
      if (
        !is.list(.resources) ||
          is.data.frame(.resources) ||
          !is.null(names(.resources))
      ) {
        tempest_research_workspace_abort(
          "{.arg .resources} must be an unnamed list of exact TempestResource values."
        )
      }
      records <- lapply(seq_along(reviews), function(index) {
        review <- reviews[[index]]
        if (!identical(S7::S7_class(review), TempestClaimReview)) {
          tempest_research_workspace_abort(
            "Claim-review entry {.val {index}} must be an exact TempestClaimReview value."
          )
        }
        tryCatch(
          S7::validate(review),
          error = function(error) {
            tempest_research_workspace_abort(
              "Claim-review entry {.val {index}} failed live validation.",
              parent = error
            )
          }
        )
        tempest_claim_review_record(review)
      })
      ids <- vapply(records, `[[`, character(1), "review_id")
      if (anyDuplicated(ids)) {
        tempest_research_workspace_abort(
          "{.arg reviews} cannot contain duplicate review ids."
        )
      }
      claim_ids <- vapply(records, `[[`, character(1), "claim_id")
      if (anyDuplicated(claim_ids)) {
        tempest_research_workspace_abort(
          "{.arg reviews} cannot contain more than one record per claim id."
        )
      }

      resources <- lapply(.resources, private$validate_resource)
      resource_ids <- vapply(
        resources,
        \(resource) resource@resource_id,
        character(1)
      )
      if (anyDuplicated(resource_ids)) {
        tempest_research_workspace_abort(
          "{.arg .resources} cannot contain duplicate resource ids."
        )
      }
      candidate_resources <- tempest_research_workspace_environment_copy(
        private$resources_value
      )
      new_resource_ids <- character()
      for (index in seq_along(resources)) {
        resource <- resources[[index]]
        resource_id <- resource@resource_id
        existing <- private$resources_value[[resource_id]]
        if (!is.null(existing)) {
          if (
            !identical(existing@resource_kind, resource@resource_kind) ||
              !identical(existing@locator, resource@locator) ||
              !identical(existing@content_hash, resource@content_hash) ||
              !identical(existing@content, resource@content)
          ) {
            tempest_research_workspace_abort(
              "A staged review resource conflicts with existing source {.val {resource_id}}."
            )
          }
          next
        }
        if (tempest_is_accepted_knowledge_resource(resource)) {
          tempest_research_workspace_abort(
            "New review-only resources cannot replace or admit accepted Graft knowledge."
          )
        }
        candidate_resources[[resource_id]] <- resource
        new_resource_ids <- c(new_resource_ids, resource_id)
      }
      if (
        length(new_resource_ids) > 0L &&
          is.finite(self$max_sources) &&
          private$retrieved_source_count() + length(new_resource_ids) >
            self$max_sources
      ) {
        tempest_research_workspace_abort(
          "ResearchWorkspace resource limit reached for review-only sources."
        )
      }
      if (length(new_resource_ids) > 0L) {
        for (resource_id in new_resource_ids) {
          referenced <- any(vapply(
            records,
            function(record) {
              dossier_ids <- vapply(
                record$dossier,
                `[[`,
                character(1),
                "source_id"
              )
              resource_id %in%
                record$repair$source_ids &&
                resource_id %in% dossier_ids
            },
            logical(1)
          ))
          if (!referenced) {
            tempest_research_workspace_abort(
              "New review-only source {.val {resource_id}} must appear in a repair result and its dossier."
            )
          }
        }
      }

      candidate <- tempest_research_workspace(
        max_sources = self$max_sources,
        artifact_selection = self$artifact_selection
      )
      candidate_private <- candidate$.__enclos_env__$private
      candidate_private$resources_value <- candidate_resources
      candidate_private$claims_value <-
        tempest_research_workspace_environment_copy(private$claims_value)
      candidate_private$evidence_spans_value <-
        tempest_research_workspace_environment_copy(
          private$evidence_spans_value
        )
      candidate_private$disputes_value <-
        tempest_research_workspace_environment_copy(private$disputes_value)
      candidate_private$claims_by_source <-
        tempest_research_workspace_environment_copy(private$claims_by_source)

      for (index in seq_along(records)) {
        review <- tempest_claim_review(records[[index]])
        tempest_claim_review_validate_workspace(review, candidate)
      }

      claim_reviews_value <- new.env(parent = emptyenv())
      for (index in seq_along(records)) {
        claim_reviews_value[[ids[[index]]]] <- tempest_claim_review(
          records[[index]]
        )
      }
      previous_resources <- private$resources_value
      previous_reviews <- private$claim_reviews_value
      tryCatch(
        {
          private$resources_value <- candidate_resources
          private$claim_reviews_value <- claim_reviews_value
        },
        error = function(error) {
          private$resources_value <- previous_resources
          private$claim_reviews_value <- previous_reviews
          stop(error)
        }
      )
      invisible(self)
    },

    #' @description List advisory claim-review records in deterministic claim order.
    list_claim_reviews = function() {
      ids <- sort(ls(private$claim_reviews_value, all.names = TRUE))
      if (length(ids) > 1L) {
        claim_ids <- vapply(
          ids,
          function(id) {
            tempest_claim_review_record(
              private$claim_reviews_value[[id]]
            )$claim_id
          },
          character(1)
        )
        ids <- ids[order(claim_ids, ids)]
      }
      purrr::map(ids, function(id) {
        tempest_research_workspace_copy(
          private$claim_reviews_value[[id]]
        )
      })
    },

    #' @description Pin an exact artifact selection after admitting its resources.
    #' @param selection A validated artifact selection.
    bind_artifact_selection = function(selection) {
      private$assert_mutation_open()
      selection <- tempest_artifact_selection(selection)
      if (
        length(private$artifact_selection_value) &&
          !identical(private$artifact_selection_value, selection)
      ) {
        tempest_knowledge_abort(
          "A workspace cannot change its pinned artifact selection."
        )
      }
      tempest_artifact_selection_validate(
        selection,
        self$list_retrieved_resources()
      )
      private$artifact_selection_value <- selection
      invisible(self)
    },

    #' @description Validate all authoritative workspace cross-record links.
    validate_integrity = function() {
      tempest_artifact_selection_validate(
        private$artifact_selection_value,
        self$list_retrieved_resources()
      )
      resource_ids <- sort(ls(private$resources_value, all.names = TRUE))
      for (resource_id in resource_ids) {
        resource <- private$validate_resource(
          private$resources_value[[resource_id]]
        )
        if (!identical(resource@resource_id, resource_id)) {
          tempest_research_workspace_abort(
            "Retrieved-resource storage key does not match its resource id."
          )
        }
      }
      span_ids <- sort(ls(private$evidence_spans_value, all.names = TRUE))
      for (span_id in span_ids) {
        span <- private$validate_evidence_span(
          private$evidence_spans_value[[span_id]]
        )
        if (!identical(span@evidence_span_id, span_id)) {
          tempest_research_workspace_abort(
            "Evidence-span storage key does not match its span id."
          )
        }
      }
      claim_ids <- sort(ls(private$claims_value, all.names = TRUE))
      expected_by_source <- new.env(parent = emptyenv())
      for (claim_id in claim_ids) {
        claim <- private$validate_proposed_claim(
          private$claims_value[[claim_id]]
        )
        if (!identical(claim@claim_id, claim_id)) {
          tempest_research_workspace_abort(
            "Claim storage key does not match its claim id."
          )
        }
        for (source_id in claim@source_ids) {
          expected_by_source[[source_id]] <- sort(unique(c(
            expected_by_source[[source_id]] %||% character(),
            claim_id
          )))
        }
      }
      indexed_source_ids <- sort(ls(private$claims_by_source, all.names = TRUE))
      expected_source_ids <- sort(ls(expected_by_source, all.names = TRUE))
      if (!identical(indexed_source_ids, expected_source_ids)) {
        tempest_research_workspace_abort(
          "Claim-by-source index does not match authoritative claims."
        )
      }
      for (source_id in expected_source_ids) {
        if (
          !identical(
            sort(private$claims_by_source[[source_id]]),
            expected_by_source[[source_id]]
          )
        ) {
          tempest_research_workspace_abort(
            "Claim-by-source index contains inconsistent claim ids."
          )
        }
      }
      for (dispute in self$list_disputes()) {
        tryCatch(
          S7::validate(dispute),
          error = function(error) {
            tempest_research_workspace_abort(
              "A dispute failed live validation.",
              parent = error
            )
          }
        )
        missing_claims <- setdiff(dispute@claim_ids, claim_ids)
        if (length(missing_claims) > 0L) {
          tempest_research_workspace_abort(
            "A dispute cites claims absent from the workspace."
          )
        }
      }
      review_ids <- sort(ls(private$claim_reviews_value, all.names = TRUE))
      for (review_id in review_ids) {
        review <- private$claim_reviews_value[[review_id]]
        if (!identical(S7::S7_class(review), TempestClaimReview)) {
          tempest_research_workspace_abort(
            "Claim-review storage must contain exact TempestClaimReview values."
          )
        }
        tryCatch(
          S7::validate(review),
          error = function(error) {
            tempest_research_workspace_abort(
              "A claim-review record failed live validation.",
              parent = error
            )
          }
        )
        review_record <- tempest_claim_review_record(review)
        if (!identical(review_record$review_id, review_id)) {
          tempest_research_workspace_abort(
            "Claim-review storage key does not match its review id."
          )
        }
        tempest_claim_review_validate_workspace(review, self)
      }
      supports <- self$list_claim_supports()
      if (length(supports) > 0L) {
        supports <- private$validate_claim_support_batch(supports)
        supports <- supports[order(vapply(
          supports,
          \(support) support@claim_support_id,
          character(1)
        ))]
        expected_audit <- tempest_claim_supports_tibble(supports)
        if (!identical(private$citation_audit_value, expected_audit)) {
          tempest_research_workspace_abort(
            paste0(
              "Citation audit must exactly project the authoritative ",
              "claim-support records."
            )
          )
        }
        for (claim_id in claim_ids) {
          claim_supports <- Filter(
            \(support) identical(support@claim_id, claim_id),
            supports
          )
          expected <- private$aggregate_claim_support(claim_supports)
          claim <- private$claims_value[[claim_id]]
          if (
            !identical(claim@verification_status, expected$status) ||
              !isTRUE(all.equal(
                claim@support_score,
                expected$score,
                check.attributes = FALSE
              )) ||
              is.na(claim@verified_at)
          ) {
            tempest_research_workspace_abort(
              "Claim verification summary does not match its support records."
            )
          }
        }
      } else {
        if (
          !is.null(private$citation_audit_value) &&
            nrow(private$citation_audit_value) != 0L
        ) {
          tempest_research_workspace_abort(
            "Citation audit cannot exist without claim-support records."
          )
        }
        stale <- vapply(
          self$list_proposed_claims(),
          \(claim) !identical(claim@verification_status, "unverified"),
          logical(1)
        )
        if (any(stale)) {
          tempest_research_workspace_abort(
            "Verified claim summaries require exact claim-support records."
          )
        }
      }
      invisible(self)
    },

    #' @description Convert sources, claims, and disputes to tibbles.
    to_tibbles = function() {
      s <- self$list_retrieved_sources()
      resources <- if (length(s) == 0) {
        tibble::tibble(
          id = character(),
          resource_kind = character(),
          locator = character(),
          url = character(),
          title = character(),
          media_type = character(),
          snippet = character(),
          content_text = character(),
          context_text = character(),
          fetched_at = character(),
          meta = list()
        )
      } else {
        tibble::tibble(
          id = purrr::map_chr(s, "id"),
          resource_kind = purrr::map_chr(
            s,
            function(source) {
              source$meta$resource_kind %||% "web"
            }
          ),
          locator = purrr::map_chr(
            s,
            function(source) {
              source$meta$locator %||% source$url %||% NA_character_
            }
          ),
          url = purrr::map_chr(s, "url"),
          title = purrr::map_chr(s, ~ .x$title %||% NA_character_),
          media_type = purrr::map_chr(
            s,
            function(source) {
              source$meta$media_type %||% "text/html"
            }
          ),
          snippet = purrr::map_chr(s, tempest_source_snippet_text),
          content_text = purrr::map_chr(
            s,
            ~ .x$content_text %||% NA_character_
          ),
          context_text = purrr::map_chr(s, tempest_source_context_text),
          fetched_at = purrr::map_chr(s, ~ .x$fetched_at %||% NA_character_),
          meta = purrr::map(s, ~ .x$meta %||% list())
        )
      }
      list(
        retrieved_resources = resources,
        proposed_claims = tempest_claims_tibble(self$list_proposed_claims()),
        claim_supports = tempest_claim_supports_tibble(
          self$list_claim_supports()
        )
      )
    }
  ),
  active = list(
    retrieved_resources = function(value) {
      if (!missing(value)) {
        private$read_only_binding("retrieved_resources")
      }
      tempest_research_workspace_values(private$resources_value)
    },
    proposed_claims = function(value) {
      if (!missing(value)) {
        private$read_only_binding("proposed_claims")
      }
      tempest_research_workspace_values(private$claims_value)
    },
    evidence_spans = function(value) {
      if (!missing(value)) {
        private$read_only_binding("evidence_spans")
      }
      tempest_research_workspace_values(private$evidence_spans_value)
    },
    claim_supports = function(value) {
      if (!missing(value)) {
        private$read_only_binding("claim_supports")
      }
      tempest_research_workspace_values(private$claim_supports_value)
    },
    disputes = function(value) {
      if (!missing(value)) {
        private$read_only_binding("disputes")
      }
      tempest_research_workspace_values(private$disputes_value)
    },
    max_sources = function(value) {
      if (!missing(value)) {
        tempest_research_workspace_abort(
          paste0(
            "{.field max_sources} is read-only; use ",
            "{.fn set_max_sources}."
          )
        )
      }
      private$max_sources_value
    },

    artifact_selection = function(value) {
      if (!missing(value)) {
        private$read_only_binding("artifact_selection")
      }
      tempest_research_workspace_copy(private$artifact_selection_value)
    },

    citation_audit = function(value) {
      if (!missing(value)) {
        private$read_only_binding("citation_audit")
      }
      if (is.null(private$citation_audit_value)) {
        return(NULL)
      }
      rlang::duplicate(private$citation_audit_value, shallow = FALSE)
    }
  ),
  private = list(
    retrieved_source_count = function() {
      sum(
        !vapply(
          self$list_retrieved_resources(),
          tempest_is_accepted_knowledge_resource,
          logical(1)
        )
      )
    },
    mutation_state_value = "open",
    publication_owner_token_value = NULL,
    resources_value = NULL,
    claims_value = NULL,
    evidence_spans_value = NULL,
    claim_supports_value = NULL,
    disputes_value = NULL,
    claim_reviews_value = NULL,
    max_sources_value = NULL,
    claims_by_source = NULL,

    artifact_selection_value = list(),

    citation_audit_value = NULL,
    verification_owner_token_value = NULL,
    assert_mutation_open = function() {
      if (!identical(private$mutation_state_value, "open")) {
        tempest_research_workspace_abort(
          paste0(
            "ResearchWorkspace is read-only during or after terminal ",
            "product publication."
          )
        )
      }
      invisible(NULL)
    },
    assert_verification_write = function(owner) {
      state <- private$mutation_state_value
      if (identical(state, "sealed")) {
        tempest_research_workspace_abort(
          "A sealed ResearchWorkspace cannot be verified again."
        )
      }
      if (
        identical(state, "publication_locked") &&
          !identical(owner, private$publication_owner_token_value)
      ) {
        tempest_research_workspace_abort(
          paste0(
            "Only the exact publication owner may verify a locked ",
            "ResearchWorkspace."
          )
        )
      }
      if (!state %in% c("open", "publication_locked")) {
        tempest_research_workspace_abort(
          "ResearchWorkspace mutation state is invalid."
        )
      }
      invisible(NULL)
    },
    validate_resource = function(resource) {
      if (!tempest_is_exact_resource(resource)) {
        tempest_research_workspace_abort(
          "Retrieved resources must be exact TempestResource records."
        )
      }
      tryCatch(
        {
          S7::validate(resource)
          tempest_resource_data(resource, include_content = TRUE)
        },
        error = function(error) {
          tempest_research_workspace_abort(
            "A retrieved resource failed live validation.",
            parent = error
          )
        }
      )
      if (!is.null(resource@content)) {
        expected_hash <- tempest_product_content_hash(
          resource@content,
          resource@media_type
        )
        if (
          is.na(resource@content_hash) ||
            !identical(resource@content_hash, expected_hash)
        ) {
          tempest_research_workspace_abort(
            paste0(
              "Retrieved-resource content hash must exactly match its ",
              "captured inline content."
            )
          )
        }
      }
      tempest_research_workspace_copy(resource)
    },
    validate_proposed_claim = function(
      claim,
      evidence_spans_value = private$evidence_spans_value,
      resources_value = private$resources_value
    ) {
      if (!S7::S7_inherits(claim, tempest_claim)) {
        tempest_research_workspace_abort(
          "{.arg claims} must contain only {.cls tempest_claim} records."
        )
      }
      tryCatch(
        S7::validate(claim),
        error = function(error) {
          tempest_research_workspace_abort(
            "A proposed claim failed live validation.",
            parent = error
          )
        }
      )
      missing_sources <- setdiff(
        claim@source_ids,
        ls(resources_value, all.names = TRUE)
      )
      if (length(missing_sources) > 0L) {
        tempest_research_workspace_abort(
          "Claim cites unknown source id{?s}: {.val {missing_sources}}."
        )
      }
      missing_contradicting_sources <- setdiff(
        claim@contradicting_source_ids,
        ls(resources_value, all.names = TRUE)
      )
      if (length(missing_contradicting_sources) > 0L) {
        tempest_research_workspace_abort(
          paste0(
            "Claim cites unknown contradicting source id{?s}: ",
            "{.val {missing_contradicting_sources}}."
          )
        )
      }
      missing_spans <- setdiff(
        claim@evidence_span_ids,
        ls(evidence_spans_value, all.names = TRUE)
      )
      if (length(missing_spans) > 0L) {
        tempest_research_workspace_abort(
          "Claim cites unknown evidence span id{?s}: {.val {missing_spans}}."
        )
      }
      mismatched_spans <- claim@evidence_span_ids[
        vapply(
          claim@evidence_span_ids,
          function(span_id) {
            !evidence_spans_value[[span_id]]@source_id %in%
              claim@source_ids
          },
          logical(1)
        )
      ]
      if (length(mismatched_spans) > 0L) {
        tempest_research_workspace_abort(
          paste0(
            "Claim evidence span{?s} must come from a source cited by the ",
            "claim: {.val {mismatched_spans}}."
          )
        )
      }
      supporting_quotes <- claim@supporting_quotes
      valid_quotes <- is.list(supporting_quotes) &&
        !is.data.frame(supporting_quotes) &&
        is.null(names(supporting_quotes)) &&
        all(vapply(supporting_quotes, rlang::is_string, logical(1)))
      if (!valid_quotes) {
        tempest_research_workspace_abort(
          "Claim supporting quotes must be a flat unnamed string array."
        )
      }
      expected_quotes <- lapply(
        claim@evidence_span_ids,
        function(span_id) evidence_spans_value[[span_id]]@quote
      )
      expected_quotes <- Filter(
        \(quote) !is.na(quote),
        expected_quotes
      )
      if (!identical(supporting_quotes, unname(expected_quotes))) {
        tempest_research_workspace_abort(
          paste0(
            "Claim supporting quotes must exactly match its referenced ",
            "quoted evidence spans in order."
          )
        )
      }
      tempest_research_workspace_copy(claim)
    },
    validate_evidence_span = function(
      span,
      resources_value = private$resources_value
    ) {
      if (!S7::S7_inherits(span, tempest_evidence_span)) {
        tempest_research_workspace_abort(
          paste0(
            "{.arg evidence_spans} must contain only ",
            "{.cls tempest_evidence_span} records."
          )
        )
      }
      tryCatch(
        S7::validate(span),
        error = function(error) {
          tempest_research_workspace_abort(
            "An evidence span failed live validation.",
            parent = error
          )
        }
      )
      resource <- resources_value[[span@source_id]]
      if (is.null(resource)) {
        tempest_research_workspace_abort(
          "Evidence span cites unknown source id: {.val {span@source_id}}."
        )
      }
      if (!is.na(span@quote)) {
        if (!nzchar(span@quote)) {
          tempest_research_workspace_abort(
            "Quoted evidence spans must contain a non-empty quote."
          )
        }
        source <- tempest_resource_as_source(resource)
        captured_text <- c(
          source$content_text %||% NA_character_,
          source$snippet %||% NA_character_,
          source$context_text %||% NA_character_
        )
        captured_text <- unique(captured_text[
          !is.na(captured_text) & nzchar(captured_text)
        ])
        if (length(captured_text) == 0L) {
          tempest_research_workspace_abort(
            paste0(
              "Evidence-span quote cannot be validated because source ",
              "{.val {span@source_id}} has no captured text."
            )
          )
        }
        has_offsets <- !is.na(span@start_offset)
        matched <- any(vapply(
          captured_text,
          function(text) {
            if (!has_offsets) {
              return(grepl(span@quote, text, fixed = TRUE))
            }
            span@end_offset <= nchar(text, type = "chars") &&
              identical(
                substr(text, span@start_offset + 1L, span@end_offset),
                span@quote
              )
          },
          logical(1)
        ))
        if (!matched) {
          tempest_research_workspace_abort(
            paste0(
              "Evidence-span quote and zero-based half-open offsets are not ",
              "captured by source {.val {span@source_id}}."
            )
          )
        }
      }
      tempest_research_workspace_copy(span)
    },
    validate_claim_support = function(
      support,
      claims_value = private$claims_value,
      evidence_spans_value = private$evidence_spans_value,
      resources_value = private$resources_value
    ) {
      if (!S7::S7_inherits(support, TempestClaimSupport)) {
        tempest_research_workspace_abort(
          paste0(
            "{.arg claim_supports} must contain only exact ",
            "{.cls tempest_claim_support} records."
          )
        )
      }
      tryCatch(
        S7::validate(support),
        error = function(error) {
          tempest_research_workspace_abort(
            "A claim-support assessment failed live validation."
          )
        }
      )
      claim <- claims_value[[support@claim_id]]
      if (is.null(claim)) {
        tempest_research_workspace_abort(
          "Claim support cites unknown claim id: {.val {support@claim_id}}."
        )
      }
      span <- evidence_spans_value[[support@evidence_span_id]]
      if (is.null(span)) {
        tempest_research_workspace_abort(
          paste0(
            "Claim support cites unknown evidence-span id: ",
            "{.val {support@evidence_span_id}}."
          )
        )
      }
      if (!support@evidence_span_id %in% claim@evidence_span_ids) {
        tempest_research_workspace_abort(
          "Claim support binds an evidence span not cited by its claim."
        )
      }
      if (
        !identical(support@source_id, span@source_id) ||
          !support@source_id %in% claim@source_ids ||
          is.null(resources_value[[support@source_id]])
      ) {
        tempest_research_workspace_abort(
          paste0(
            "Claim-support source must exactly match the authoritative span ",
            "and cited claim source."
          )
        )
      }
      if (is.na(span@quote) || !nzchar(span@quote)) {
        tempest_research_workspace_abort(
          "Claim support requires a non-empty captured evidence-span quote."
        )
      }
      private$validate_evidence_span(
        span,
        resources_value = resources_value
      )
      tempest_research_workspace_copy(support)
    },
    validate_claim_support_batch = function(
      supports,
      min_support_score = NULL,
      claims_value = private$claims_value,
      evidence_spans_value = private$evidence_spans_value,
      resources_value = private$resources_value
    ) {
      supports <- lapply(
        supports,
        private$validate_claim_support,
        claims_value = claims_value,
        evidence_spans_value = evidence_spans_value,
        resources_value = resources_value
      )
      support_ids <- vapply(
        supports,
        \(support) support@claim_support_id,
        character(1)
      )
      if (anyDuplicated(support_ids)) {
        tempest_research_workspace_abort(
          "Claim-support batches cannot contain duplicate pair identities."
        )
      }
      claim_ids <- sort(ls(claims_value, all.names = TRUE))
      expected_ids <- character()
      for (claim_id in claim_ids) {
        claim <- claims_value[[claim_id]]
        if (length(claim@evidence_span_ids) == 0L) {
          tempest_research_workspace_abort(
            paste0(
              "Every verified claim requires at least one exact evidence ",
              "span: {.val {claim_id}}."
            )
          )
        }
        span_sources <- vapply(
          claim@evidence_span_ids,
          \(span_id) evidence_spans_value[[span_id]]@source_id,
          character(1)
        )
        if (!setequal(unique(span_sources), claim@source_ids)) {
          tempest_research_workspace_abort(
            paste0(
              "Every cited claim source requires at least one exact evidence ",
              "span: {.val {claim_id}}."
            )
          )
        }
        expected_ids <- c(
          expected_ids,
          vapply(
            claim@evidence_span_ids,
            \(span_id) tempest_claim_support_id(claim_id, span_id),
            character(1)
          )
        )
      }
      if (!setequal(support_ids, expected_ids)) {
        tempest_research_workspace_abort(
          paste0(
            "{.arg claim_supports} must exactly cover every current ",
            "claim-by-evidence-span pair."
          )
        )
      }
      if (!is.null(min_support_score)) {
        min_support_score <- tempest_normalize_min_support_score(
          min_support_score
        )
        below_threshold <- vapply(
          supports,
          function(support) {
            identical(support@verification_status, "supported") &&
              (is.na(support@support_score) ||
                support@support_score < min_support_score)
          },
          logical(1)
        )
        if (any(below_threshold)) {
          tempest_research_workspace_abort(
            paste0(
              "Supported claim-span assessments must satisfy the exact ",
              "verification threshold."
            )
          )
        }
      }
      supports[match(expected_ids, support_ids)]
    },
    aggregate_claim_support = function(supports) {
      tempest_claim_support_aggregate(supports)
    },
    verified_claim_state = function(
      claims_value,
      supports,
      verifier,
      verified_at
    ) {
      claims_value <- tempest_research_workspace_environment_copy(claims_value)
      claim_ids <- sort(ls(claims_value, all.names = TRUE))
      for (claim_id in claim_ids) {
        claim_supports <- Filter(
          \(support) identical(support@claim_id, claim_id),
          supports
        )
        aggregate <- private$aggregate_claim_support(claim_supports)
        claims_value[[claim_id]] <- S7::set_props(
          claims_value[[claim_id]],
          verification_status = aggregate$status,
          support_score = aggregate$score,
          verifier_model = verifier,
          verified_at = verified_at
        )
      }
      claims_value
    },
    unverified_claim_state = function(claims_value) {
      claims_value <- tempest_research_workspace_environment_copy(claims_value)
      for (claim_id in ls(claims_value, all.names = TRUE)) {
        claims_value[[claim_id]] <- S7::set_props(
          claims_value[[claim_id]],
          verification_status = "unverified",
          support_score = NA_real_,
          verified_at = NA_character_,
          verifier_model = NA_character_
        )
      }
      claims_value
    },
    commit_claim_state = function(
      claims_value,
      claims_by_source,
      claim_supports_value,
      citation_audit,
      commit = NULL
    ) {
      previous_claims <- private$claims_value
      previous_claims_by_source <- private$claims_by_source
      previous_claim_supports <- private$claim_supports_value
      previous_citation_audit <- private$citation_audit_value
      tryCatch(
        {
          private$claims_value <- claims_value
          private$claims_by_source <- claims_by_source
          private$claim_supports_value <- claim_supports_value
          private$citation_audit_value <- citation_audit
          if (!is.null(commit)) {
            commit()
          }
        },
        error = function(error) {
          private$claims_value <- previous_claims
          private$claims_by_source <- previous_claims_by_source
          private$claim_supports_value <- previous_claim_supports
          private$citation_audit_value <- previous_citation_audit
          stop(error)
        }
      )
      invisible(NULL)
    },
    commit_extraction_state = function(
      claims_value,
      claims_by_source,
      evidence_spans_value,
      claim_supports_value,
      citation_audit,
      commit = NULL
    ) {
      previous_claims <- private$claims_value
      previous_claims_by_source <- private$claims_by_source
      previous_evidence_spans <- private$evidence_spans_value
      previous_claim_supports <- private$claim_supports_value
      previous_citation_audit <- private$citation_audit_value
      tryCatch(
        {
          private$claims_value <- claims_value
          private$claims_by_source <- claims_by_source
          private$evidence_spans_value <- evidence_spans_value
          private$claim_supports_value <- claim_supports_value
          private$citation_audit_value <- citation_audit
          if (!is.null(commit)) {
            commit()
          }
        },
        error = function(error) {
          private$claims_value <- previous_claims
          private$claims_by_source <- previous_claims_by_source
          private$evidence_spans_value <- previous_evidence_spans
          private$claim_supports_value <- previous_claim_supports
          private$citation_audit_value <- previous_citation_audit
          stop(error)
        }
      )
      invisible(NULL)
    },
    commit_verification_state = function(
      claims_value,
      claim_supports_value,
      citation_audit,
      commit = NULL
    ) {
      previous_claims <- private$claims_value
      previous_claim_supports <- private$claim_supports_value
      previous_citation_audit <- private$citation_audit_value
      tryCatch(
        {
          private$claims_value <- claims_value
          private$claim_supports_value <- claim_supports_value
          private$citation_audit_value <- citation_audit
          if (!is.null(commit)) {
            commit()
          }
        },
        error = function(error) {
          private$claims_value <- previous_claims
          private$claim_supports_value <- previous_claim_supports
          private$citation_audit_value <- previous_citation_audit
          stop(error)
        }
      )
      invisible(NULL)
    },
    assert_verification_mutable = function() {
      if (length(ls(private$claim_supports_value, all.names = TRUE)) > 0L) {
        tempest_research_workspace_abort(
          paste0(
            "Verified ResearchWorkspace evidence is sealed; current ",
            "claim-support proof cannot be invalidated or superseded."
          )
        )
      }
      invisible(NULL)
    },
    invalidate_verification_state = function() {
      private$assert_verification_mutable()
      private$claims_value <- private$unverified_claim_state(
        private$claims_value
      )
      private$claim_supports_value <- new.env(parent = emptyenv())
      private$citation_audit_value <- NULL
      invisible(NULL)
    },
    read_only_binding = function(field) {
      tempest_research_workspace_abort(
        paste0(
          "{.field {field}} is a read-only snapshot; use workspace ",
          "mutation methods."
        )
      )
    }
  )
)

#' Create a provisional research workspace
#'
#' `tempest_research_workspace()` creates the run-scoped ledger for material
#' gathered or proposed during scientific research. Accepted knowledge remains
#' in its authoritative store; this workspace retains exact selection
#' references and materialized evidence for inspection. Reuse requires fresh
#' host admission.
#'
#' @param artifact_selection Exact artifact input retained by the workspace.
#' @param max_sources Maximum number of unique resources admitted.
#'
#' @return A [ResearchWorkspace] object. Supply it as a custom retriever's
#'   `$workspace` so Tempest and the host write to the same research ledger.
#' @examples
#' workspace <- tempest_research_workspace(max_sources = 10L)
#' inherits(workspace, "ResearchWorkspace")
#' @export
tempest_research_workspace <- function(
  max_sources = Inf,

  artifact_selection = list()
) {
  ResearchWorkspace$new(
    max_sources = max_sources,

    artifact_selection = artifact_selection
  )
}
