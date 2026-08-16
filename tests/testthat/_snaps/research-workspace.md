# ResearchWorkspace validates snapshot and accepted references

    Code
      tempest_research_workspace(base_snapshot_id = character())
    Condition
      Error in `tempest_research_workspace_abort()`:
      ! `base_snapshot_id` must be `NULL` or a single non-empty string.

---

    Code
      tempest_research_workspace(base_snapshot_id = " ")
    Condition
      Error in `tempest_research_workspace_abort()`:
      ! `base_snapshot_id` must be `NULL` or a single non-empty string.

---

    Code
      tempest_research_workspace(accepted_graft_references = data.frame())
    Condition
      Error in `tempest_research_workspace_abort()`:
      ! `accepted_graft_references` must be a list of references.

---

    Code
      tempest_research_workspace(accepted_graft_references = list(list(record_id = NA_character_)))
    Condition
      Error in `abort()`:
      ! Accepted graft references cannot contain missing or non-finite values at accepted_graft_references[[1]]$record_id.

---

    Code
      tempest_research_workspace(accepted_graft_references = list(named = list(
        record_id = "record-a")))
    Condition
      Error in `tempest_research_workspace_abort()`:
      ! `accepted_graft_references` must be an unnamed list of references.

# proposed claims cannot become accepted through workspace mutation

    Code
      S7::set_props(claim, accepted = TRUE)
    Condition
      Error:
      ! Can't find property <tempest::tempest_claim>@accepted

---

    Code
      workspace$accepted_graft_references <- list(list(record_id = "proposal"))
    Condition
      Error in `tempest_research_workspace_abort()`:
      ! accepted_graft_references is read-only; use `record_accepted_graft_reference()`.

---

    Code
      workspace$base_snapshot_id <- "another-snapshot"
    Condition
      Error in `tempest_research_workspace_abort()`:
      ! base_snapshot_id is pinned when the workspace is created.

# ResearchWorkspace validates its explicit citation audit

    Code
      workspace$set_citation_audit(list(claim_id = "claim-a"))
    Condition
      Error in `tempest_research_workspace_abort()`:
      ! `citation_audit` must be a data frame or `NULL`.

---

    Code
      workspace$set_citation_audit(tibble::tibble(claim_id = "claim-a"))
    Condition
      Error in `tempest_research_workspace_abort()`:
      ! `citation_audit` must contain exactly the claim-audit fields.
      x Missing fields: claim_text, verification_status, support_score, and rationale.

---

    Code
      workspace$citation_audit <- NULL
    Condition
      Error in `tempest_research_workspace_abort()`:
      ! citation_audit is read-only; use `set_citation_audit()`.
