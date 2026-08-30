test_that("report renders verification badges under claim_verified", {
  store <- fake_store_with_sources(1)
  s1 <- store$list_retrieved_sources()[[1]]$id
  claim <- tempest_claim(
    claim_text = "A questionable sentence",
    source_ids = s1,
    verification_status = "supported",
    support_score = 0.9
  )
  store$add_proposed_claim(claim)
  fake_verify_claim_supports(store, list(claim))
  body <- paste0("A verified sentence [", s1, "].")
  md <- tempest:::tempest_report_md_render(
    "Title",
    body,
    store,
    citation_policy = "claim_verified"
  )
  expect_match(md, "✓") # check mark badge for supported
})

test_that("citation helpers consume a ResearchWorkspace directly", {
  workspace <- tempest_research_workspace()
  source <- fake_source("https://example.org/workspace")
  workspace$upsert_retrieved_resource(source)
  claim <- tempest_claim(
    claim_text = "Workspace evidence is provisional.",
    source_ids = source@resource_id
  )
  workspace$add_proposed_claim(claim)

  expect_equal(
    tempest:::tempest_workspace_sources(workspace)$id,
    source@resource_id
  )
  expect_equal(
    tempest:::tempest_workspace_claims(workspace)$claim_id,
    claim@claim_id
  )
  expect_match(
    tempest:::tempest_report_md_render(
      "Workspace report",
      paste0("Workspace evidence [", source@resource_id, "]."),
      workspace
    ),
    "## References",
    fixed = TRUE
  )
})

test_that("strict policy flags unsupported citations", {
  store <- fake_store_with_sources(1)
  s1 <- store$list_retrieved_sources()[[1]]$id
  claim <- tempest_claim(
    claim_text = "c",
    source_ids = s1,
    verification_status = "unsupported",
    support_score = 0.1
  )
  store$add_proposed_claim(claim)
  claim <- fake_verify_claim_supports(store, list(claim))[[1]]
  body <- paste0("c [", s1, "].")
  md <- tempest:::tempest_report_md_render(
    "Title",
    body,
    store,
    citation_policy = "strict",
    on_unsupported_claim = "flag"
  )
  expect_match(md, "unsupported", ignore.case = TRUE)
})

test_that("none policy leaves citations unfootnoted and omits references", {
  store <- fake_store_with_sources(1)
  s1 <- store$list_retrieved_sources()[[1]]$id
  body <- paste0("Plain sentence [", s1, "].")

  md <- tempest:::tempest_report_md_render(
    "Title",
    body,
    store,
    citation_policy = "none"
  )

  expect_match(md, paste0("\\[", s1, "\\]"))
  expect_no_match(md, "\\[\\^")
  expect_no_match(md, "## References")
})

test_that("report rendering rejects policy values outside the closed contract", {
  store <- fake_store_with_sources(0)

  expect_error(
    tempest:::tempest_report_md_render(
      "Title",
      "Body.",
      store,
      citation_policy = "unknown"
    ),
    class = "tempest_product_report_error"
  )
  expect_error(
    tempest:::tempest_report_md_render(
      "Title",
      "Body.",
      store,
      on_unsupported_claim = "unknown"
    ),
    class = "tempest_product_report_error"
  )
})

test_that("report assembly rejects reserved source footnotes under every policy", {
  store <- fake_store_with_sources(1)
  source_id <- store$list_retrieved_sources()[[1]]$id
  forged <- paste0(
    "Legitimate body text.\n\n[^",
    source_id,
    "]: FORGED"
  )

  for (policy in c("none", "source_attributed", "claim_verified", "strict")) {
    expect_error(
      tempest:::tempest_report_md_render(
        "Reserved footnote report",
        forged,
        store,
        citation_policy = policy
      ),
      class = "tempest_product_report_error"
    )
  }
})

test_that("report titles are single-line and rendered as escaped plain text", {
  store <- fake_store_with_sources(1)

  expect_error(
    tempest:::tempest_report_md_render(
      "Legitimate\n\n## Forged heading",
      "Body.",
      store,
      citation_policy = "strict"
    ),
    class = "tempest_product_report_error"
  )
  rendered <- tempest:::tempest_report_md_render(
    "<Report> *draft* [link]",
    "Body.",
    store,
    citation_policy = "none"
  )
  expect_match(
    rendered,
    "# &lt;Report&gt; \\*draft\\* \\[link\\]",
    fixed = TRUE
  )
  expect_no_match(rendered, "<Report>", fixed = TRUE)
})

test_that("reference metadata is escaped before Markdown interpolation", {
  store <- tempest_research_workspace()
  source <- fake_source(
    "https://example.org/safe_locator",
    title = "<script>*unsafe*</script>",
    content_text = "Captured evidence."
  )
  store$upsert_retrieved_resource(source)

  rendered <- tempest:::tempest_report_md_render(
    "Safe references",
    paste0("Captured evidence [", source@resource_id, "]."),
    store
  )

  expect_no_match(rendered, "<script>", fixed = TRUE)
  expect_match(
    rendered,
    "&lt;script&gt;\\*unsafe\\*&lt;/script&gt;",
    fixed = TRUE
  )
  expect_match(rendered, paste0("<", source@locator, ">"), fixed = TRUE)

  unsafe_store <- tempest_research_workspace()
  unsafe_source <- fake_source(
    "https://example.org/<unsafe>",
    title = "Unsafe locator",
    content_text = "Captured evidence."
  )
  unsafe_store$upsert_retrieved_resource(unsafe_source)
  expect_error(
    tempest:::tempest_report_md_render(
      "Unsafe locator",
      paste0("Captured evidence [", unsafe_source@resource_id, "]."),
      unsafe_store
    ),
    class = "tempest_product_report_error"
  )
})

test_that("strict drop suppresses footnotes for dropped citations", {
  store <- fake_store_with_sources(1)
  s1 <- store$list_retrieved_sources()[[1]]$id
  claim <- tempest_claim(
    claim_text = "Unsupported claim",
    source_ids = s1,
    verification_status = "unsupported",
    support_score = 0.1
  )
  store$add_proposed_claim(claim)
  claim <- fake_verify_claim_supports(store, list(claim))[[1]]
  body <- paste0("Unsupported claim [", s1, "].")

  md <- tempest:::tempest_report_md_render(
    "Title",
    body,
    store,
    citation_policy = "strict",
    on_unsupported_claim = "drop"
  )

  expect_no_match(md, paste0("\\[\\^", s1, "\\]"))
  expect_no_match(md, paste0("\\[\\^", s1, "\\]:"))
  expect_no_match(md, "## References")
  expect_no_match(md, "Unsupported claim", fixed = TRUE)
})

test_that("strict policy applies status to the matching cited claim", {
  store <- fake_store_with_sources(1)
  s1 <- store$list_retrieved_sources()[[1]]$id
  claims <- list(
    tempest_claim(
      claim_text = "Supported claim",
      source_ids = s1,
      verification_status = "supported",
      support_score = 0.9
    ),
    tempest_claim(
      claim_text = "Unsupported claim",
      source_ids = s1,
      verification_status = "unsupported",
      support_score = 0.1
    )
  )
  lapply(claims, store$add_proposed_claim)
  claims <- fake_verify_claim_supports(store, claims)
  body <- paste0("Supported claim [", s1, "]. Unsupported claim [", s1, "].")

  md <- tempest:::tempest_report_md_render(
    "Title",
    body,
    store,
    citation_policy = "strict",
    on_unsupported_claim = "drop"
  )

  expect_match(md, paste0("Supported claim \\[\\^", s1, "\\]\\."))
  expect_no_match(md, "Unsupported claim", fixed = TRUE)
  expect_match(md, paste0("\\[\\^", s1, "\\]:"))
})

test_that("strict policy derives multi-span support independent of pair order", {
  store <- fake_store_with_sources(1)
  source <- store$list_retrieved_sources()[[1]]
  spans <- list(
    tempest_evidence_span(
      source_id = source$id,
      quote = source$content_text,
      extracted_by = "test::strict-extractor",
      evidence_span_id = "span.strict.first"
    ),
    tempest_evidence_span(
      source_id = source$id,
      quote = source$content_text,
      extracted_by = "test::strict-extractor",
      evidence_span_id = "span.strict.second"
    )
  )
  claim <- tempest_claim(
    "Layered evidence is mixed",
    source_ids = source$id,
    evidence_span_ids = vapply(
      spans,
      \(span) span@evidence_span_id,
      character(1)
    ),
    supporting_quotes = lapply(spans, \(span) span@quote),
    claim_id = "claim.strict.multi-span"
  )
  store$add_extracted_claim_batch(list(claim), spans)
  claim <- store$get_proposed_claim(claim@claim_id)
  spans <- spans[order(vapply(
    spans,
    \(span) {
      tempest:::tempest_claim_support_id(
        claim@claim_id,
        span@evidence_span_id
      )
    },
    character(1)
  ))]
  supports <- list(
    test_claim_support(
      claim,
      spans[[1]],
      status = "supported",
      score = 0.9
    ),
    test_claim_support(
      claim,
      spans[[2]],
      status = "partially_supported",
      score = 0.8
    )
  )
  body <- paste0(claim@claim_text, " [", source$id, "].")

  reports <- lapply(list(supports, rev(supports)), function(batch) {
    store$verify_proposed_claims_batch(
      batch,
      verified_at = "2026-08-16T12:03:00Z",
      min_support_score = 0.7,
      verifier = "test::strict-verifier"
    )
    claim <- store$get_proposed_claim(claim@claim_id)
    expect_identical(claim@verification_status, "partially_supported")
    expect_identical(claim@support_score, 0.8)
    expect_identical(
      store$citation_audit$verification_status[[1]],
      "supported"
    )
    expect_identical(store$citation_audit$support_score[[1]], 0.9)
    tempest:::tempest_report_md_render(
      "Title",
      body,
      store,
      citation_policy = "strict",
      min_support_score = 0.7
    )
  })

  expect_identical(reports[[1]], reports[[2]])

  supported <- list(
    test_claim_support(claim, spans[[1]], score = 0.9),
    test_claim_support(claim, spans[[2]], score = 0.8)
  )
  store$verify_proposed_claims_batch(
    rev(supported),
    verified_at = "2026-08-16T12:03:00Z",
    min_support_score = 0.7,
    verifier = "test::strict-verifier"
  )
  thresholded <- tempest:::tempest_report_md_render(
    "Title",
    body,
    store,
    citation_policy = "strict",
    on_unsupported_claim = "flag",
    min_support_score = 0.85
  )
  expect_match(thresholded, "unsupported citation", fixed = TRUE)
})

test_that("strict revise replaces unsupported assertions distinctly", {
  store <- fake_store_with_sources(1)
  source_id <- store$list_retrieved_sources()[[1]]$id
  claim <- tempest_claim(
    claim_text = "Unsupported assertion",
    source_ids = source_id,
    verification_status = "unsupported",
    support_score = 0.1
  )
  store$add_proposed_claim(claim)
  claim <- fake_verify_claim_supports(store, list(claim))[[1]]
  body <- paste0("Unsupported assertion [", source_id, "].")

  revised <- tempest:::tempest_report_md_render(
    "Title",
    body,
    store,
    citation_policy = "strict",
    on_unsupported_claim = "revise"
  )

  expect_no_match(revised, "Unsupported assertion", fixed = TRUE)
  expect_match(revised, "withheld pending revision", fixed = TRUE)
  expect_no_match(revised, "## References", fixed = TRUE)
})

test_that("strict policy refuses publication without completed verification", {
  store <- fake_store_with_sources(1)
  source_id <- store$list_retrieved_sources()[[1]]$id
  store$add_proposed_claim(tempest_claim(
    claim_text = "Unverified assertion",
    source_ids = source_id
  ))

  expect_error(
    tempest:::tempest_report_md_render(
      "Title",
      paste0("Unverified assertion [", source_id, "]."),
      store,
      citation_policy = "strict"
    ),
    class = "tempest_product_report_error"
  )
})

test_that("strict policy requires each citation to bind to exact support", {
  store <- fake_store_with_sources(2)
  source_ids <- vapply(
    store$list_retrieved_sources(),
    `[[`,
    character(1),
    "id"
  )
  claim <- tempest_claim(
    claim_text = "Audited assertion",
    source_ids = source_ids[[1]],
    verification_status = "supported",
    support_score = 0.9
  )
  store$add_proposed_claim(claim)

  expect_error(
    tempest:::tempest_report_md_render(
      "Title",
      paste0("Unbound assertion [", source_ids[[2]], "]."),
      store,
      citation_policy = "strict"
    ),
    class = "tempest_product_report_error"
  )
})

test_that("strict policy rejects citation-free reports when claims exist", {
  store <- fake_store_with_sources(1)
  source_id <- store$list_retrieved_sources()[[1]]$id
  store$add_proposed_claim(tempest_claim(
    claim_text = "A report claim",
    source_ids = source_id,
    verification_status = "supported",
    support_score = 0.9
  ))

  expect_error(
    tempest:::tempest_report_md_render(
      "Title",
      "A citation-free report body.",
      store,
      citation_policy = "strict"
    ),
    class = "tempest_product_report_error"
  )
})

test_that("strict policy rejects citation-free reports with an empty workspace", {
  expect_error(
    tempest:::tempest_report_md_render(
      "Title",
      "A citation-free report body.",
      tempest_research_workspace(),
      citation_policy = "strict"
    ),
    class = "tempest_product_report_error"
  )
})

test_that("strict policy binds every assertion and factual heading exactly", {
  store <- fake_store_with_sources(1)
  source_id <- store$list_retrieved_sources()[[1]]$id
  claim <- tempest_claim(
    "Aspirin reduces fever",
    source_ids = source_id,
    verification_status = "supported",
    support_score = 0.9
  )
  store$add_proposed_claim(claim)
  claim <- fake_verify_claim_supports(store, list(claim))[[1]]
  cited <- paste0("Aspirin reduces fever [", source_id, "].")

  expect_no_error(tempest:::tempest_report_md_render(
    "Title",
    paste("## Evidence", cited, sep = "\n\n"),
    store,
    citation_policy = "strict"
  ))
  for (body in c(
    paste("## Cancer is cured", cited, sep = "\n\n"),
    paste0(cited, " Aspirin cures cancer."),
    paste(cited, "- Aspirin cures cancer.", sep = "\n"),
    paste0("Aspirin reduces fever and cures cancer [", source_id, "].")
  )) {
    expect_error(
      tempest:::tempest_report_md_render(
        "Title",
        body,
        store,
        citation_policy = "strict"
      ),
      class = "tempest_product_report_error"
    )
  }
})

test_that("strict briefing publication treats each governed item as one assertion", {
  store <- fake_store_with_sources(1)
  source_id <- store$list_retrieved_sources()[[1]]$id
  claim <- tempest_claim(
    "A U.S. rule becomes effective today.",
    source_ids = source_id,
    verification_status = "supported",
    support_score = 0.9
  )
  store$add_proposed_claim(claim)
  claim <- fake_verify_claim_supports(store, list(claim))[[1]]
  item <- tempest:::TempestBriefingItem(
    kind = "observation",
    text = claim@claim_text,
    claim_ids = claim@claim_id
  )
  body <- paste(
    "## Evidence focus",
    tempest:::tempest_briefing_items_markdown(
      list(item),
      store
    ),
    sep = "\n\n"
  )

  expect_no_error(tempest:::tempest_report_md_render(
    "Title",
    body,
    store,
    citation_policy = "strict"
  ))
  expect_error(
    tempest:::tempest_report_md_render(
      "Title",
      sub(
        "## Evidence focus",
        "## Evidence focus: Revenue increased 18%",
        body,
        fixed = TRUE
      ),
      store,
      citation_policy = "strict"
    ),
    class = "tempest_product_report_error"
  )
  expect_error(
    tempest:::tempest_report_md_render(
      "Title",
      paste(body, "An unbound assertion.", sep = "\n\n"),
      store,
      citation_policy = "strict"
    ),
    class = "tempest_product_report_error"
  )
})

test_that("briefing items render each source as a citation token", {
  store <- fake_store_with_sources(2)
  source_ids <- vapply(
    store$list_retrieved_sources(),
    `[[`,
    character(1),
    "id"
  )
  claims <- list(
    tempest_claim(
      "Joint evidence",
      source_ids = source_ids,
      verification_status = "supported",
      support_score = 0.9
    ),
    tempest_claim(
      "The permit schedule remains unchanged since the prior review",
      source_ids = source_ids,
      verification_status = "supported",
      support_score = 0.9
    )
  )
  for (claim in claims) {
    store$add_proposed_claim(claim)
  }
  claims <- fake_verify_claim_supports(store, claims)
  items <- list(
    tempest:::TempestBriefingItem(
      kind = "observation",
      text = claims[[1L]]@claim_text,
      claim_ids = claims[[1L]]@claim_id
    ),
    tempest:::TempestBriefingItem(
      kind = "no_change",
      text = claims[[2L]]@claim_text,
      claim_ids = claims[[2L]]@claim_id,
      confidence = "high"
    )
  )
  body <- tempest:::tempest_briefing_items_markdown(items, store)
  citations <- paste0("[", source_ids, "]", collapse = " ")

  citation_matches <- gregexpr(citations, body, fixed = TRUE)[[1L]]
  expect_length(citation_matches[citation_matches != -1L], 2L)
  report <- tempest:::tempest_report_md_render(
    "Title",
    body,
    store,
    citation_policy = "strict"
  )
  footnotes <- paste0("[^", source_ids, "]", collapse = " ")
  footnote_matches <- gregexpr(footnotes, report, fixed = TRUE)[[1L]]
  expect_length(footnote_matches[footnote_matches != -1L], 2L)
  for (source_id in source_ids) {
    expect_match(report, paste0("[^", source_id, "]:"), fixed = TRUE)
  }
})

test_that("briefing items preserve ordinary sentence-level validation", {
  store <- fake_store_with_sources(1)
  source_id <- store$list_retrieved_sources()[[1]]$id
  claims <- lapply(
    c(
      "A U.S. rule becomes effective today.",
      "The first ordinary sentence is verified",
      "The second ordinary sentence is verified"
    ),
    \(text) {
      tempest_claim(
        text,
        source_ids = source_id,
        verification_status = "supported",
        support_score = 0.9
      )
    }
  )
  for (claim in claims) {
    store$add_proposed_claim(claim)
  }
  claims <- fake_verify_claim_supports(store, claims)
  item <- tempest:::TempestBriefingItem(
    kind = "observation",
    text = claims[[1L]]@claim_text,
    claim_ids = claims[[1L]]@claim_id
  )
  ordinary <- paste0(
    claims[[2L]]@claim_text,
    " [",
    source_id,
    "]. ",
    claims[[3L]]@claim_text,
    " [",
    source_id,
    "]."
  )
  body <- paste(
    "## Evidence focus",
    tempest:::tempest_briefing_items_markdown(list(item), store),
    ordinary,
    sep = "\n\n"
  )

  expect_no_error(tempest:::tempest_report_md_render(
    "Title",
    body,
    store,
    citation_policy = "strict"
  ))
})

test_that("strict publication requires exact captured source evidence", {
  store <- tempest_research_workspace()
  source <- tempest_resource(
    resource_kind = "web",
    locator = "https://example.org/strict-empty-source",
    title = "Empty source",
    media_type = "text/html"
  )
  store$upsert_retrieved_resource(source)
  claim <- tempest_claim(
    "Uncaptured assertion",
    source_ids = source@resource_id,
    verification_status = "supported",
    support_score = 0.9
  )
  store$add_proposed_claim(claim)

  expect_error(
    tempest:::tempest_report_md_render(
      "Uncaptured report",
      paste0("Uncaptured assertion [", source@resource_id, "]."),
      store,
      citation_policy = "strict"
    ),
    class = "tempest_product_report_error"
  )
})

test_that("strict policy rejects malformed and partial multi-source citations", {
  store <- fake_store_with_sources(2)
  source_ids <- vapply(
    store$list_retrieved_sources(),
    `[[`,
    character(1),
    "id"
  )
  claim <- tempest_claim(
    "Joint evidence",
    source_ids = source_ids,
    verification_status = "supported",
    support_score = 0.9
  )
  store$add_proposed_claim(claim)
  claim <- fake_verify_claim_supports(store, list(claim))[[1]]

  expect_error(
    tempest:::tempest_report_md_render(
      "Title",
      paste0("Joint evidence [", source_ids[[1]], "]."),
      store,
      citation_policy = "strict"
    ),
    class = "tempest_product_report_error"
  )
  expect_error(
    tempest:::tempest_report_md_render(
      "Title",
      paste0(
        "Joint evidence [",
        paste(source_ids, collapse = "] ["),
        "] [Sforged-source]."
      ),
      store,
      citation_policy = "strict"
    ),
    class = "tempest_product_report_error"
  )
})

test_that("final report validation rebinds canonical rendered citations", {
  store <- fake_store_with_sources(1)
  source_id <- store$list_retrieved_sources()[[1]]$id
  claim <- tempest_claim(
    "Canonical assertion",
    source_ids = source_id,
    verification_status = "supported",
    support_score = 0.9
  )
  store$add_proposed_claim(claim)
  claim <- fake_verify_claim_supports(store, list(claim))[[1]]
  report <- tempest:::tempest_report_md_render(
    "Title",
    paste0("Canonical assertion [", source_id, "]."),
    store,
    citation_policy = "strict"
  )

  expect_no_error(tempest:::tempest_final_report_validate(
    report,
    store,
    title = "Title",
    citation_policy = "strict",
    on_unsupported_claim = "flag",
    min_support_score = 0.7
  ))
  expect_error(
    tempest:::tempest_final_report_validate(
      sub("Canonical assertion", "Forged assertion", report, fixed = TRUE),
      store,
      title = "Title",
      citation_policy = "strict",
      on_unsupported_claim = "flag",
      min_support_score = 0.7
    ),
    class = "tempest_product_report_error"
  )
})

test_that("final report validation accepts exact no-reference rendering", {
  store <- fake_store_with_sources(0)
  for (body in c("Durable body.", "Durable body.\n")) {
    report <- tempest:::tempest_report_md_render(
      "Title",
      body,
      store,
      citation_policy = "source_attributed"
    )
    expect_no_error(tempest:::tempest_final_report_validate(
      report,
      store,
      title = "Title",
      citation_policy = "source_attributed",
      on_unsupported_claim = "flag",
      min_support_score = 0.7
    ))
  }
})

test_that("final report validation preserves fenced reserved-heading literals", {
  store <- fake_store_with_sources(1)
  source_id <- store$list_retrieved_sources()[[1]]$id
  literal <- paste(
    "Example:",
    "",
    "```text",
    "## References",
    "",
    "literal content",
    "## Execution review",
    "literal review content",
    "```",
    paste0("A cited body [", source_id, "]."),
    sep = "\n"
  )
  report <- tempest:::tempest_report_md_render(
    "Title",
    literal,
    store,
    citation_policy = "source_attributed"
  )

  expect_no_error(tempest:::tempest_final_report_validate(
    report,
    store,
    title = "Title",
    citation_policy = "source_attributed",
    on_unsupported_claim = "flag",
    min_support_score = 0.7
  ))
  expect_match(report, "literal content", fixed = TRUE)
  expect_match(report, "literal review content", fixed = TRUE)
})

test_that("final report validation accepts canonical omitted definitions", {
  store <- fake_store_with_sources(1)
  source_id <- store$list_retrieved_sources()[[1]]$id
  report <- tempest:::tempest_report_md_render(
    "Title",
    paste0("A cited body [", source_id, "]."),
    store,
    citation_policy = "source_attributed",
    on_unsupported_claim = "flag",
    min_support_score = 0.7,
    include_references = FALSE
  )

  expect_match(report, paste0("[", source_id, "]"), fixed = TRUE)
  expect_no_match(report, "## References", fixed = TRUE)
  expect_no_error(tempest:::tempest_final_report_validate(
    report,
    store,
    title = "Title",
    citation_policy = "source_attributed",
    on_unsupported_claim = "flag",
    min_support_score = 0.7
  ))
})

test_that("verified no-reference reports reject non-supported citations", {
  statuses <- list(
    partially_supported = 0.6,
    unverifiable = NA_real_
  )
  for (policy in c("claim_verified", "strict")) {
    for (status in names(statuses)) {
      store <- fake_store_with_sources(1)
      source_id <- store$list_retrieved_sources()[[1]]$id
      claim <- tempest_claim(
        paste("Evidence status is", status),
        source_ids = source_id,
        verification_status = status,
        support_score = statuses[[status]]
      )
      store$add_proposed_claim(claim)
      claim <- fake_verify_claim_supports(store, list(claim))[[1]]

      expect_error(
        tempest:::tempest_report_md_render(
          "Title",
          paste0(claim@claim_text, " [", source_id, "]."),
          store,
          citation_policy = policy,
          on_unsupported_claim = "flag",
          min_support_score = 0.7,
          include_references = FALSE
        ),
        class = "tempest_product_report_error",
        info = paste(policy, status)
      )
    }
  }

  store <- fake_store_with_sources(2)
  source_ids <- vapply(
    store$list_retrieved_sources(),
    `[[`,
    character(1),
    "id"
  )
  claims <- list(
    tempest_claim(
      "Fully supported evidence",
      source_ids = source_ids[[1]],
      verification_status = "supported",
      support_score = 0.9
    ),
    tempest_claim(
      "Partial evidence",
      source_ids = source_ids[[2]],
      verification_status = "partially_supported",
      support_score = 0.6
    )
  )
  for (claim in claims) {
    store$add_proposed_claim(claim)
  }
  claims <- fake_verify_claim_supports(store, claims)
  mixed <- paste0(
    claims[[1]]@claim_text,
    " [",
    source_ids[[1]],
    "].\n\n",
    claims[[2]]@claim_text,
    " [",
    source_ids[[2]],
    "]."
  )

  expect_error(
    tempest:::tempest_report_md_render(
      "Title",
      mixed,
      store,
      citation_policy = "claim_verified",
      on_unsupported_claim = "flag",
      min_support_score = 0.7,
      include_references = FALSE
    ),
    class = "tempest_product_report_error"
  )
})

test_that("final report validation preserves canonical rendering before review", {
  store <- fake_store_with_sources(1)
  source_id <- store$list_retrieved_sources()[[1]]$id
  report <- tempest:::tempest_report_md_render(
    "Title",
    paste0("Durable cited body [", source_id, "]."),
    store,
    citation_policy = "source_attributed"
  )
  running <- tempest:::tempest_stage_record_start(
    "query_decomposition",
    paste0("sha256:", strrep("8", 64L)),
    attempt_id = "report-review-attempt"
  )
  fallback <- tempest:::tempest_stage_record_succeed(
    running,
    tempest:::tempest_stage_content_reference(list(queries = "Query")),
    support_status = "unknown",
    fallback_taken = TRUE,
    primary_error = simpleError("provider failed")
  )
  records <- list(fallback)
  review <- tempest:::tempest_stage_records_execution_review(records)
  reviewed <- tempest:::tempest_markdown_append_execution_review(report, review)

  expect_no_error(tempest:::tempest_final_report_validate(
    reviewed,
    store,
    title = "Title",
    citation_policy = "source_attributed",
    on_unsupported_claim = "flag",
    min_support_score = 0.7,
    stage_records = records
  ))
  expect_identical(
    substr(reviewed, 1L, nchar(report)),
    report
  )
})

test_that("default policy is unchanged source-attributed output", {
  store <- fake_store_with_sources(1)
  s1 <- store$list_retrieved_sources()[[1]]$id
  body <- paste0("Plain sentence [", s1, "].")
  md <- tempest:::tempest_report_md_render("Title", body, store)
  expect_match(md, "## References")
  expect_no_match(md, "✓")
})

test_that("session report Markdown comes from narrow product state", {
  skip_if_not_installed("ellmer")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session(
    "Canonical report",
    config = cfg,
    experts = list(test_expert(
      expert_id = "expert.canonical-report",
      name = "Dr. Canonical"
    ))
  )
  fixture <- test_persistence_add_costorm_evidence(
    session,
    key = "canonical-report"
  )
  report_md <- tempest:::tempest_report_md_render(
    title = tempest:::tempest_session_title(session),
    body = paste0(
      fixture$claim@claim_text,
      " [",
      fixture$span@source_id,
      "]."
    ),
    workspace = tempest:::tempest_session_workspace(session),
    citation_policy = cfg@citation_policy,
    on_unsupported_claim = cfg@on_unsupported_claim,
    min_support_score = cfg@min_support_score
  )
  report_md <- test_persistence_commit_costorm_report(session, report_md)

  expect_identical(
    tempest:::tempest_session_manifest(session)@status,
    "succeeded"
  )
  expect_identical(tempest_report(session), report_md)
})

test_that("session report Markdown requires a canonical report artifact", {
  skip_if_not_installed("ellmer")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session(
    "Missing report",
    config = cfg,
    experts = list(test_expert(
      expert_id = "expert.missing-report",
      name = "Dr. Missing"
    ))
  )

  expect_error(
    tempest_report(session),
    class = "tempest_product_report_error"
  )
})
