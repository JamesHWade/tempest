test_that("graft memory helpers manage streams, corrections, withdrawal, and reuse", {
  skip_if_not_installed("graft")
  skip_if_not_installed("ellmer")
  skip_if_not("graft_store" %in% getNamespaceExports("graft"))

  store_dir <- withr::local_tempdir()
  store_path <- file.path(store_dir, "memory")
  store <- tempest:::tempest_graft_store_open(store_path, create = TRUE)
  expect_s7_class(store, graft::LocalArtifactStore)
  expect_identical(
    tempest:::tempest_graft_store_path(store),
    normalizePath(store_path, winslash = "/", mustWork = TRUE)
  )
  expect_identical(
    tempest:::tempest_graft_stream_slug("Grid-Scale Battery Recycling?"),
    "grid-scale-battery-recycling"
  )

  fixture_a <- test_promotion_storm_fixture(
    run_id = "memory-run-1",
    evidence_text = "Initial pilot data showed 82% battery recovery."
  )
  fixture_b <- test_promotion_storm_fixture(
    run_id = "memory-run-2",
    evidence_text = "Corrected pilot data showed 74% battery recovery."
  )

  sel_a <- tempest_publish_artifact_research(fixture_a$research, store)
  sel_b <- tempest_publish_artifact_research(fixture_b$research, store)

  status_prov <- tempest:::tempest_graft_selection_status(store, sel_a)
  expect_identical(status_prov$status, "provisional")

  dec_1 <- tempest:::tempest_graft_accept_selection(
    store,
    selection_id = sel_a,
    stream = "battery-recycling",
    actor = "alice",
    reason = "Initial evidence checked",
    purpose = "research"
  )
  expect_s7_class(dec_1, graft::Decision)

  status_acc <- tempest:::tempest_graft_selection_status(store, sel_a)
  expect_identical(status_acc$status, "accepted")
  expect_identical(status_acc$stream, "battery-recycling")
  expect_identical(nrow(status_acc$history), 1L)

  knowledge <- tempest:::tempest_graft_reuse_stream(
    store,
    stream = "battery-recycling",
    purpose = "research"
  )
  expect_true(S7::S7_inherits(knowledge, TempestKnowledge))

  # Eligibility callback filters listed streams.
  denied_streams <- tempest:::tempest_graft_memory_streams(
    store,
    purpose = "research",
    eligible = function(stream) identical(stream, "other-stream")
  )
  expect_identical(nrow(denied_streams), 0L)

  # A correction advances the stream; earlier selection becomes superseded.
  dec_2 <- tempest:::tempest_graft_accept_selection(
    store,
    selection_id = sel_b,
    stream = "battery-recycling",
    actor = "bob",
    reason = "Updated evidence checked",
    purpose = "research"
  )
  expect_identical(dec_2@sequence, 2L)
  expect_identical(
    tempest:::tempest_graft_selection_status(store, sel_a)$status,
    "superseded"
  )
  expect_identical(
    tempest:::tempest_graft_selection_status(store, sel_b)$status,
    "accepted"
  )

  # Stale acceptance targeting a superseded stream head fails closed.
  expect_error(
    tempest:::tempest_graft_accept_selection(
      store,
      selection_id = sel_a,
      stream = "battery-recycling",
      actor = "alice",
      reason = "Stale re-acceptance",
      purpose = "research",
      expected = dec_1@id
    ),
    class = "tempest_graft_memory_error"
  )

  # Stale acceptance expecting an empty stream fails closed once a decision exists.
  expect_error(
    tempest:::tempest_graft_accept_selection(
      store,
      selection_id = sel_a,
      stream = "battery-recycling",
      actor = "alice",
      reason = "Stale initial acceptance",
      purpose = "research",
      expected = ""
    ),
    class = "tempest_graft_memory_error"
  )

  # Stale withdrawal targeting the superseded decision fails closed.
  expect_error(
    tempest:::tempest_graft_withdraw_stream(
      store,
      stream = "battery-recycling",
      actor = "alice",
      reason = "Stale withdrawal",
      expected = dec_1@id
    ),
    class = "tempest_graft_memory_error"
  )

  # Withdrawing the current reviewed decision removes active eligibility while
  # keeping history readable.
  dec_3 <- tempest:::tempest_graft_withdraw_stream(
    store,
    stream = "battery-recycling",
    actor = "bob",
    reason = "Needs reassessment",
    expected = dec_2@id
  )
  expect_identical(dec_3@action, "withdraw")
  status_withdrawn <- tempest:::tempest_graft_selection_status(store, sel_b)
  expect_identical(status_withdrawn$status, "withdrawn")
  expect_identical(nrow(status_withdrawn$history), 3L)

  expect_error(
    tempest:::tempest_graft_reuse_stream(
      store,
      stream = "battery-recycling",
      purpose = "research"
    ),
    class = "tempest_graft_memory_error"
  )

  # Direct stream lookup resolves status without requiring a full store scan,
  # and a failed store scan reports "unknown" rather than "provisional".
  local_mocked_bindings(
    tempest_graft_memory_streams = function(...) {
      stop("store exceeded stream listing bound")
    }
  )
  direct_status <- tempest:::tempest_graft_selection_status(
    store,
    selection_id = sel_b,
    purpose = "research",
    stream = "battery-recycling"
  )
  expect_identical(direct_status$status, "withdrawn")
  expect_identical(direct_status$stream, "battery-recycling")

  unknown_status <- tempest:::tempest_graft_selection_status(
    store,
    selection_id = sel_b,
    purpose = "research"
  )
  expect_identical(unknown_status$status, "unknown")
})

test_that("graft memory tools expose multi-stream recall and extract @extra$graft metadata", {
  skip_if_not_installed("graft")
  skip_if_not_installed("ellmer")
  skip_if_not("graft_tools" %in% getNamespaceExports("graft"))

  store <- graft::graft_store(
    file.path(withr::local_tempdir(), "memory"),
    create = TRUE
  )
  fixture <- test_promotion_fixture("storm")
  sel <- tempest_publish_artifact_research(fixture$research, store)
  tempest:::tempest_graft_accept_selection(
    store,
    selection_id = sel,
    stream = "grid-batteries",
    actor = "reviewer",
    reason = "Verified",
    purpose = "research"
  )

  tools <- tempest:::tempest_graft_memory_tools(store, purpose = "research")
  expect_length(tools, 2L)
  tool_names <- vapply(tools, \(t) t@name, character(1))
  expect_setequal(
    tool_names,
    c("list_project_memory", "recall_project_memory")
  )
  expect_identical(
    tempest:::tempest_researcher_checked_tool_names(tools),
    tool_names
  )

  recall_tool <- tools[[which(tool_names == "recall_project_memory")]]
  res <- recall_tool(stream = "grid-batteries")
  expect_s7_class(res, ellmer::ContentToolResult)

  consultations <- tempest:::tempest_graft_consultations(list(res))
  expect_identical(nrow(consultations), 1L)
  expect_identical(consultations$status, "accepted")
  expect_identical(consultations$stream, "grid-batteries")
  expect_identical(consultations$purpose, "research")
  expect_identical(consultations$selection_id, sel)

  # Single-stream fallback when graft_tools() is unavailable.
  local_mocked_bindings(tempest_graft_has_multi_tools = function() FALSE)
  fallback_tools <- tempest:::tempest_graft_memory_tools(
    store,
    purpose = "research"
  )
  expect_length(fallback_tools, 1L)
  expect_identical(fallback_tools[[1L]]@name, "recall_project_memory")
})
