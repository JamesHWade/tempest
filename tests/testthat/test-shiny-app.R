# Tests for the bundled Shiny app's modules. The app files live in
# inst/shiny/R and are sourced on demand.

source_shiny_modules <- function() {
  dir <- system.file("shiny", "R", package = "tempest")
  skip_if(identical(dir, ""), "Shiny app files not found")
  env <- new.env(parent = globalenv())
  for (f in sort(list.files(dir, pattern = "[.][Rr]$", full.names = TRUE))) {
    sys.source(f, envir = env)
  }
  env
}

test_that("every module UI builds a Shiny tag", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("shinychat")
  app <- source_shiny_modules()

  expect_s3_class(app$mod_config_ui("config"), "shiny.tag")
  expect_s3_class(
    app$mod_chat_ui("chat", app$mod_config_ui("config")),
    "shiny.tag"
  )
  expect_s3_class(app$mod_storm_ui("storm"), "shiny.tag")
  expect_s3_class(app$mod_mindmap_ui("mindmap"), "shiny.tag")
  expect_s3_class(app$mod_sources_ui("sources"), "shiny.tag")
  expect_s3_class(app$mod_facts_ui("facts"), "shiny.tag")
  expect_s3_class(app$mod_transcript_ui("transcript"), "shiny.tag")
  expect_s3_class(app$mod_report_ui("report"), "shiny.tag")
})

test_that("evidence tabs prioritize the graph and curated table shells", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  app <- source_shiny_modules()

  mindmap_html <- paste(
    as.character(app$mod_mindmap_ui("mindmap")),
    collapse = ""
  )
  sources_html <- paste(
    as.character(app$mod_sources_ui("sources")),
    collapse = ""
  )
  facts_html <- paste(as.character(app$mod_facts_ui("facts")), collapse = "")

  expect_match(mindmap_html, "tempest-mindmap-card", fixed = TRUE)
  expect_match(mindmap_html, "height:620px", fixed = TRUE)
  expect_match(mindmap_html, "<details", fixed = TRUE)
  expect_match(mindmap_html, "Accessible mind map outline", fixed = TRUE)
  expect_match(sources_html, "tempest-evidence-card", fixed = TRUE)
  expect_match(sources_html, "sources-source_count", fixed = TRUE)
  expect_match(facts_html, "tempest-evidence-card", fixed = TRUE)
  expect_match(facts_html, "facts-fact_count", fixed = TRUE)
})

test_that("module UIs namespace their input ids", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  app <- source_shiny_modules()
  html <- as.character(app$mod_storm_ui("storm"))
  expect_match(paste(html, collapse = ""), "storm-run")
  expect_match(paste(html, collapse = ""), "storm-topic")
  expect_match(paste(html, collapse = ""), "storm-cancel_control")
  chat_html <- paste(
    as.character(app$mod_chat_ui("chat", app$mod_config_ui("config"))),
    collapse = ""
  )
  expect_match(chat_html, "chat-setup_settings_toggle")
  expect_match(chat_html, "shiny-chat-footer")
  expect_match(chat_html, "chat-runtime_footer")
  expect_match(chat_html, "tempestCitationSanitizer")
})

test_that("STORM worker cancellation stops Mirai work and records progress", {
  skip_if_not_installed("mirai")
  local_mirai_coverage_dir()
  app <- source_shiny_modules()
  job <- mirai::mirai({
    Sys.sleep(5)
    "completed"
  })

  expect_equal(app$storm_cancel_worker(job), TRUE)
  deadline <- Sys.time() + 2
  while (mirai::unresolved(job) && Sys.time() < deadline) {
    Sys.sleep(0.01)
  }
  expect_equal(mirai::unresolved(job), FALSE)
  expect_equal(app$storm_cancel_worker(job), FALSE)

  event <- app$storm_cancelled_event("Topic", "run-1")
  expect_equal(event@event_type, "cancellation")
  expect_equal(event@status, "cancelled")
})

test_that("Co-STORM async queue serializes work and cancels stale commits", {
  skip_if_not_installed("promises")
  skip_if_not_installed("later")
  app <- source_shiny_modules()
  queue <- app$costorm_async_queue()
  resolve_first <- NULL
  order <- character()
  heartbeat <- FALSE

  first <- queue$enqueue(function(is_current) {
    order <<- c(order, "first-start")
    promises::promise(function(resolve, reject) {
      resolve_first <<- function() {
        if (is_current()) {
          order <<- c(order, "first-commit")
        }
        resolve(NULL)
      }
    })
  })
  second <- queue$enqueue(function(is_current) {
    order <<- c(order, "second-start")
    if (is_current()) {
      order <<- c(order, "second-commit")
    }
    NULL
  })
  later::later(function() heartbeat <<- TRUE, delay = 0)
  later::run_now(0.02)

  expect_equal(heartbeat, TRUE)
  expect_equal(order, "first-start")
  resolve_first()
  expect_null(await_tempest_promise(first)$error)
  expect_null(await_tempest_promise(second)$error)
  expect_equal(
    order,
    c("first-start", "first-commit", "second-start", "second-commit")
  )

  stale_commit <- FALSE
  resolve_stale <- NULL
  stale <- queue$enqueue(function(is_current) {
    promises::promise(function(resolve, reject) {
      resolve_stale <<- function() {
        stale_commit <<- is_current()
        resolve(NULL)
      }
    })
  })
  later::run_now(0.02)
  queue$cancel()
  resolve_stale()
  expect_null(await_tempest_promise(stale)$error)
  expect_equal(stale_commit, FALSE)
})

test_that("Co-STORM session failures expose only safe error metadata", {
  app <- source_shiny_modules()
  secret <- "Authorization: Bearer sk-live-secret"
  error <- structure(
    simpleError(secret),
    class = c(secret, "error", "condition")
  )

  event <- app$costorm_session_failed_event("session-safe", error)

  expect_identical(event@message, "Session setup failed.")
  expect_identical(event@payload$error_class, "tempest_operation_error")
  expect_identical(event@payload$error_message, "The operation failed.")
  expect_no_match(jsonlite::toJSON(event@payload, auto_unbox = TRUE), secret)
})

test_that("tempest_shiny_ui builds namespaced host panels", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("shinychat")
  html <- paste(
    as.character(tempest_shiny_ui(
      "host",
      panels = c("chat", "sources", "report"),
      show_config = TRUE
    )),
    collapse = ""
  )

  expect_match(html, "host-chat-topic")
  expect_match(html, "host-chat-save_session")
  expect_match(html, "host-sources-body")
  expect_match(html, "host-report-body")
  expect_match(html, "host-config-coordinator")
})

test_that("chat UI uses the Tempest assistant icon", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("shinychat")
  app <- source_shiny_modules()
  chat_html <- paste(
    as.character(app$mod_chat_ui("chat", app$mod_config_ui("config"))),
    collapse = ""
  )

  expect_match(chat_html, "icon-assistant")
  expect_match(chat_html, "tempest-logos")
  expect_match(chat_html, "tempest.svg", fixed = TRUE)
  expect_match(chat_html, "tempest-chat-icon")
  expect_no_match(chat_html, "robot")

  logo_resource <- app$tempest_logo_resource_path()
  logo_dir <- shiny::resourcePaths()[[logo_resource]]
  expect_equal(file.exists(file.path(logo_dir, "tempest.svg")), TRUE)
  expect_match(app$tempest_logo_src(), paste0("^", logo_resource, "/"))
})

test_that("config UI uses current OpenAI model defaults", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  app <- source_shiny_modules()
  html <- paste(as.character(app$mod_config_ui("config")), collapse = "")
  expect_match(html, 'value="openai/gpt-5.6-sol"', fixed = TRUE)
  expect_match(html, 'value="openai/gpt-5.6-luna"', fixed = TRUE)
})

test_that("config UI inherits tempest.chat until a model is edited", {
  withr::local_options(
    tempest.chat = "anthropic/claude-sonnet-4-20250514"
  )
  app <- source_shiny_modules()
  defaults <- app$shiny_default_models()

  expect_equal(
    unname(unlist(defaults)),
    rep("anthropic/claude-sonnet-4-20250514", 5L)
  )
  expect_null(app$shiny_config_models(defaults, defaults))

  edited <- defaults
  edited$expert <- "openai/gpt-5.6-luna"
  expect_equal(app$shiny_config_models(edited, defaults), edited)
})

test_that("the About popover links the papers and upstream repos", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  app <- source_shiny_modules()
  html <- paste(as.character(app$about_nav_item()), collapse = "")
  expect_match(html, "arxiv.org/abs/2402.14207")
  expect_match(html, "arxiv.org/abs/2408.15232")
  expect_match(html, "arxiv.org/abs/2310.03714")
  expect_match(html, "github.com/stanford-oval/storm")
  expect_match(html, "github.com/stanfordnlp/dspy")
})

test_that("the chat module provides an interactive landing greeting", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  app <- source_shiny_modules()
  html <- paste(
    as.character(app$chat_session_greeting_ui(shiny::NS("chat"))),
    collapse = ""
  )

  expect_match(html, "tempest-chat-welcome", fixed = TRUE)
  expect_match(html, "Welcome to tempest", fixed = TRUE)
  expect_match(html, "cited evidence", fixed = TRUE)
  expect_match(html, "one-shot report", fixed = TRUE)
  expect_match(html, "chat-topic", fixed = TRUE)
  expect_match(html, "chat-n_experts", fixed = TRUE)
  expect_match(html, "chat-start", fixed = TRUE)
  expect_match(html, "chat-research_options", fixed = TRUE)
  expect_match(html, "chat-setup_settings_toggle", fixed = TRUE)
  expect_match(html, "tempest-chat-start btn-sm", fixed = TRUE)
})

test_that("the bundled chat greeting offers an expert panel builder", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  app <- source_shiny_modules()
  ns <- shiny::NS("chat")
  html <- paste(
    as.character(app$chat_session_greeting_ui(
      ns,
      allow_user_experts = TRUE
    )),
    collapse = ""
  )
  modal <- paste(
    as.character(app$custom_expert_setup_modal(ns)),
    collapse = ""
  )

  expect_match(html, "chat-expert_setup", fixed = TRUE)
  expect_match(html, "3 generated", fixed = TRUE)
  expect_no_match(html, "chat-n_experts", fixed = TRUE)
  expect_match(modal, "Choose my own experts", fixed = TRUE)
  expect_match(modal, "chat-custom_expert_fields", fixed = TRUE)
  expect_match(modal, "chat-apply_expert_setup", fixed = TRUE)
  expect_match(
    modal,
    '<option value="3" selected>3 experts</option>',
    fixed = TRUE
  )
})

test_that("custom expert forms create validated user profiles", {
  app <- source_shiny_modules()
  profiles <- app$custom_expert_profiles(list(
    list(
      name = "Maya Chen",
      title = "Battery policy analyst",
      perspective = "Compare incentives, regulation, and adoption barriers."
    ),
    list(
      name = "Sam Okafor",
      title = "Grid engineer",
      perspective = "Test infrastructure assumptions against grid constraints."
    )
  ))

  expect_length(profiles, 2L)
  expect_s7_class(profiles[[1]], tempest:::TempestExpertProfile)
  expect_equal(profiles[[1]]@expert_id, "expert.user.01")
  expect_equal(profiles[[1]]@name, "Maya Chen")
  expect_equal(profiles[[1]]@title, "Battery policy analyst")
  expect_equal(
    profiles[[1]]@description,
    "Compare incentives, regulation, and adoption barriers."
  )
  expect_equal(profiles[[1]]@selection_metadata$source, "user")
  expect_match(profiles[[1]]@instructions, "cite relevant evidence")

  invalid <- tryCatch(
    app$custom_expert_profiles(list(list(
      name = "",
      title = "Analyst",
      perspective = "Inspect the evidence."
    ))),
    error = identity
  )
  expect_s3_class(invalid, "tempest_custom_expert_input_error")
  expect_match(conditionMessage(invalid), "Expert 1 needs a name")
})

test_that("user expert panels override host defaults only when enabled", {
  app <- source_shiny_modules()
  host <- list(test_expert(expert_id = "expert.host", name = "Host Expert"))
  user <- list(test_expert(expert_id = "expert.user", name = "User Expert"))

  expect_identical(
    app$costorm_session_experts(host, user, FALSE, "custom"),
    host
  )
  expect_identical(
    app$costorm_session_experts(host, user, TRUE, "generated"),
    host
  )
  expect_identical(
    app$costorm_session_experts(host, user, TRUE, "custom"),
    user
  )
})

test_that("chat footer reserves controls for active sessions", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  app <- source_shiny_modules()
  ns <- shiny::NS("chat")
  shell <- paste(
    as.character(app$chat_footer_ui(shiny::NS("chat"))),
    collapse = ""
  )
  expect_match(shell, "tempest-chat-footer", fixed = TRUE)
  expect_match(shell, "chat-runtime_footer", fixed = TRUE)
  expect_no_match(shell, "chat-footer_new", fixed = TRUE)
  idle <- paste(
    as.character(app$chat_runtime_footer_ui(NULL, ns = ns)),
    collapse = ""
  )
  expect_match(idle, "tempest-chat-footer-idle", fixed = TRUE)
  expect_no_match(idle, "No session", fixed = TRUE)

  ses <- list2env(
    list(experts = list(list(name = "Dr. Footer"))),
    parent = emptyenv()
  )
  ses$workspace <- new.env(parent = emptyenv())
  ses$workspace$list_retrieved_sources <- function() list()
  ses$workspace$list_proposed_claims <- function() list()
  html <- paste(
    as.character(app$chat_runtime_footer_ui(ses, ns = ns)),
    collapse = ""
  )

  expect_match(html, "justify-content-between")
  expect_match(html, "tempest-chat-footer-active")
  expect_match(html, "chat-footer_settings_toggle")
  expect_match(html, "chat-footer_new")
  expect_match(html, "chat-generate_report")
  expect_match(html, "chat-report_options")
  expect_match(html, "chat-report_style")
  expect_match(html, ">New session</span>", fixed = TRUE)
  expect_match(html, "bslib-toolbar-label", fixed = TRUE)
  expect_match(html, ">Report options</span>", fixed = TRUE)
  expect_match(
    html,
    '<div style="display:contents;">Report options</div>',
    fixed = TRUE
  )
  expect_match(html, "Generate report", fixed = TRUE)
  expect_no_match(html, "No report", fixed = TRUE)
  expect_no_match(html, "footer_sources", fixed = TRUE)
  expect_no_match(html, "footer_system", fixed = TRUE)
  expect_no_match(html, "footer_tools", fixed = TRUE)
})

test_that("chat UI delegates presentation features to shinychat", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("shinychat")
  app <- source_shiny_modules()
  html <- paste(
    as.character(app$mod_chat_ui("chat", shiny::div())),
    collapse = ""
  )
  css <- paste(as.character(app$tempest_app_styles()), collapse = "")

  expect_match(html, "<shiny-chat-container", fixed = TRUE)
  expect_match(html, "<shiny-chat-footer", fixed = TRUE)
  expect_match(html, "Welcome to tempest", fixed = TRUE)
  expect_no_match(html, "submit-key", fixed = TRUE)
  expect_match(html, "application/pdf", fixed = TRUE)
  expect_match(html, "image/png", fixed = TRUE)
  expect_no_match(html, "enable-cancel", fixed = TRUE)
  expect_no_match(css, ".suggestion", fixed = TRUE)
  expect_no_match(css, "shiny-chat-suggestion", fixed = TRUE)
  expect_no_match(css, ".shiny-chat-footer", fixed = TRUE)
  expect_match(css, "@media (max-width: 575.98px)", fixed = TRUE)
  expect_match(css, "--shiny-chat-greeting-max-width: 680px", fixed = TRUE)
  expect_match(css, "tempest-chat-welcome-actions", fixed = TRUE)
  expect_match(css, "--tempest-chat-welcome-control-height", fixed = TRUE)
  expect_match(css, "bslib-toolbar-input-button[data-type=", fixed = TRUE)
  expect_match(css, "'experts'", fixed = TRUE)
  expect_match(css, "'tools'", fixed = TRUE)
  expect_match(css, "'start'", fixed = TRUE)
  expect_match(css, "padding-right: 2.25rem", fixed = TRUE)
})

test_that("chat module delegates native chat and session lifecycle work", {
  app <- source_shiny_modules()
  server_code <- paste(deparse(body(app$mod_chat_server)), collapse = "\n")

  expect_named(
    formals(tempest_shiny_server),
    c(
      "id",
      "config",
      "store",
      "panels",
      "experts",
      "session_id",
      "program_set",
      "knowledge_view"
    )
  )
  expect_named(
    formals(app$mod_chat_server),
    c(
      "id",
      "config",
      "store",
      "experts",
      "session_id",
      "program_set",
      "knowledge_view",
      "allow_user_experts"
    )
  )

  expect_match(server_code, "tempest_shinychat_adapter", fixed = TRUE)
  expect_match(server_code, "tempest_session_process_turn_async", fixed = TRUE)
  expect_no_match(
    server_code,
    "tempest_costorm_last_deputy_execution",
    fixed = TRUE
  )
  expect_no_match(server_code, "rlang::duplicate", fixed = TRUE)
  expect_match(
    server_code,
    "completion_id = completion_id",
    fixed = TRUE
  )
  expect_no_match(server_code, "deputy_execution =", fixed = TRUE)
  expect_no_match(server_code, "provider_turn =", fixed = TRUE)
  expect_no_match(server_code, "turn_id =", fixed = TRUE)
  expect_no_match(server_code, "tempest_uuid(\"chat-turn\")", fixed = TRUE)
  expect_match(server_code, "tempest_session_warmup_async", fixed = TRUE)
  expect_match(server_code, "tempest_session_new", fixed = TRUE)
  expect_match(
    server_code,
    "tempest_program_set_manifest_programs",
    fixed = TRUE
  )
  expect_match(server_code, "tempest_product_knowledge_view", fixed = TRUE)
  expect_match(
    server_code,
    "knowledge_view = knowledge_view_value",
    fixed = TRUE
  )
  expect_match(server_code, "tempest_generate_experts_async", fixed = TRUE)
  expect_no_match(server_code, "tempest::tempest_session(", fixed = TRUE)
  expect_no_match(server_code, "shinychat::", fixed = TRUE)
  expect_no_match(server_code, "$last_turn", fixed = TRUE)
  expect_no_match(server_code, "$last_input", fixed = TRUE)
})

test_that("expert cards render deterministic expert icons", {
  skip_if_not_installed("shiny")
  app <- source_shiny_modules()
  html <- paste(
    as.character(app$expert_card(test_expert(
      expert_id = "expert.history",
      name = "Dr. Ada Flow",
      title = "Historian",
      description = "Archives and provenance."
    ))),
    collapse = ""
  )

  expect_match(html, "tempest-persona-icon")
  expect_match(html, "aria-label=\"Expert Dr. Ada Flow\"", fixed = TRUE)
  expect_match(html, ">DA<", fixed = TRUE)
  expect_match(html, "Historian")
})

test_that("nav panels carry explicit string values for nav_select()", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  app <- source_shiny_modules()
  report_html <- paste(as.character(app$mod_report_ui("report")), collapse = "")
  expect_match(report_html, 'data-value="Report"')
})

test_that("the report module starts report-free", {
  skip_if_not_installed("shiny")
  app <- source_shiny_modules()
  store <- app$new_session_store()

  shiny::testServer(app$mod_report_server, args = list(store = store), {
    expect_match(as.character(output$body$html), "Generate a report")
    expect_identical("set_report" %in% names(store), FALSE)
  })
})

test_that("citation_markdown renders numbered references from cited sources", {
  skip_if_not_installed("shiny")
  app <- source_shiny_modules()
  source_store <- fake_store_with_sources(2)
  sources <- source_store$list_retrieved_sources()
  first_id <- sources[[1]]$id
  second_id <- sources[[2]]$id
  missing_id <- "S000000000000"
  md <- paste0(
    "First claim [",
    first_id,
    "] and again [",
    first_id,
    "]. Second claim [",
    second_id,
    "]. Missing claim [",
    missing_id,
    "]."
  )

  rendered <- app$citation_markdown(
    md,
    store = source_store,
    include_references = TRUE
  )

  expect_match(rendered, "tempest-citation")
  expect_match(rendered, "tempest-reference-panel")
  expect_match(rendered, paste0("href=\"#tempest-ref-", first_id, "\""))
  expect_match(rendered, paste0("id=\"tempest-ref-", first_id, "\""))
  expect_match(rendered, "[1]", fixed = TRUE)
  expect_match(rendered, "[2]", fixed = TRUE)
  expect_match(rendered, "[3?]", fixed = TRUE)
  expect_match(rendered, "Missing source metadata")
  expect_no_match(rendered, paste0("[", first_id, "]"), fixed = TRUE)
})

test_that("citation rendering uses the retriever's T1 workspace", {
  skip_if_not_installed("shiny")
  app <- source_shiny_modules()
  workspace <- tempest_research_workspace()
  workspace$upsert_retrieved_resource(fake_source(
    url = "https://example.org/t1-workspace",
    title = "T1 Workspace Source"
  ))
  source_id <- workspace$list_retrieved_sources()[[1]]$id
  retriever <- tempest_retriever(
    config = tempest_config(),
    workspace = workspace
  )

  rendered <- app$citation_markdown(
    paste0("Workspace-backed claim [", source_id, "]."),
    store = retriever,
    include_references = TRUE
  )

  expect_identical(app$citation_source_store(retriever), workspace)
  expect_match(rendered, "T1 Workspace Source", fixed = TRUE)
  expect_match(rendered, "https://example.org/t1-workspace", fixed = TRUE)
  expect_no_match(rendered, "Missing source metadata", fixed = TRUE)

  removed_alias <- structure(
    list(workspace = NULL, store = workspace),
    class = "TempestRetriever"
  )
  canonical <- structure(
    list(workspace = workspace, store = tempest_research_workspace()),
    class = "TempestRetriever"
  )
  expect_null(app$citation_source_store(removed_alias))
  expect_identical(app$citation_source_store(canonical), workspace)
})

test_that("citation_markdown strips private external citation markers", {
  app <- source_shiny_modules()
  marker <- paste0(
    intToUtf8(0xE200),
    "cite",
    intToUtf8(0xE202),
    "turn0search0",
    intToUtf8(0xE201)
  )
  compound_marker <- paste0(
    intToUtf8(0xE200),
    "cite",
    intToUtf8(0xE202),
    "turn0search1",
    intToUtf8(0xE202),
    "turn0search0",
    intToUtf8(0xE201)
  )
  md <- paste0(
    "Visible claim ",
    marker,
    " still reads well. Another ",
    compound_marker,
    " sentence cites [S123456789abc]."
  )

  expect_equal(
    app$sanitize_external_citation_markers(md),
    "Visible claim still reads well. Another sentence cites [S123456789abc]."
  )
  rendered <- app$citation_markdown(md)
  expect_no_match(rendered, "turn0search")
  expect_match(rendered, "tempest-citation")
  expect_match(rendered, "[1?]", fixed = TRUE)
})

test_that("citation_markdown replaces Tempest footnotes with a reference panel", {
  skip_if_not_installed("shiny")
  app <- source_shiny_modules()
  source_store <- fake_store_with_sources(2)
  sources <- source_store$list_retrieved_sources()
  cited_id <- sources[[1]]$id
  unused_title <- sources[[2]]$title
  md <- paste0(
    "# Report\n\nFinding [^",
    cited_id,
    "].\n\n## References\n\n[^",
    cited_id,
    "]: Old footnote.\n"
  )

  rendered <- app$citation_markdown(
    md,
    store = source_store,
    include_references = TRUE
  )

  expect_match(rendered, paste0("href=\"#tempest-ref-", cited_id, "\""))
  expect_match(rendered, "tempest-reference-panel")
  expect_no_match(rendered, "Old footnote")
  expect_no_match(rendered, unused_title, fixed = TRUE)
})


test_that("Shiny rendering escapes untrusted HTML and unsafe links", {
  skip_if_not_installed("commonmark")
  skip_if_not_installed("DT")
  app <- source_shiny_modules()
  source_store <- fake_store_with_sources(1)
  source_id <- source_store$list_retrieved_sources()[[1]]$id
  malicious <- paste0(
    "<script>globalThis.tempestXss = true</script>",
    "<img src=x onerror=alert(1)>",
    " Supported claim [",
    source_id,
    "]."
  )

  rendered <- as.character(app$markdown_ui(malicious, store = source_store))
  expect_no_match(rendered, "<script", fixed = TRUE)
  expect_no_match(rendered, "<img", fixed = TRUE)
  expect_match(rendered, "&lt;script&gt;", fixed = TRUE)
  expect_match(rendered, "&lt;img src=x onerror=alert(1)&gt;", fixed = TRUE)
  expect_match(rendered, "tempest-citation", fixed = TRUE)
  expect_equal(app$citation_safe_url("javascript:alert(1)"), "")
  expect_equal(
    app$citation_safe_url("https://example.org/source"),
    "https://example.org/source"
  )

  widget <- app$styled_datatable(
    data.frame(title = "<img onerror=alert(1)>", url = "<a>safe</a>"),
    html_columns = "url"
  )
  expect_equal(attr(widget$x$options, "escapeIdx"), '"1"')
  expect_equal(widget$x$options$responsive, TRUE)
  expect_match(widget$x$options$dom, "tempest-table-toolbar", fixed = TRUE)
  expect_null(widget$x$options$scrollX)

  source_link <- app$source_table_links(
    '<img src=x onerror="alert(1)">',
    "javascript:alert(1)",
    "Sunsafe"
  )
  expect_no_match(source_link, "<img", fixed = TRUE)
  expect_no_match(source_link, "href=", fixed = TRUE)
  expect_match(source_link, "&lt;img", fixed = TRUE)

  document <- app$report_html_document("<p>Safe body</p>")
  expect_match(document, "Content-Security-Policy", fixed = TRUE)
  expect_match(document, "default-src 'none'", fixed = TRUE)
})

test_that("session store mutations work outside reactive consumers", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ellmer")
  app <- source_shiny_modules()
  store <- app$new_session_store()
  config <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  ses <- tempest_session(
    "Test topic",
    config = config,
    experts = list(test_expert(expert_id = "expert.store"))
  )

  expect_silent(store$touch())
  expect_silent(store$set(ses))
  expect_identical(shiny::isolate(store$get()), ses)
})

test_that("the transcript module shows recent turns from the store", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ellmer")
  app <- source_shiny_modules()
  store <- app$new_session_store()
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  ses <- tempest_session(
    "Test topic",
    config = cfg,
    experts = list(test_expert(
      expert_id = "expert.a",
      name = "Dr. A",
      title = "Sci",
      description = "X"
    ))
  )

  shiny::testServer(app$mod_transcript_server, args = list(store = store), {
    expect_match(as.character(output$body$html), "Start a conversation")
    ses$add_turn("user", "user", "hello world")
    tempest:::tempest_session_append_transcript(
      ses,
      "Moderator",
      "assistant",
      "answer text"
    )
    store$set(ses)
    session$flushReact()
    body <- as.character(output$body$html)
    expect_match(body, "hello world")
    expect_match(body, "answer text")
    expect_match(body, "tempest-inline-icon")
    expect_no_match(body, "fa-robot")
  })
})

test_that("the transcript module renders citations in completed answers", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ellmer")
  app <- source_shiny_modules()
  store <- app$new_session_store()
  source_store <- fake_store_with_sources(1)
  source_id <- source_store$list_retrieved_sources()[[1]]$id
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  ses <- tempest_session(
    "Citation topic",
    config = cfg,
    experts = list(test_expert(
      expert_id = "expert.citations",
      name = "Dr. Citations"
    )),
    retriever = tempest_retriever(config = cfg, workspace = source_store)
  )

  shiny::testServer(app$mod_transcript_server, args = list(store = store), {
    tempest:::tempest_session_append_transcript(
      ses,
      "Moderator",
      "assistant",
      paste0("Cited answer [", source_id, "].")
    )
    store$set(ses)
    session$flushReact()
    body <- as.character(output$body$html)
    expect_match(body, "tempest-citation")
    expect_match(body, paste0("href=\"#tempest-ref-", source_id, "\""))
    expect_no_match(body, paste0("[", source_id, "]"), fixed = TRUE)
  })
})

test_that("the mind map module counts nodes, sources, facts, and turns", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ellmer")
  app <- source_shiny_modules()
  store <- app$new_session_store()
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  ses <- tempest_session(
    "Test topic",
    config = cfg,
    experts = list(test_expert(
      expert_id = "expert.a",
      name = "Dr. A",
      title = "Sci",
      description = "X"
    ))
  )
  ses$add_turn("user", "user", "q1")

  shiny::testServer(app$mod_mindmap_server, args = list(store = store), {
    store$set(ses)
    session$flushReact()
    expect_equal(output$n_turns, "1")
    expect_match(output$n_sources, "^[0-9]+$")
    expect_match(output$n_facts, "^[0-9]+$")
    accessible <- as.character(output$graph_accessible)[[1]]
    expect_match(accessible, "Test topic", fixed = TRUE)
    expect_match(accessible, "<details>", fixed = TRUE)
    expect_match(as.character(output$graph_status)[[1]], "role=\"status\"")
  })
})

test_that("mind map accessible view exposes relationships, notes, and sources", {
  skip_if_not_installed("shiny")
  app <- source_shiny_modules()
  source_store <- fake_store_with_sources(1)
  source <- source_store$list_retrieved_sources()[[1]]
  mindmap <- list(
    nodes = list(
      list(
        id = "root",
        label = "Root topic",
        parent = NULL,
        notes = "Root notes",
        source_ids = character()
      ),
      list(
        id = "child",
        label = "Child finding",
        parent = "root",
        notes = "Finding notes",
        source_ids = source$id
      )
    ),
    edges = list(list(from = "root", to = "child", relation = "supports"))
  )

  html <- as.character(app$mindmap_accessible_tree(mindmap, source_store))

  expect_match(html, "Root topic", fixed = TRUE)
  expect_match(html, "Child finding", fixed = TRUE)
  expect_match(html, "Finding notes", fixed = TRUE)
  expect_match(html, "supports to Child finding", fixed = TRUE)
  expect_match(html, source$url, fixed = TRUE)
  expect_match(html, "<summary>", fixed = TRUE)

  network <- app$mindmap_to_visnetwork(mindmap)
  expect_equal(network$edges$label, "")
  expect_equal(network$edges$title, "supports")

  mindmap$nodes[[3]] <- list(
    id = "leaf",
    label = "Leaf",
    parent = "child",
    notes = "",
    source_ids = character()
  )
  structural_network <- app$mindmap_to_visnetwork(mindmap)
  leaf_edge <- structural_network$edges[
    structural_network$edges$to == "leaf",
    ,
    drop = FALSE
  ]
  expect_equal(leaf_edge$from, "child")
  expect_equal(leaf_edge$title, "Contains")
  expect_equal(leaf_edge$dashes, TRUE)
})

test_that("shared fake Co-STORM session populates evidence tabs", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ellmer")
  app <- source_shiny_modules()
  shared <- app$new_session_store()
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  source_store <- fake_store_with_sources(1)
  source_id <- source_store$list_retrieved_sources()[[1]]$id
  source_store$add_proposed_claim(tempest_claim(
    claim_text = "Integrated evidence is available.",
    source_ids = source_id,
    confidence = "high"
  ))
  retriever <- tempest_retriever(config = cfg, workspace = source_store)
  ses <- tempest_session(
    "Integration topic",
    config = cfg,
    experts = list(tempest_expert(
      expert_id = "expert.integration",
      name = "Integration Expert",
      title = "Researcher",
      description = "End-to-end app behavior",
      instructions = "Inspect the integrated application behavior."
    )),
    retriever = retriever
  )
  tempest:::tempest_session_commit_mindmap(
    ses,
    list(
      nodes = list(
        list(
          id = "root",
          label = "Integration topic",
          parent = NULL,
          notes = "",
          source_ids = character()
        ),
        list(
          id = "evidence",
          label = "Evidence",
          parent = "root",
          notes = "Integrated evidence node.",
          source_ids = source_id
        )
      ),
      edges = list(list(from = "root", to = "evidence", relation = "supports"))
    )
  )
  ses$add_turn("user", "user", "What evidence exists?")
  tempest:::tempest_session_append_transcript(
    ses,
    "Moderator",
    "assistant",
    paste0("Integrated evidence is available [", source_id, "].")
  )
  shared$set(ses)

  shiny::testServer(app$mod_sources_server, args = list(store = shared), {
    session$flushReact()
    expect_no_match(as.character(output$body$html), "No sources collected yet")
  })
  shiny::testServer(app$mod_facts_server, args = list(store = shared), {
    session$flushReact()
    expect_no_match(as.character(output$body$html), "No facts extracted yet")
  })
  shiny::testServer(app$mod_mindmap_server, args = list(store = shared), {
    session$flushReact()
    expect_equal(output$n_nodes, "2")
    expect_equal(output$n_sources, "1")
    expect_equal(output$n_facts, "1")
    expect_equal(output$n_turns, "2")
  })
  shiny::testServer(app$mod_transcript_server, args = list(store = shared), {
    session$flushReact()
    expect_match(as.character(output$body$html), "Integrated evidence")
  })
  shiny::testServer(app$mod_report_server, args = list(store = shared), {
    session$flushReact()
    expect_match(as.character(output$body$html), "Generate a report")
  })
})

test_that("chat command messages summarize active session state", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  app <- source_shiny_modules()
  source_store <- fake_store_with_sources(1)
  source_id <- source_store$list_retrieved_sources()[[1]]$id
  claim <- tempest_claim(
    claim_text = "Command summaries include facts.",
    source_ids = source_id,
    verification_status = "supported",
    support_score = 0.82
  )
  source_store$add_proposed_claim(claim)
  fake_verify_claim_supports(source_store, list(claim))
  ses <- list(
    topic = "Command topic",
    report_md = "# Command report",
    experts = list(test_expert(
      expert_id = "expert.command",
      name = "Dr. Command",
      title = "Interaction specialist",
      description = "Runtime controls"
    )),
    workspace = source_store,
    config = tempest_config(search_provider = "wikipedia")
  )

  expect_match(app$chat_command_message("experts", ses), "Dr. Command")
  expect_match(app$chat_command_message("sources", ses), "Example 1")
  expect_match(app$chat_command_message("facts", ses), "support 0.82")
  expect_match(app$chat_command_message("facts", ses), "Evidence review")
  expect_match(
    app$chat_command_message("facts", ses),
    "Verified evidence: 1 of 1 claims.",
    fixed = TRUE
  )
  expect_match(app$chat_command_message("tools", ses), "wikipedia")
  expect_match(
    app$chat_command_message("tools", ses),
    "Deputy-owned session adapters",
    fixed = TRUE
  )

  footer <- paste(
    as.character(app$chat_runtime_footer_ui(ses, chat_status = "streaming")),
    collapse = ""
  )
  expect_match(footer, "Answering")
  expect_match(footer, "Report ready")
  expect_match(
    footer,
    "<template>Session status: Answering</template>",
    fixed = TRUE
  )
  expect_match(footer, "<template>Experts: 1</template>", fixed = TRUE)
  expect_match(footer, "<template>Sources: 1</template>", fixed = TRUE)
  expect_match(footer, "<template>Facts: 1</template>", fixed = TRUE)
  expect_match(
    footer,
    "<template>Report status: Report ready</template>",
    fixed = TRUE
  )
})

test_that("facts review exposes safe durable execution downgrades", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ellmer")
  app <- source_shiny_modules()
  config <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  session <- tempest_session(
    "Review downgrade",
    config = config,
    experts = list(test_expert(
      expert_id = "expert.review-downgrade",
      name = "Review Downgrade Expert"
    )),
    session_id = "review-downgrade"
  )
  source <- fake_source("https://example.org/review-downgrade")
  session$workspace$upsert_retrieved_resource(source)
  program <- tempest:::tempest_session_programs(session)$personas
  running <- tempest:::tempest_stage_record_start(
    "personas",
    program$program_artifact_id,
    trace_references = list(research_run_id = session$session_id),
    attempt_id = "stage-attempt-review-fallback"
  )
  fallback <- tempest:::tempest_stage_record_succeed(
    running,
    tempest:::tempest_stage_output_reference(
      "state_field",
      "experts",
      content_digest = tempest:::tempest_stage_state_output_digest(
        "personas",
        session$experts
      )
    ),
    support_status = "unknown",
    fallback_taken = TRUE,
    primary_error = simpleError("raw provider secret")
  )
  tempest:::tempest_session_set_stage_records(session, list(fallback))

  review <- app$chat_command_message("facts", session)

  expect_match(review, "Verified evidence: 0 of 0 claims.", fixed = TRUE)
  expect_match(review, "Execution review", fixed = TRUE)
  expect_match(review, "stage-attempt-review-fallback", fixed = TRUE)
  expect_match(
    review,
    "tempest::fallback/personas/ellmer-structured@1",
    fixed = TRUE
  )
  expect_no_match(review, "raw provider secret", fixed = TRUE)
})

test_that("start suggestions wait for warmup when experts are available", {
  app <- source_shiny_modules()

  expect_equal(
    app$should_delay_start_suggestions(TRUE, list(list(name = "Dr. A"))),
    TRUE
  )
  expect_equal(
    app$should_delay_start_suggestions(FALSE, list(list(name = "Dr. A"))),
    FALSE
  )
  expect_equal(app$should_delay_start_suggestions(TRUE, list()), FALSE)
})

test_that("the chat greeting prioritizes one-time session controls", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("shinychat")
  app <- source_shiny_modules()
  html <- paste(
    as.character(app$mod_chat_ui("chat", app$mod_config_ui("config"))),
    collapse = ""
  )
  expect_match(html, "tempest-chat-welcome", fixed = TRUE)
  expect_match(html, "chat-topic", fixed = TRUE)
  expect_match(html, "chat-n_experts", fixed = TRUE)
  expect_match(html, "chat-start", fixed = TRUE)
  expect_match(html, "chat-research_options", fixed = TRUE)
  expect_match(html, "chat-setup_settings_toggle", fixed = TRUE)
  expect_match(html, "Welcome to tempest", fixed = TRUE)
  expect_no_match(html, "tempest-chat-card-header", fixed = TRUE)
  expect_no_match(html, "chat-progress", fixed = TRUE)
  expect_no_match(html, "detailed workspace settings", fixed = TRUE)
  expect_match(html, 'role="switch"', fixed = TRUE)
  expect_match(html, "chat-suggest")
  expect_match(html, "Suggest follow-up questions")
})

test_that("the bundled app enables user-configured expert panels", {
  app_file <- system.file("shiny", "app.R", package = "tempest")
  app_code <- paste(readLines(app_file, warn = FALSE), collapse = "\n")

  expect_equal(
    lengths(regmatches(
      app_code,
      gregexpr("allow_user_experts = TRUE", app_code, fixed = TRUE)
    )),
    2L
  )
})

test_that("the chat settings drawer holds secondary workspace controls", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("shinychat")
  app <- source_shiny_modules()
  html <- paste(
    as.character(app$mod_chat_ui("chat", app$mod_config_ui("config"))),
    collapse = ""
  )

  expect_match(html, "sidebar-right", fixed = TRUE)
  expect_match(html, 'data-open-desktop="closed"', fixed = TRUE)
  expect_match(html, "chat-session_settings", fixed = TRUE)
  expect_match(html, "Expert panel", fixed = TRUE)
  expect_match(html, "Session storage", fixed = TRUE)
  expect_match(html, "chat-save_session")
  expect_match(html, "chat-load_session")
  expect_match(html, "chat-autosave_session")
  expect_no_match(html, "Research configuration", fixed = TRUE)
  expect_no_match(html, "Bundle directory", fixed = TRUE)
})

test_that("the chat server toggles settings from greeting and footer", {
  app <- source_shiny_modules()
  server_code <- paste(deparse(body(app$mod_chat_server)), collapse = "\n")

  expect_match(server_code, "input$setup_settings_toggle", fixed = TRUE)
  expect_match(server_code, "input$footer_settings_toggle", fixed = TRUE)
  expect_match(server_code, 'toggle_sidebar("settings"', fixed = TRUE)
  expect_match(server_code, "suggestions_enabled", fixed = TRUE)
  expect_match(server_code, "update_expert_setup_button", fixed = TRUE)
  expect_match(
    server_code,
    "shiny::isolate(expert_setup_mode())",
    fixed = TRUE
  )
  expect_match(server_code, "ignoreNULL = TRUE", fixed = TRUE)
  expect_no_match(server_code, "input$footer_report", fixed = TRUE)
})

test_that("session archives round-trip product state through upload", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ellmer")
  skip_if_not_installed("zip")
  app <- source_shiny_modules()
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  ses <- tempest_session(
    "Downloadable session",
    config = cfg,
    experts = list(test_expert(
      expert_id = "expert.archive",
      name = "Dr. Archive"
    ))
  )
  store <- app$new_session_store()
  store$set(ses)
  archive <- tempfile(fileext = ".zip")
  extract_root <- file.path(withr::local_tempdir(), "archive")

  expect_no_error(app$session_archive_write(store, archive))
  bundle <- app$session_archive_extract(archive, extract_root)
  restored <- tempest_session_resume(bundle, config = cfg)

  expect_r6_class(restored, "TempestSession")
  expect_equal(restored$topic, "Downloadable session")
  expect_null(tempest:::tempest_session_report_value(restored))
  archive_files <- utils::unzip(archive, list = TRUE)$Name
  manifest <- tempest:::tempest_product_read_json(
    file.path(bundle, "session.json")
  )
  expect_setequal(
    archive_files,
    c("session.json", as.character(unlist(manifest$files)))
  )
  expect_contains(archive_files, "session.json")
  expect_disjoint(archive_files, "report.md")
})

test_that("session archive validation rejects unsafe entries and large files", {
  skip_if_not_installed("shiny")
  app <- source_shiny_modules()

  expect_equal(
    app$session_archive_listing_is_safe("session.json", 100),
    TRUE
  )
  expect_equal(
    app$session_archive_listing_is_safe("../session.json", 100),
    FALSE
  )
  expect_equal(
    app$session_archive_listing_is_safe(
      c("session.json", "session.json"),
      c(100, 100)
    ),
    FALSE
  )
  expect_equal(
    app$session_archive_listing_is_safe("session.json", 51 * 1024^2),
    FALSE
  )
  expect_equal(
    app$session_archive_listing_is_safe("unexpected.txt", 100),
    TRUE
  )
  expect_equal(
    app$session_archive_listing_is_safe(
      c("session.json", "unexpected.txt"),
      c(100, 100),
      declared_files = "experts.json"
    ),
    FALSE
  )
  expect_equal(
    app$session_archive_listing_is_safe("/session.json", 100),
    FALSE
  )
  expect_equal(
    app$session_archive_listing_is_safe("a/./session.json", 100),
    FALSE
  )
})

test_that("session archive manifests accept only the current envelope", {
  skip_if_not_installed("shiny")
  app <- source_shiny_modules()
  files <- c(
    "experts.json",
    "report.md"
  )
  manifest_fields <- list(
    package_version = "0.2.0.9000",
    session_id = "session-1",
    topic = "Archive topic",
    title = "Archive topic",
    research_manifest = list(),
    report_reference = NULL,
    workspace = list(),
    saved_at = "2026-08-15T00:00:00.000000Z",
    files = files,
    checksums = stats::setNames(as.list(rep("checksum", length(files))), files)
  )
  current <- c(
    list(
      schema_version = 9L,
      bundle_type = "costorm",
      bundle_status = "complete"
    ),
    manifest_fields
  )
  expect_identical(app$session_archive_manifest_files(current), files)

  expect_error(
    app$session_archive_manifest_files(c(
      list(schema_version = 7L, status = "complete"),
      manifest_fields
    )),
    "unsupported schema or status",
    fixed = TRUE
  )

  expect_error(
    app$session_archive_manifest_files(
      utils::modifyList(current, list(bundle_type = "storm"))
    ),
    "unsupported schema or status",
    fixed = TRUE
  )
  expect_error(
    app$session_archive_manifest_files(
      utils::modifyList(current, list(bundle_status = "running"))
    ),
    "unsupported schema or status",
    fixed = TRUE
  )
  expect_error(
    app$session_archive_manifest_files(
      utils::modifyList(current, list(schema_version = 9.9))
    ),
    "unsupported schema or status",
    fixed = TRUE
  )
  expect_error(
    app$session_archive_manifest_files(c(
      list(schema_version = 9L, status = "complete"),
      manifest_fields
    )),
    "unsupported schema or status",
    fixed = TRUE
  )
  expect_error(
    app$session_archive_manifest_files(c(
      list(
        schema_version = 10L,
        bundle_type = "costorm",
        bundle_status = "complete"
      ),
      manifest_fields
    )),
    "unsupported schema or status",
    fixed = TRUE
  )
})

test_that("session archive extraction rejects undeclared and tampered files", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ellmer")
  skip_if_not_installed("zip")
  app <- source_shiny_modules()
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  ses <- tempest_session(
    "Archive integrity",
    config = cfg,
    experts = list(test_expert(
      expert_id = "expert.integrity",
      name = "Dr. Integrity"
    ))
  )
  root <- withr::local_tempdir()
  bundle <- file.path(root, "bundle")
  tempest_session_save(ses, bundle)
  manifest <- tempest:::tempest_product_read_json(
    file.path(bundle, "session.json")
  )
  content_files <- as.character(unlist(manifest$files))
  expect_gt(length(content_files), 0L)
  content_file <- content_files[[1L]]
  writeLines("tampered", file.path(bundle, content_file))
  tampered_archive <- file.path(root, "tampered.zip")
  zip::zip(
    tampered_archive,
    files = list.files(
      bundle,
      recursive = TRUE,
      all.files = TRUE,
      no.. = TRUE
    ),
    include_directories = FALSE,
    root = bundle,
    mode = "mirror"
  )
  tampered_extract <- file.path(root, "tampered-extract")

  expect_error(
    app$session_archive_extract(tampered_archive, tampered_extract),
    class = "tempest_session_restore_error"
  )
  expect_false(dir.exists(tampered_extract))

  extra_bundle <- file.path(root, "extra-bundle")
  tempest_session_save(ses, extra_bundle)
  writeLines("undeclared", file.path(extra_bundle, "extra.txt"))
  extra_archive <- file.path(root, "extra.zip")
  zip::zip(
    extra_archive,
    files = list.files(
      extra_bundle,
      recursive = TRUE,
      all.files = TRUE,
      no.. = TRUE
    ),
    include_directories = FALSE,
    root = extra_bundle,
    mode = "mirror"
  )
  extra_extract <- file.path(root, "extra-extract")

  expect_error(
    app$session_archive_extract(extra_archive, extra_extract),
    "do not match"
  )
  expect_false(dir.exists(extra_extract))
})

test_that("Shiny session storage is isolated, private, and quota-bound", {
  skip_if_not_installed("shiny")
  app <- source_shiny_modules()
  first <- app$session_storage_root(list(token = "session-one"))
  second <- app$session_storage_root(list(token = "../session-two"))
  on.exit(unlink(c(first, second), recursive = TRUE, force = TRUE), add = TRUE)
  writeLines("private", file.path(first, "private.txt"))
  app$session_secure_permissions(first)

  expect_false(identical(first, second))
  expect_equal(file.exists(file.path(second, "private.txt")), FALSE)
  expect_equal(dirname(first), dirname(second))
  expect_no_match(basename(second), "[.][.]", perl = TRUE)
  if (.Platform$OS.type != "windows") {
    expect_equal(as.character(file.info(first)$mode), "700")
    expect_equal(
      as.character(file.info(file.path(first, "private.txt"))$mode),
      "600"
    )
  }
  expect_error(
    app$session_bundle_enforce_quota(first, max_bytes = 1),
    "storage quota"
  )
  expect_equal(dir.exists(first), FALSE)
})

test_that("session store saves and restores bundles for shared app tabs", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ellmer")
  app <- source_shiny_modules()
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  source_store <- fake_store_with_sources(1)
  source_id <- source_store$list_retrieved_sources()[[1]]$id
  source_store$add_proposed_claim(tempest_claim(
    claim_text = "Shiny restore preserves cited evidence.",
    source_ids = source_id,
    confidence = "high"
  ))
  ses <- tempest_session(
    "Shiny persistence",
    config = cfg,
    experts = list(tempest_expert(
      expert_id = "expert.persist",
      name = "Dr. Persist",
      title = "Researcher",
      description = "Session durability",
      instructions = "Inspect session durability."
    )),
    retriever = tempest_retriever(config = cfg, workspace = source_store)
  )
  tempest:::tempest_session_commit_mindmap(
    ses,
    list(
      nodes = list(
        list(
          id = "root",
          label = "Shiny persistence",
          parent = NULL,
          notes = "",
          source_ids = character()
        ),
        list(
          id = "evidence",
          label = "Evidence",
          parent = "root",
          notes = "Cited evidence.",
          source_ids = source_id
        )
      ),
      edges = list(list(from = "root", to = "evidence", relation = "supports"))
    )
  )
  ses$add_turn("User", "user", "What survives restore?")
  tempest:::tempest_session_append_transcript(
    ses,
    "Moderator",
    "assistant",
    paste0("Cited evidence survives [", source_id, "].")
  )
  ses$emit_progress(
    "stage",
    "succeeded",
    stage = "dialogue",
    step = "turn"
  )

  store <- app$new_session_store()
  bundle_dir <- file.path(withr::local_tempdir(), "session-bundle")
  store$set(ses)
  saved <- store$save(bundle_dir)
  store$set(NULL)

  restored <- store$restore(saved, config = cfg)

  expect_r6_class(restored, "TempestSession")
  expect_equal(restored$topic, "Shiny persistence")
  expect_null(restored$artifacts[["report_md"]])
  expect_null(shiny::isolate(store$report()))
  expect_equal(shiny::isolate(store$persistence())$status, "restored")
  expect_equal(
    tempest_progress_state(tempest_execution_events(restored))$run_id,
    restored$session_id
  )

  chat_calls <- tempest:::tempest_shinychat_restore_messages(
    restored$transcript,
    topic = restored$topic,
    report_available = !is.null(
      tempest:::tempest_session_report_value(restored)
    )
  )
  chat_text <- paste(
    vapply(chat_calls, `[[`, character(1), "content"),
    collapse = "\n"
  )
  chat_roles <- vapply(chat_calls, `[[`, character(1), "role")
  expect_match(chat_text, "Resumed Co-STORM session")
  expect_match(chat_text, "Cited evidence survives")
  expect_contains(chat_roles, c("assistant", "user"))

  shiny::testServer(app$mod_sources_server, args = list(store = store), {
    session$flushReact()
    expect_no_match(as.character(output$body$html), "No sources collected yet")
  })
  shiny::testServer(app$mod_mindmap_server, args = list(store = store), {
    session$flushReact()
    expect_equal(output$n_nodes, "2")
    expect_equal(output$n_sources, "1")
    expect_equal(output$n_facts, "1")
    expect_equal(output$n_turns, "2")
  })
  shiny::testServer(app$mod_report_server, args = list(store = store), {
    session$flushReact()
    expect_match(as.character(output$body$html), "Generate a report")
  })
})

test_that("session autosave debounces store mutations", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ellmer")
  app <- source_shiny_modules()
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  ses <- tempest_session(
    "Autosave topic",
    config = cfg,
    experts = list(tempest_expert(
      expert_id = "expert.autosave",
      name = "Dr. Autosave",
      title = "Researcher",
      description = "Persistence",
      instructions = "Inspect autosave persistence."
    ))
  )
  store <- app$new_session_store()
  bundle_dir <- file.path(withr::local_tempdir(), "autosave-bundle")
  saved_paths <- character()
  errors <- list()

  autosave_server <- function(id, app, store, bundle_dir) {
    shiny::moduleServer(id, function(input, output, session) {
      app$session_autosave_server(
        store = store,
        path = shiny::reactive(bundle_dir),
        enabled = shiny::reactive(TRUE),
        delay_ms = 1,
        on_saved = function(path) {
          saved_paths <<- c(saved_paths, path)
        },
        on_error = function(error) {
          errors[[length(errors) + 1L]] <<- error
        }
      )
    })
  }

  shiny::testServer(
    autosave_server,
    args = list(app = app, store = store, bundle_dir = bundle_dir),
    {
      session$flushReact()
      store$set(ses)
      store$touch()
      store$touch()
      session$flushReact()
      session$elapse(10)
      session$flushReact()

      expect_length(errors, 0L)
      expect_length(saved_paths, 1L)
      expect_true(file.exists(file.path(bundle_dir, "session.json")))
      expect_equal(shiny::isolate(store$persistence())$status, "autosaved")
    }
  )
})

test_that("session autosave does not fire after restoring a bundle", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ellmer")
  app <- source_shiny_modules()
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  ses <- tempest_session(
    "Restore autosave topic",
    config = cfg,
    experts = list(test_expert(
      expert_id = "expert.restore",
      name = "Dr. Restore"
    ))
  )
  store <- app$new_session_store()
  bundle_dir <- file.path(withr::local_tempdir(), "restore-bundle")
  tempest_session_save(ses, bundle_dir)
  saved_paths <- character()

  autosave_server <- function(id, app, store, bundle_dir) {
    shiny::moduleServer(id, function(input, output, session) {
      app$session_autosave_server(
        store = store,
        path = shiny::reactive(bundle_dir),
        enabled = shiny::reactive(TRUE),
        delay_ms = 1,
        on_saved = function(path) {
          saved_paths <<- c(saved_paths, path)
        }
      )
    })
  }

  shiny::testServer(
    autosave_server,
    args = list(app = app, store = store, bundle_dir = bundle_dir),
    {
      session$flushReact()
      store$restore(bundle_dir, config = cfg)
      session$flushReact()
      session$elapse(10)
      session$flushReact()

      expect_length(saved_paths, 0L)
      expect_equal(shiny::isolate(store$persistence())$status, "restored")
    }
  )
})

test_that("tempest_shiny_server renders host-managed shared state", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ellmer")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake_chat()
  )
  source_store <- fake_store_with_sources(1)
  source_id <- source_store$list_retrieved_sources()[[1]]$id
  source_store$add_proposed_claim(tempest_claim(
    claim_text = "Host adapter evidence renders.",
    source_ids = source_id
  ))
  store <- tempest_shiny_store()
  ses <- tempest_session(
    "Embedded host topic",
    config = cfg,
    experts = list(test_expert(
      expert_id = "expert.host",
      name = "Host Expert"
    )),
    retriever = tempest_retriever(config = cfg, workspace = source_store),
    session_id = "host-session-1"
  )
  tempest:::tempest_session_commit_mindmap(
    ses,
    list(
      nodes = list(list(
        id = "root",
        label = "Embedded host topic",
        parent = NULL,
        notes = "",
        source_ids = source_id
      )),
      edges = list()
    )
  )
  ses$add_turn("User", "user", "Show embedded state.")

  shiny::testServer(
    tempest_shiny_server,
    args = list(
      config = cfg,
      store = store,
      panels = c("sources", "facts", "mindmap", "transcript", "report")
    ),
    {
      expect_identical(shared_store, store)
      shared_store$set(ses)
      session$flushReact()

      expect_equal(
        shiny::isolate(shared_store$get())$session_id,
        "host-session-1"
      )
      expect_no_match(
        as.character(output[["sources-body"]]$html),
        "No sources collected yet"
      )
      expect_no_match(
        as.character(output[["facts-body"]]$html),
        "No facts extracted yet"
      )
      expect_equal(output[["mindmap-n_nodes"]], "1")
      expect_match(
        as.character(output[["transcript-body"]]$html),
        "embedded state"
      )
      expect_match(
        as.character(output[["report-body"]]$html),
        "Generate a report"
      )
    }
  )
})

test_that("the example host app parses", {
  path <- system.file(
    "examples",
    "shiny-host",
    "app.R",
    package = "tempest"
  )
  skip_if(identical(path, ""), "example host app not installed")
  expect_no_error(parse(path))
})

test_that("workflow_progress_ui renders reducer state", {
  skip_if_not_installed("shiny")
  app <- source_shiny_modules()
  state <- tempest_progress_state(list(
    tempest_progress_event(
      run_id = "session-1",
      workflow = "costorm",
      event_type = "workflow",
      status = "started",
      stage = "session",
      step = "created"
    ),
    tempest_progress_event(
      run_id = "session-1",
      workflow = "costorm",
      event_type = "stage",
      status = "started",
      stage = "warmup",
      step = "expert_fanout"
    ),
    tempest_progress_event(
      run_id = "session-1",
      workflow = "costorm",
      event_type = "expert",
      status = "started",
      stage = "warmup",
      step = "expert_fanout",
      payload = list(expert_name = "Dr. Flow")
    )
  ))

  html <- paste(
    as.character(app$workflow_progress_ui(state, app$costorm_stage_labels())),
    collapse = ""
  )

  expect_match(html, "Co-STORM")
  expect_match(html, "Running")
  expect_match(html, "Warmup")
  expect_match(html, "Dr. Flow")
  expect_match(html, "tempest-persona-icon")
  expect_match(html, "aria-label=\"Expert Dr. Flow\"", fixed = TRUE)
  expect_match(html, ">DF<", fixed = TRUE)
})

test_that("Co-STORM startup progress renders before warmup starts", {
  skip_if_not_installed("shiny")
  app <- source_shiny_modules()
  event <- app$costorm_starting_event("session-startup")
  state <- app$costorm_progress_state(list(event))
  html <- paste(
    as.character(app$workflow_progress_ui(state, app$costorm_stage_labels())),
    collapse = ""
  )

  expect_equal(state$status, "running")
  expect_equal(state$current_stage, "session")
  expect_match(html, "Co-STORM")
  expect_match(html, "Running")
  expect_match(html, "Setup")
  expect_match(html, "fa-spinner")
})

test_that("Co-STORM startup progress closes when the session is ready", {
  skip_if_not_installed("shiny")
  app <- source_shiny_modules()
  state <- app$costorm_progress_state(list(
    app$costorm_starting_event("session-startup"),
    app$costorm_session_ready_event(
      "session-startup",
      list(experts = list("one"))
    )
  ))
  html <- paste(
    as.character(app$workflow_progress_ui(state, app$costorm_stage_labels())),
    collapse = ""
  )

  expect_equal(state$completed_stages, "session")
  expect_equal(state$current_stage, NA_character_)
  expect_length(state$active$stages, 0L)
  expect_match(html, "Setup")
  expect_no_match(html, "fa-spinner")
})

test_that("workflow_progress_ui renders compact Co-STORM answer labels", {
  skip_if_not_installed("shiny")
  app <- source_shiny_modules()
  state <- tempest_progress_state(list(
    tempest_progress_event(
      run_id = "session-1",
      workflow = "costorm",
      event_type = "stage",
      status = "started",
      stage = "dialogue",
      step = "turn",
      correlation_id = "turn-1"
    ),
    tempest_progress_event(
      run_id = "session-1",
      workflow = "costorm",
      event_type = "step",
      status = "started",
      stage = "dialogue",
      step = "moderator_response",
      correlation_id = "turn-1"
    )
  ))

  html <- paste(
    as.character(app$workflow_progress_ui(state, app$costorm_stage_labels())),
    collapse = ""
  )

  expect_match(html, "Current:")
  expect_match(html, "Answer")
  expect_match(html, "Moderator")
  expect_match(html, "Next")
  expect_no_match(html, "Dialogue")
  expect_no_match(html, "Moderator Response")
})

test_that("async Co-STORM progress renders warmup state before completion", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("later")
  app <- source_shiny_modules()

  progress_server <- function(id, app) {
    shiny::moduleServer(id, function(input, output, session) {
      progress_events <- shiny::reactiveVal(list())
      output$progress <- shiny::renderUI({
        state <- app$costorm_progress_state(progress_events())
        app$workflow_progress_ui(state, app$costorm_stage_labels())
      })
      record <- function(event) {
        app$record_costorm_progress_event(progress_events, event, session)
      }
    })
  }

  shiny::testServer(progress_server, args = list(app = app), {
    event <- tempest_progress_event(
      run_id = "async-progress",
      workflow = "costorm",
      event_type = "stage",
      status = "started",
      stage = "warmup",
      step = "expert_fanout"
    )
    later::later(
      function() {
        record(event)
      },
      delay = 0
    )
    later::run_now(0.01)
    session$flushReact()

    html <- paste(as.character(output$progress$html), collapse = "")
    expect_match(html, "Co-STORM")
    expect_match(html, "Running")
    expect_match(html, "Warmup")
    expect_match(html, "fa-spinner")
  })
})

test_that("storm_progress_state renders failed and running states", {
  skip_if_not_installed("shiny")
  app <- source_shiny_modules()
  running <- app$storm_progress_state(list(app$storm_running_event("Topic")))
  failed <- app$storm_progress_state(list(), "error")

  expect_equal(running$status, "running")
  expect_equal(failed$status, "failed")
  html <- paste(
    as.character(app$workflow_progress_ui(failed, app$storm_stage_labels())),
    collapse = ""
  )
  expect_match(html, "Failed")
  expect_match(html, "STORM pipeline failed")
})

test_that("STORM worker uses a local serializable progress collector", {
  app <- source_shiny_modules()
  collector <- app$storm_worker_progress_collector(include_payload = TRUE)
  event <- tempest_progress_event(
    event_id = "worker-research-start",
    run_id = "worker-run",
    workflow = "storm",
    event_type = "stage",
    status = "started",
    stage = "research",
    payload = list(expert = "Dr. Flow")
  )

  collector$record(event)
  data <- collector$data()

  expect_length(data, 1L)
  expect_equal(data[[1]]$run_id, "worker-run")
  expect_equal(data[[1]]$stage, "research")
  expect_equal(data[[1]]$payload$expert, "Dr. Flow")
  expect_equal(tempest_progress_state(data)$current_stage, "research")

  module_source <- readLines(system.file(
    "shiny",
    "R",
    "mod_storm.R",
    package = "tempest"
  ))
  expect_no_match(
    paste(module_source, collapse = "\n"),
    "tempest::tempest_progress_collector",
    fixed = TRUE
  )
})

test_that("STORM worker writes progress events to a stream", {
  app <- source_shiny_modules()
  stream_path <- withr::local_tempfile(fileext = ".ndjson")
  event <- tempest_progress_event(
    event_id = "stream-research-start",
    run_id = "worker-run",
    workflow = "storm",
    event_type = "stage",
    status = "started",
    stage = "research",
    payload = list(expert = "Dr. Flow")
  )
  collector <- app$storm_worker_progress_collector(
    include_payload = TRUE,
    stream_path = stream_path
  )

  collector$record(event)
  streamed <- app$storm_read_progress_stream(stream_path)

  expect_length(streamed, 1L)
  expect_equal(streamed[[1]]$event_id, "stream-research-start")
  expect_equal(streamed[[1]]$stage, "research")
  expect_equal(streamed[[1]]$step, NA_character_)
  expect_equal(streamed[[1]]$payload$expert, "Dr. Flow")
  expect_equal(tempest_progress_state(streamed)$current_stage, "research")
  expect_length(app$storm_merge_progress_events(list(event), streamed), 1L)
})

test_that("STORM progress stream skips malformed lines", {
  app <- source_shiny_modules()
  stream_path <- withr::local_tempfile(fileext = ".ndjson")
  good <- jsonlite::toJSON(
    list(event_id = "ok", stage = "research"),
    auto_unbox = TRUE
  )
  writeLines(c(good, "{not valid json", ""), stream_path)

  streamed <- app$storm_read_progress_stream(stream_path)

  expect_length(streamed, 1L)
  expect_equal(streamed[[1]]$event_id, "ok")
})

test_that("STORM progress stream reads only newly appended lines", {
  app <- source_shiny_modules()
  stream_path <- withr::local_tempfile(fileext = ".ndjson")
  cursor <- app$storm_progress_stream_cursor()
  first <- jsonlite::toJSON(list(event_id = "one"), auto_unbox = TRUE)
  cat(first, "\n", file = stream_path, sep = "")

  initial <- app$storm_read_progress_stream_incremental(stream_path, cursor)
  expect_length(initial, 1L)
  expect_equal(initial[[1]]$event_id, "one")
  expect_length(
    app$storm_read_progress_stream_incremental(stream_path, cursor),
    0L
  )

  second <- jsonlite::toJSON(list(event_id = "two"), auto_unbox = TRUE)
  cat(second, "\n", file = stream_path, append = TRUE, sep = "")
  appended <- app$storm_read_progress_stream_incremental(stream_path, cursor)
  expect_length(appended, 1L)
  expect_equal(appended[[1]]$event_id, "two")
})

test_that("STORM progress stream defers a partially-written line", {
  app <- source_shiny_modules()
  stream_path <- withr::local_tempfile(fileext = ".ndjson")
  cursor <- app$storm_progress_stream_cursor()
  cat('{"event_id":"partial"', file = stream_path, sep = "")

  expect_length(
    app$storm_read_progress_stream_incremental(stream_path, cursor),
    0L
  )

  cat(',"stage":"research"}\n', file = stream_path, append = TRUE, sep = "")
  completed <- app$storm_read_progress_stream_incremental(stream_path, cursor)
  expect_length(completed, 1L)
  expect_equal(completed[[1]]$event_id, "partial")
})

test_that("STORM progress stream renders while a task is still active", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("later")
  app <- source_shiny_modules()
  stream_path <- withr::local_tempfile(fileext = ".ndjson")

  progress_server <- function(id, app, stream_path) {
    shiny::moduleServer(id, function(input, output, session) {
      active <- TRUE
      progress_events <- shiny::reactiveVal(list(
        app$storm_running_event("Topic", "worker-run")
      ))
      app$storm_poll_progress_stream(
        path = stream_path,
        progress_events = progress_events,
        session = session,
        is_current = function() active,
        interval = 1
      )
      stop_polling <- function() {
        active <<- FALSE
      }
    })
  }

  shiny::testServer(
    progress_server,
    args = list(app = app, stream_path = stream_path),
    {
      collector <- app$storm_worker_progress_collector(
        include_payload = TRUE,
        stream_path = stream_path
      )
      collector$record(tempest_progress_event(
        event_id = "stream-ui-research-start",
        run_id = "worker-run",
        workflow = "storm",
        event_type = "stage",
        status = "started",
        stage = "research"
      ))

      later::run_now(0.05)
      session$flushReact()

      state <- tempest_progress_state(progress_events())
      html <- paste(
        as.character(app$workflow_progress_ui(state, app$storm_stage_labels())),
        collapse = ""
      )
      stop_polling()

      expect_equal(state$current_stage, "research")
      expect_match(html, "STORM")
      expect_match(html, "Research")
      expect_match(html, "fa-spinner")
    }
  )
})

test_that("STORM worker requires current run and progress contracts", {
  app <- source_shiny_modules()
  old_run <- function(
    topic,
    config,
    n_experts,
    research_strategy,
    max_rounds,
    parallel_research,
    verbose
  ) {
    list(report_md = "old run")
  }

  expect_error(
    app$storm_run_with_progress(
      topic = "Topic",
      cfg = tempest_config(),
      n_experts = 1,
      strategy = "key_questions",
      max_rounds = 1,
      parallel = FALSE,
      tempest_run = old_run
    ),
    class = "simpleError"
  )

  seen_run_id <- NULL
  new_run <- function(
    topic,
    config,
    n_experts,
    research_strategy,
    max_rounds,
    parallel_research,
    run_id,
    progress,
    verbose
  ) {
    seen_run_id <<- run_id
    progress(tempest_progress_event(
      run_id = run_id,
      workflow = "storm",
      event_type = "stage",
      status = "started",
      stage = "research"
    ))
    list(report_md = "new run")
  }

  old_collector <- function(include_payload) {
    list(
      record = function(event) invisible(event),
      data = function() list()
    )
  }
  expect_error(
    app$storm_run_with_progress(
      topic = "Topic",
      cfg = tempest_config(),
      n_experts = 1,
      strategy = "key_questions",
      max_rounds = 1,
      parallel = FALSE,
      progress_collector = old_collector,
      tempest_run = new_run
    ),
    class = "simpleError"
  )

  value <- app$storm_run_with_progress(
    topic = "Topic",
    cfg = tempest_config(),
    n_experts = 1,
    strategy = "key_questions",
    max_rounds = 1,
    parallel = FALSE,
    progress_run_id = "shiny-run",
    tempest_run = new_run
  )

  expect_equal(value$result$report_md, "new run")
  expect_equal(seen_run_id, "shiny-run")
  expect_equal(value$progress[[1]]$run_id, "shiny-run")
  expect_equal(value$progress[[1]]$stage, "research")
})

test_that("STORM worker streams progress before mirai resolves", {
  skip_if_not_installed("mirai")
  local_mirai_coverage_dir()
  app <- source_shiny_modules()
  stream_path <- withr::local_tempfile(fileext = ".ndjson")
  release_path <- withr::local_tempfile(fileext = ".release")
  unlink(release_path)
  event <- tempest_progress_event(
    event_id = "mirai-research-start",
    run_id = "worker-run",
    workflow = "storm",
    event_type = "stage",
    status = "started",
    stage = "research"
  )

  value <- mirai::mirai(
    {
      fake_run <- function(
        topic,
        config,
        n_experts,
        research_strategy,
        max_rounds,
        parallel_research,
        run_id,
        progress,
        verbose
      ) {
        progress(event)
        deadline <- Sys.time() + 60
        while (!file.exists(release_path) && Sys.time() < deadline) {
          Sys.sleep(0.02)
        }
        list(report_md = paste("worker run", topic))
      }

      storm_runner(
        topic = topic,
        cfg = cfg,
        n_experts = n_experts,
        strategy = strategy,
        max_rounds = max_rounds,
        parallel = parallel,
        progress_stream_path = progress_stream_path,
        progress_collector = progress_collector,
        tempest_run = fake_run
      )
    },
    topic = "Topic",
    cfg = list(),
    n_experts = 1,
    strategy = "key_questions",
    max_rounds = 1,
    parallel = FALSE,
    progress_stream_path = stream_path,
    progress_collector = app$storm_worker_progress_collector,
    storm_runner = app$storm_run_with_progress,
    event = event,
    release_path = release_path
  )

  seen_stream <- FALSE
  deadline <- Sys.time() + 60
  while (
    mirai::unresolved(value) &&
      !seen_stream &&
      Sys.time() < deadline
  ) {
    seen_stream <- length(app$storm_read_progress_stream(stream_path)) > 0L
    Sys.sleep(0.02)
  }

  expect_equal(seen_stream, TRUE)
  expect_equal(mirai::unresolved(value), TRUE)
  invisible(file.create(release_path))
  result <- value[]
  expect_equal(result$result$report_md, "worker run Topic")
  expect_equal(result$progress[[1]]$stage, "research")
})

test_that("workflow_progress_ui hides recorded failures once succeeded", {
  skip_if_not_installed("shiny")
  app <- source_shiny_modules()
  state <- tempest_progress_state(list(
    tempest_progress_event(
      run_id = "run-1",
      workflow = "storm",
      event_type = "tool",
      status = "failed",
      stage = "research",
      step = "web_search",
      payload = list(error_message = "transient tool error")
    ),
    tempest_progress_event(
      run_id = "run-1",
      workflow = "storm",
      event_type = "workflow",
      status = "succeeded"
    )
  ))

  expect_equal(state$status, "succeeded")
  expect_length(state$failures, 1L)
  html <- paste(
    as.character(app$workflow_progress_ui(state, app$storm_stage_labels())),
    collapse = ""
  )
  expect_no_match(html, "transient tool error")
})

test_that("workflow_progress_ui renders idle Co-STORM sessions as ready", {
  skip_if_not_installed("shiny")
  app <- source_shiny_modules()
  run_id <- "session-1"
  state <- app$costorm_progress_state(list(
    app$costorm_starting_event(run_id),
    app$costorm_session_ready_event(run_id, list(experts = list("one"))),
    tempest_progress_event(
      run_id = run_id,
      workflow = "costorm",
      event_type = "stage",
      status = "started",
      stage = "warmup",
      step = "expert_fanout"
    ),
    tempest_progress_event(
      run_id = run_id,
      workflow = "costorm",
      event_type = "tool",
      status = "failed",
      stage = "warmup",
      step = "expert_question",
      correlation_id = "question-1",
      payload = list(error_message = "timed out")
    ),
    tempest_progress_event(
      run_id = run_id,
      workflow = "costorm",
      event_type = "stage",
      status = "succeeded",
      stage = "warmup",
      step = "expert_fanout"
    )
  ))
  html <- paste(
    as.character(app$workflow_progress_ui(state, app$costorm_stage_labels())),
    collapse = ""
  )

  expect_equal(state$status, "running")
  expect_equal(state$current_stage, NA_character_)
  expect_match(html, "Ready")
  expect_no_match(html, "fa-spinner")
  expect_no_match(html, "timed out")
})

test_that("warmup result messages summarize typed lifecycle results", {
  app <- source_shiny_modules()
  orientation <- function(name, status, error_message = NA_character_) {
    list(
      expert_id = paste0("expert.", tolower(name)),
      expert_name = name,
      expert_session_id = NA_character_,
      deputy_run_id = if (identical(status, "succeeded")) {
        "deputy-run-ready"
      } else {
        NA_character_
      },
      deputy_session_id = if (identical(status, "succeeded")) {
        "deputy-session-ready"
      } else {
        NA_character_
      },
      correlation_id = paste0("warmup-", tolower(name)),
      status = status,
      evidence_status = if (identical(status, "succeeded")) {
        "committed"
      } else {
        "not_run"
      },
      source_ids = if (identical(status, "succeeded")) {
        "S123456789abc"
      } else {
        character()
      },
      sources_added = as.integer(identical(status, "succeeded")),
      claims_added = as.integer(identical(status, "succeeded")),
      failure_kind = if (identical(status, "timeout")) {
        "timeout"
      } else {
        NA_character_
      },
      error_class = if (identical(status, "timeout")) {
        "tempest_operation_error"
      } else {
        NA_character_
      },
      error_message = if (identical(status, "timeout")) {
        "The operation failed."
      } else {
        error_message
      },
      tools_available = TRUE,
      capability_count = 1L,
      session_retired = identical(status, "timeout"),
      cancellation_supported = FALSE
    )
  }
  result <- tempest:::TempestWarmupResult(
    session_id = "session-1",
    status = "succeeded",
    expert_count = 2L,
    orientation_count = 1L,
    failure_count = 1L,
    evidence_failure_count = 0L,
    source_count = 1L,
    claim_count = 1L,
    mindmap_updated = TRUE,
    orientations = list(
      orientation("Dr. Ready", "succeeded"),
      orientation("Dr. Slow", "timeout")
    )
  )

  messages <- app$warmup_result_messages(result)

  expect_length(messages, 2L)
  expect_match(messages[[1]], "Dr. Slow", fixed = TRUE)
  expect_match(messages[[1]], "timed out", fixed = TRUE)
  expect_match(messages[[2]], "Oriented 1 of 2", fixed = TRUE)
  expect_match(messages[[2]], "1 source-backed fact", fixed = TRUE)
})

test_that("turn notices render explicit partial-result guidance", {
  app <- source_shiny_modules()
  gap <- tempest:::tempest_session_turn_notice(
    code = "evidence_gap",
    stage = "evidence",
    message = "No source was inspected."
  )
  failure <- tempest:::tempest_session_turn_error_notice(
    code = "mindmap_failed",
    stage = "mindmap",
    message = "Mind-map update failed.",
    error = simpleError("map unavailable")
  )

  expect_match(app$turn_notice_message(gap), "Treat", fixed = TRUE)
  expect_match(
    app$turn_notice_message(failure),
    "The operation failed.",
    fixed = TRUE
  )
  expect_no_match(
    jsonlite::toJSON(tempest_session_turn_notice_data(failure)),
    "map unavailable",
    fixed = TRUE
  )
})
