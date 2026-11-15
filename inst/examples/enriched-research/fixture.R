# Authored offline replay for the enriched CO2 example.
# This fixture exercises Tempest's public STORM product path; it does not
# evaluate a live model, search provider, or the quality of a live response.

TempestCO2ReplayChat <- R6::R6Class(
  "TempestCO2ReplayChat",
  inherit = ellmer::Chat,
  public = list(
    initialize = function(
      data_statement,
      study_statement,
      protocol_statement,
      data_source_id,
      study_source_id,
      system_prompt = NULL
    ) {
      private$data_statement <- data_statement
      private$study_statement <- study_statement
      private$protocol_statement <- protocol_statement
      private$data_source_id <- data_source_id
      private$study_source_id <- study_source_id
      super$initialize(
        provider = ellmer::Provider(
          name = "tempest-authored-replay",
          base_url = "https://example.invalid"
        ),
        model = ellmer::Model(name = "authored-offline-fixture"),
        system_prompt = system_prompt,
        echo = "none"
      )
    },
    chat_structured = function(..., type, echo = "none", convert = TRUE) {
      prompt <- paste(unlist(list(...)), collapse = "\n")
      fields <- names(type@properties)
      data_statement <- private$data_statement
      study_statement <- private$study_statement
      protocol_statement <- private$protocol_statement
      data_source_id <- private$data_source_id
      study_source_id <- private$study_source_id
      outline <- list(
        title = "Cold treatment and CO2 uptake",
        sections = list(list(
          title = "Study design and response curves",
          summary = paste(
            "The R help describes 12 plants and summarizes chilling as",
            "overnight; the original Methods specify 14 hours at 7 C followed",
            "by 10 hours of recovery at 20 C and report matched plant pairs."
          ),
          subsections = list(list(
            title = "What the dataset supports",
            bullets = c(
              "Compare uptake across the tabulated CO2 concentration levels.",
              "Use the original paper's plant pairs within each origin.",
              "Treat plant, not each repeated concentration reading, as the unit."
            ),
            needed = "The built-in CO2 dataset and its documentation"
          ))
        ))
      )
      value <- if ("perspectives" %in% fields) {
        list(
          title = outline$title,
          perspectives = list(list(
            name = "Experimental design",
            description = paste(
              "Read the dataset documentation carefully and keep plant-level",
              "replication distinct from repeated concentration readings."
            ),
            key_questions = "What does the documented design allow us to compare?"
          ))
        )
      } else if ("queries" %in% fields) {
        list(queries = "R datasets CO2 Echinochloa crus-galli chilling")
      } else if ("facts" %in% fields) {
        list(
          facts = list(
            list(
              claim = data_statement,
              sources = list(list(
                source_id = data_source_id,
                quote = data_statement
              )),
              confidence = "high"
            ),
            list(
              claim = study_statement,
              sources = list(list(
                source_id = study_source_id,
                quote = study_statement
              )),
              confidence = "high"
            ),
            list(
              claim = protocol_statement,
              sources = list(list(
                source_id = study_source_id,
                quote = protocol_statement
              )),
              confidence = "high"
            )
          )
        )
      } else if ("status" %in% fields) {
        list(
          status = "supported",
          score = 0.95,
          rationale = "The replayed statements match their authored local source notes."
        )
      } else if ("sections" %in% fields) {
        outline
      } else if ("items" %in% fields) {
        prompt_lines <- strsplit(prompt, "\n", fixed = TRUE)[[1L]]
        evidence_lines <- prompt_lines[grepl("^- ", prompt_lines)]
        matches <- lapply(evidence_lines, function(line) {
          regmatches(
            line,
            regexec(
              "^- (.*?) \\[[^]]+\\] \\(claim_id: ([^)]+)\\)",
              line,
              perl = TRUE
            )
          )[[1L]]
        })
        matches <- Filter(function(value) length(value) == 3L, matches)
        if (length(matches) == 0L) {
          stop("The replay received no grounded claim identity.")
        }
        list(
          items = lapply(matches, function(match) {
            list(
              kind = "observation",
              text = match[[2L]],
              claim_ids = list(match[[3L]])
            )
          })
        )
      } else {
        stop(
          "The replay has no response for structured fields: ",
          paste(fields, collapse = ", ")
        )
      }
      private$record(
        prompt,
        ellmer::ContentText(as.character(jsonlite::toJSON(
          value,
          auto_unbox = TRUE
        )))
      )
      value
    },
    chat = function(..., echo = "none") {
      prompt <- paste(unlist(list(...)), collapse = "\n")
      response <- private$answer()
      private$record(prompt, ellmer::ContentText(response))
      response
    },
    chat_async = function(..., echo = "none") {
      promises::promise_resolve(self$chat(..., echo = echo))
    },
    chat_structured_async = function(..., type, echo = "none", convert = TRUE) {
      promises::promise_resolve(self$chat_structured(
        ...,
        type = type,
        echo = echo,
        convert = convert
      ))
    },
    stream = function(..., stream = c("text", "content"), controller = NULL) {
      prompt <- paste(unlist(list(...)), collapse = "\n")
      response <- private$answer()
      coro::generator(function() {
        tool <- self$get_tools()[["get_retrieved_source"]]
        if (!is.null(tool)) {
          request <- ellmer::ContentToolRequest(
            id = paste0("co2-replay-source-", length(self$get_turns()) + 1L),
            name = "get_retrieved_source",
            arguments = list(source_id = private$study_source_id),
            tool = tool
          )
          private$record(prompt, request)
          coro::yield(request)
          result <- private$invoke_tool(tool, request)
          private$tool_result_callback(result)
          coro::yield(result)
        }
        coro::yield(ellmer::ContentText(response))
        private$record(prompt, ellmer::ContentText(response))
      })()
    },
    stream_async = function(
      ...,
      stream = c("text", "content"),
      controller = NULL
    ) {
      source <- self$stream(..., stream = stream, controller = controller)
      coro::async_generator(function() {
        repeat {
          content <- source()
          if (coro::is_exhausted(content)) {
            break
          }
          coro::yield(content)
        }
      })()
    },
    on_tool_request = function(callback) {
      private$tool_request_callback <- callback
      invisible(self)
    },
    on_tool_result = function(callback) {
      private$tool_result_callback <- callback
      invisible(self)
    }
  ),
  private = list(
    data_statement = NULL,
    study_statement = NULL,
    protocol_statement = NULL,
    data_source_id = NULL,
    study_source_id = NULL,
    tool_request_callback = NULL,
    tool_result_callback = NULL,
    invoke_tool = function(tool, request) {
      value <- tryCatch(
        {
          private$tool_request_callback(request)
          tool(source_id = private$study_source_id)
        },
        error = identity
      )
      if (inherits(value, "error")) {
        ellmer::ContentToolResult(
          value = NULL,
          error = conditionMessage(value),
          request = request
        )
      } else {
        ellmer::ContentToolResult(value = value, request = request)
      }
    },
    answer = function() {
      paste0(
        private$data_statement,
        " [",
        private$data_source_id,
        "]. ",
        private$study_statement,
        " [",
        private$study_source_id,
        "]. ",
        private$protocol_statement,
        " [",
        private$study_source_id,
        "]."
      )
    },
    record = function(prompt, content) {
      self$add_turn(
        ellmer::UserTurn(list(ellmer::ContentText(prompt))),
        ellmer::AssistantTurn(
          list(content),
          tokens = c(0, 0, 0),
          cost = 0,
          finish_reason = "stop"
        ),
        log_tokens = FALSE
      )
    }
  )
)

tempest_co2_replay_fixture <- function() {
  data_statement <- paste(
    "The R datasets help describes 84 uptake rows from 12 Echinochloa",
    "crus-galli plants: six from Quebec and six from Mississippi; half of",
    "each origin group were chilled overnight."
  )
  data_url <- "https://www.stat.ethz.ch/R-manual/R-devel/library/datasets/html/zCO2.html"
  data_source <- tempest::tempest_resource(
    resource_kind = "web",
    locator = data_url,
    title = "Carbon Dioxide Uptake in Grass Plants (R datasets help)",
    media_type = "text/html",
    content = paste(
      "Authored offline note from the R datasets help page.\n",
      data_statement,
      "The help labels ambient CO2 concentration in mL/L and uptake in",
      "umol/m^2 sec. Its concentration unit label differs from Table 1 in",
      "the cited paper; see the worked example's source notes."
    ),
    retrieved_at = "2026-11-14T00:00:00Z"
  )
  study_statement <- paste(
    "Potvin et al. describe control and chilled plants paired before",
    "treatment within each population, with the seven CO2 levels listed",
    "in Table 1 as 95, 175, 250, 350, 500, 675, and 1000 microliters per liter."
  )
  protocol_statement <- paste(
    "The paper's Methods specify 14 hours of chilling at 7 degrees C",
    "followed by 10 hours of recovery at 20 degrees C."
  )
  study_url <- "https://biology.mcgill.ca/faculty/potvin/articles/PotvinLech_90.pdf"
  study_source <- tempest::tempest_resource(
    resource_kind = "web",
    locator = study_url,
    title = "Potvin, Lechowicz, and Tardif (1990), original study",
    media_type = "text/plain",
    content = paste(
      "Authored offline summary of the cited primary paper, pp. 1390-1391;",
      "this fixture does not include or quote the full article.\n",
      study_statement,
      protocol_statement,
      "Table 1 pairs",
      "Quebec control plants 1, 2, 3 with chilled plants 6, 4, 5, and",
      "Mississippi control plants 1, 2, 3 with chilled plants 6, 5, 4."
    ),
    retrieved_at = "2026-11-14T00:00:00Z"
  )
  data_source_id <- data_source@resource_id
  study_source_id <- study_source@resource_id
  config <- tempest::tempest_config(
    cache_enabled = FALSE,
    citation_policy = "claim_verified",
    chat_fn = function(role, model, system_prompt, echo) {
      TempestCO2ReplayChat$new(
        data_statement,
        study_statement,
        protocol_statement,
        data_source_id,
        study_source_id,
        system_prompt
      )
    }
  )
  expert <- tempest::tempest_expert(
    name = "Dataset and study reviewer",
    title = "Plant physiology methods reviewer",
    description = "Checks the dataset description against its original study.",
    instructions = "Distinguish the R help page from the paper's original design and units."
  )
  workspace <- tempest::tempest_research_workspace()
  workspace$upsert_retrieved_resource(data_source)
  workspace$upsert_retrieved_resource(study_source)
  topic <- "Chilling treatment and carbon dioxide uptake in Echinochloa crus-galli"
  retriever <- list(
    workspace = workspace,
    search = function(query, k) {
      # Perspective seeding starts from the topic. Returning no URLs here avoids
      # a table-of-contents fetch; later targeted queries use this local record.
      if (identical(query, topic)) {
        return(data.frame(
          title = character(),
          url = character(),
          snippet = character()
        ))
      }
      data.frame(
        title = c(data_source@title, study_source@title),
        url = c(data_url, study_url),
        snippet = c(data_statement, study_statement)
      )
    },
    fetch = function(requested_url) {
      if (identical(requested_url, data_url)) {
        return(data_source)
      }
      if (identical(requested_url, study_url)) {
        return(study_source)
      }
      if (!requested_url %in% c(data_url, study_url)) {
        stop("Unknown URL in the authored offline fixture.")
      }
    }
  )
  list(
    config = config,
    experts = list(expert),
    retriever = retriever,
    source = data_source,
    study_source = study_source,
    topic = topic
  )
}

tempest_co2_replay_research <- function(output_dir = NULL) {
  fixture <- tempest_co2_replay_fixture()
  tempest::tempest_run(
    fixture$topic,
    config = fixture$config,
    experts = fixture$experts,
    retriever = fixture$retriever,
    max_questions_per_perspective = 1L,
    output_dir = output_dir,
    run_id = "co2-replay-fixture",
    verbose = FALSE
  )
}
