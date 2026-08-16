fake_costorm_warmup_session <- function(
  chat_async = NULL,
  experts = NULL,
  progress = NULL,
  extractor_async = NULL,
  mindmap_async = NULL
) {
  local_mocked_bindings(
    tempest_session_programs = function(session) session$programs,
    tempest_session_stage_recorder = function(session) {
      tempest:::tempest_stage_record_discard
    },
    .env = parent.frame()
  )
  store <- fake_store_with_sources(1)
  source_id <- store$list_retrieved_sources()[[1]]$id
  if (is.null(experts)) {
    experts <- list(test_expert(
      expert_id = "expert.a",
      name = "Dr. A",
      title = "Expert",
      initial_questions = "What matters?"
    ))
  }
  if (is.null(chat_async)) {
    chat_async <- function(prompt, expert, generation) {
      promises::promise_resolve(paste0(
        expert@name,
        " orientation [",
        source_id,
        "]."
      ))
    }
  }
  if (is.null(extractor_async)) {
    extractor_async <- function(...) {
      promises::promise_resolve(list(
        facts = list(list(
          claim = "Warmup finding",
          sources = list(list(source_id = source_id)),
          confidence = "high",
          support_score = 0.9
        ))
      ))
    }
  }
  if (is.null(mindmap_async)) {
    mindmap_async <- function(...) {
      promises::promise_resolve(list(
        nodes = list(
          list(
            id = "root",
            label = "Test topic",
            parent = NULL,
            notes = "",
            source_ids = character()
          ),
          list(
            id = "orientation",
            label = "Orientation",
            parent = "root",
            notes = "",
            source_ids = source_id
          )
        ),
        edges = list()
      ))
    }
  }

  state <- new.env(parent = emptyenv())
  state$turns <- list()
  state$map_updates <- 0L
  state$retired <- 0L
  state$generations <- new.env(parent = emptyenv())
  state$chats <- new.env(parent = emptyenv())
  state$session_keys <- new.env(parent = emptyenv())

  call_chat <- function(prompt, expert, generation) {
    args <- names(formals(chat_async))
    if ("..." %in% args || length(args) >= 3L) {
      return(chat_async(prompt, expert, generation))
    }
    if (length(args) >= 2L) {
      return(chat_async(prompt, expert))
    }
    chat_async(prompt)
  }

  manager <- list()
  manager$get_or_create <- function(expert_id) {
    ids <- vapply(experts, \(expert) expert@expert_id, character(1))
    index <- match(expert_id, ids)
    if (is.na(index)) {
      stop("Unknown expert: ", expert_id)
    }
    expert <- experts[[index]]
    generation <- state$generations[[expert_id]] %||% 0L
    chat <- state$chats[[expert_id]]
    if (is.null(chat)) {
      generation <- generation + 1L
      state$generations[[expert_id]] <- generation
      chat <- list(
        chat_async = function(prompt) {
          call_chat(prompt, expert, generation)
        },
        last_turn = function() NULL
      )
      state$chats[[expert_id]] <- chat
    }
    session_id <- paste0("fake-", expert_id, "-", generation)
    state$session_keys[[session_id]] <- expert_id
    provenance <- new.env(parent = emptyenv())
    provenance$current <- list()
    list(
      chat = chat,
      session_id = session_id,
      provenance = provenance,
      grants = list(research = list(status = "granted"))
    )
  }
  manager$retire_session <- function(session_id) {
    expert_id <- state$session_keys[[session_id]]
    if (
      !is.null(expert_id) && exists(expert_id, state$chats, inherits = FALSE)
    ) {
      rm(list = expert_id, envir = state$chats)
    }
    state$retired <- state$retired + 1L
    list(retired = TRUE, cancellation_supported = FALSE)
  }

  session <- new.env(parent = emptyenv())
  class(session) <- "TempestSession"
  session$session_id <- "warmup-session"
  session$programs <- test_program_executions(run_id = session$session_id)
  session$topic <- "Test topic"
  session$experts <- experts
  session$expert_session_manager <- manager
  session$workspace <- store
  session$mindmap <- tempest:::tempest_mindmap_init(session$topic)
  session$artifacts <- new.env(parent = emptyenv())
  session$state <- state
  session$chats <- list(
    extractor = list(chat_structured_async = extractor_async),
    mindmap = list(chat_structured_async = function(...) {
      state$map_updates <- state$map_updates + 1L
      mindmap_async(...)
    })
  )
  session$harvest_native_sources <- function(chat = NULL, turn = NULL) {
    character()
  }
  session$add_turn <- function(speaker, role, text) {
    state$turns[[length(state$turns) + 1L]] <- list(
      speaker = speaker,
      role = role,
      text = text
    )
    invisible(TRUE)
  }
  session$emit_progress <- function(
    event_type,
    status,
    stage = NA_character_,
    step = NA_character_,
    message = NA_character_,
    payload = list(),
    parent_event_id = NA_character_,
    correlation_id = NA_character_
  ) {
    event <- tempest_progress_event(
      run_id = session$session_id,
      workflow = "costorm",
      event_type = event_type,
      status = status,
      stage = stage,
      step = step,
      message = message,
      payload = payload,
      parent_event_id = parent_event_id,
      correlation_id = correlation_id
    )
    if (is.function(progress)) {
      progress(event)
    }
    event
  }
  session
}
