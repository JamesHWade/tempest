# Product-owned model selection and fixed scientific tool attachment

tempest_research_tools_abort <- function(message, ..., parent = NULL) {
  tempest_abort(
    message,
    ...,
    class = c("tempest_research_tools_error", "tempest_error"),
    parent = parent,
    .envir = rlang::caller_env()
  )
}

tempest_research_model <- function(config, role) {
  if (!S7::S7_inherits(config, TempestConfig)) {
    tempest_research_tools_abort(
      "{.arg config} must be created by {.fn tempest_config}."
    )
  }
  role <- tempest_research_expert_id(role, "role")
  model <- config@models[[role]] %||% NULL
  if (!rlang::is_string(model) || !nzchar(model)) {
    tempest_research_tools_abort(c(
      "No model is configured for role {.val {role}}.",
      i = "Add the role to {.arg models} in {.fn tempest_config}."
    ))
  }
  model
}

tempest_research_tool_roles <- function() {
  c("coordinator", "expert", "writer", "mindmap", "judge")
}

tempest_research_tool_role <- function(role) {
  role <- tempest_research_expert_id(role, "role")
  if (!role %in% tempest_research_tool_roles()) {
    tempest_research_tools_abort(
      "{.arg role} must be one fixed scientific role from {.val {tempest_research_tool_roles()}}."
    )
  }
  role
}

tempest_research_tools <- function(
  retriever,
  role,
  model = NULL,
  search_provider = "native",
  claim_provenance = list()
) {
  if (!inherits(retriever, "TempestRetriever")) {
    tempest_research_tools_abort(
      "{.arg retriever} must be a TempestRetriever."
    )
  }
  role <- tempest_research_tool_role(role)
  search_provider <- tempest_normalize_search_provider(search_provider)
  claim_provenance <- tempest_product_canonical_list(
    claim_provenance,
    "claim_provenance"
  )

  tools <- tempest_tools_evidence_read(retriever)
  if (role %in% c("coordinator", "expert")) {
    tools <- c(
      tempest_tools_web(
        retriever,
        model = model,
        search_provider = search_provider
      ),
      tools
    )
  }
  if (identical(role, "expert")) {
    tools <- c(
      tools,
      tempest_tools_evidence_write(
        retriever,
        claim_provenance = claim_provenance
      )
    )
  }
  tools
}

tempest_research_attach_tools <- function(
  chat,
  retriever,
  role,
  model = NULL,
  search_provider = "native",
  claim_provenance = list(),
  semantic_retrieval = TRUE
) {
  if (is.null(chat) || !is.function(chat$register_tools)) {
    tempest_research_tools_abort(
      "{.arg chat} must provide a register_tools() method."
    )
  }
  semantic_retrieval <- tempest_product_flag(
    semantic_retrieval,
    "semantic_retrieval"
  )
  tools <- tempest_research_tools(
    retriever = retriever,
    role = role,
    model = model,
    search_provider = search_provider,
    claim_provenance = claim_provenance
  )
  if (length(tools) > 0L) {
    chat$register_tools(tools)
  }
  if (semantic_retrieval) {
    registrar <- tempest_semantic_retrieval_registrar(retriever)
    if (!is.null(registrar)) {
      registrar(chat, context = list(role = role))
    }
  }
  invisible(chat)
}
