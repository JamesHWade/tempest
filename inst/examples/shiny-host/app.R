# This Tempest 0.1 generic-kernel example is frozen and will be removed in
# Tempest 0.2.0. Use the STORM and Co-STORM product APIs for new integrations.

library(shiny)
library(bslib)
library(tempest)

project_connection <- tempest_connection_ref(
  "project-documents",
  provider_id = "example.host",
  connection_type = "document-index",
  title = "Approved project documents",
  description = "Read-only project context selected by the host.",
  scopes = "read"
)

project_read_capability <- tempest_capability_spec(
  "host.project.read",
  title = "Read project context",
  purpose = "Use the project documents approved for this workflow run.",
  instructions = "Use only the granted read-only project connection.",
  operation_id = "host.capability.project.read",
  connection_ref_ids = "project-documents",
  model_roles = "expert"
)

host_expert <- tempest_expert(
  expert_id = "expert.host-analyst",
  name = "Host Analyst",
  title = "Project researcher",
  description = "Uses host-provided context and project constraints.",
  instructions = "Investigate the host project's stated objective.",
  required_capability_ids = "host.project.read",
  initial_questions = "What should this project investigate first?"
)
host_experts <- list(host_expert)

action_register <- tempest_deliverable_spec(
  "action-register",
  title = "Action register",
  purpose = "Turn the objective into a structured set of next actions.",
  instructions = "Return concrete actions with owners and completion signals.",
  content_schema = list(
    type = "object",
    required = c("objective", "actions")
  ),
  required_fields = c("objective", "actions"),
  evidence_policy = "none",
  generator_id = "tempest.generator.provided_content",
  validator_ids = "tempest.validator.required_fields",
  renderer_ids = "host.renderer.json",
  content_type = "action-register",
  media_types = "application/json",
  operation_versions = c(
    "tempest.generator.provided_content" = "1",
    "tempest.validator.required_fields" = "1",
    "host.renderer.json" = "1"
  ),
  requires_approval = TRUE
)

host_objective <- tempest_objective(
  "Turn the approved project context into a reviewable action register.",
  title = "Plan the next project review",
  context = list(
    project = "Example host integration",
    review_window = "next planning cycle"
  ),
  constraints = c(
    "Use only the selected project connection.",
    "Do not execute until a host approves the workflow action.",
    "Do not publish until a host approves the generated output."
  ),
  acceptance_criteria = c(
    "Every action has an owner.",
    "Every action has an observable completion signal."
  ),
  deliverable_ids = "action-register"
)

host_operations <- tempest_builtin_operation_registry()
host_operations$register(
  "host.renderer.json",
  function(content) {
    tempest_artifact_representation(
      content = content,
      artifact_kind = "action-register",
      media_type = "application/json",
      metadata = list(rendered_by = "host.renderer.json")
    )
  },
  kind = "renderer"
)
host_operations$register(
  "host.workflow.build-action-register",
  function(
    objective,
    run_id,
    step,
    expert_id,
    expert_resolutions,
    artifact_catalog
  ) {
    grant <- expert_resolutions[[expert_id]]$grants[["host.project.read"]]
    content <- list(
      objective = objective@title,
      expert_id = expert_id,
      connection_scope = grant$connection_ref_ids,
      actions = list(
        list(
          action = "Review the approved project context",
          owner = expert_id,
          completion_signal = "Constraints and evidence gaps are recorded"
        ),
        list(
          action = "Confirm the next planning decision",
          owner = "host.project-owner",
          completion_signal = "The action register is approved or revised"
        )
      )
    )

    tempest_generate_deliverable(
      action_register,
      context = list(content = content),
      registry = host_operations,
      catalog = artifact_catalog,
      provenance = list(
        artifact_id = "action-register-json",
        run_id = run_id,
        step_id = step@step_id,
        expert_id = expert_id
      )
    )
  },
  kind = "step"
)

host_runtime <- tempest_runtime(
  operations = host_operations,
  capability_specs = list(project_read_capability),
  capability_implementations = list(
    "host.project.read" = list(
      authorize = function(capability_spec, context) {
        list(
          granted = identical(context$expert_id, host_expert@expert_id),
          reason = "The host selected this expert for read-only project access."
        )
      },
      factory = function(capability_spec, connections, context) {
        list(
          tools = list(),
          registrars = list(),
          metadata = list(
            access = "read-only",
            connection_ids = names(connections)
          )
        )
      }
    )
  ),
  connection_refs = list(project_connection),
  connection_bindings = list(
    "project-documents" = list(index = "example-project-context")
  )
)

host_workflow <- tempest_workflow_spec(
  "host.action-register",
  title = "Host action-register workflow",
  purpose = "Demonstrate a reusable non-report Tempest workflow.",
  supported_deliverable_types = "action-register",
  steps = list(tempest_workflow_step(
    "build-action-register",
    title = "Build action register",
    purpose = "Create the structured action register after host approval.",
    operation_id = "host.workflow.build-action-register",
    produced_artifact_ids = "action-register-json",
    assignment_rule = host_expert@expert_id,
    approval_checkpoint = TRUE
  ))
)

host_policy <- function(step, expert_ids, ...) {
  list(
    decision = "require_approval",
    reason = "The host reviews this step before the selected expert runs.",
    metadata = list(
      policy = "example.host.review",
      expert_ids = expert_ids
    )
  )
}

new_host_run <- function() {
  tempest_run_workflow(
    objective = host_objective,
    workflow = host_workflow,
    runtime = host_runtime,
    experts = host_experts,
    connection_permissions = list(
      "expert.host-analyst" = "project-documents"
    ),
    deliverables = list(action_register),
    policy_adapter = host_policy
  )
}

ui <- page_sidebar(
  title = "Embedded tempest host",
  theme = bs_theme(version = 5),
  sidebar = sidebar(
    h4("Requested outcome"),
    p(host_objective@description),
    hr(),
    h5("Run status"),
    uiOutput("run_status"),
    uiOutput("approval_controls"),
    actionButton(
      "reset_run",
      "Start a new run",
      class = "btn-outline-secondary w-100"
    )
  ),
  navset_card_tab(
    nav_panel(
      "Custom workflow",
      layout_columns(
        card(
          card_header("Selected expert and permissions"),
          card_body(
            h5(host_expert@name),
            p(host_expert@title),
            tags$code(host_expert@expert_id),
            p("Capability: read-only access to project-documents")
          )
        ),
        card(
          card_header("Workflow contract"),
          card_body(
            p(host_workflow@purpose),
            tags$code(host_workflow@workflow_id),
            p(
              paste(
                "Deliverable: validated application/json action register",
                "with host approval before publication"
              )
            )
          )
        ),
        col_widths = c(6, 6)
      ),
      card(
        card_header("Action-register artifact"),
        card_body(verbatimTextOutput("action_register"))
      ),
      card(
        card_header("Ordered workflow events"),
        card_body(tableOutput("run_events"))
      )
    ),
    nav_panel(
      "Research workspace",
      tempest_shiny_ui(
        "research",
        panels = c("sources", "facts")
      )
    )
  )
)

server <- function(input, output, session) {
  run <- reactiveVal(new_host_run())
  adapter <- tempest_shiny_server(
    "research",
    panels = c("sources", "facts"),
    experts = host_experts,
    run = run,
    session_id = "example-host-session"
  )

  output$run_status <- renderUI({
    status <- adapter$status()
    class <- switch(
      status,
      awaiting_approval = "text-bg-warning",
      succeeded = "text-bg-success",
      failed = "text-bg-danger",
      "text-bg-secondary"
    )
    span(class = paste("badge", class), status)
  })

  output$approval_controls <- renderUI({
    pending <- adapter$approvals()
    if (length(pending) == 0L) {
      return(NULL)
    }
    approval_kind <- pending[[1]]$approval_kind
    if (is.null(approval_kind)) {
      approval_kind <- "step"
    }
    button_label <- if (identical(approval_kind, "artifact")) {
      "Approve generated output"
    } else {
      "Approve workflow action"
    }
    tagList(
      p(class = "small text-muted", pending[[1]]$reason),
      actionButton(
        "approve_run",
        button_label,
        class = "btn-primary w-100"
      )
    )
  })

  observeEvent(input$approve_run, {
    pending <- adapter$approvals()
    req(length(pending) > 0L)
    adapter$approve(
      names(pending)[[1]],
      note = "Approved in the host application."
    )
  })

  observeEvent(input$reset_run, {
    run(new_host_run())
  })

  output$action_register <- renderText({
    req(identical(adapter$status(), "succeeded"))
    artifact <- tempest_run_artifact(
      adapter$run(),
      "action-register-json"
    )
    jsonlite::toJSON(
      artifact@content,
      auto_unbox = TRUE,
      pretty = TRUE
    )
  })

  output$run_events <- renderTable({
    events <- adapter$events()
    if (length(events) == 0L) {
      return(NULL)
    }
    data.frame(
      sequence = vapply(events, \(event) event$sequence, integer(1)),
      event = vapply(events, \(event) event$event_type, character(1)),
      status = vapply(events, \(event) event$status, character(1)),
      stringsAsFactors = FALSE
    )
  })
}

shinyApp(ui, server)
