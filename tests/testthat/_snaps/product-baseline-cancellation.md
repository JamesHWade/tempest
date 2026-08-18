# STORM cancellation is terminal and publishes no report

    Code
      baseline_snapshot_json(list(condition_class = class(condition),
      completed_stages = baseline_succeeded_stages(events), terminal_status = state$
        status, terminal = state$terminal, report_published = !is.null(persisted$
        research_manifest$deliverables$report_md), program_stages = fixture$
        program_stages(), event_sequence = baseline_event_labels(events)))
    Output
      {
        "condition_class": ["tempest_run_cancelled", "tempest_run_error", "tempest_error", "interrupt", "rlang_error", "error", "condition"],
        "completed_stages": "perspectives",
        "terminal_status": "cancelled",
        "terminal": true,
        "report_published": false,
        "program_stages": ["perspectives", "personas"],
        "event_sequence": ["workflow:NA:NA:started", "stage:perspectives:NA:started", "step:persistence:perspectives_artifacts:started", "step:persistence:perspectives_artifacts:succeeded", "stage:perspectives:NA:succeeded", "stage:research:NA:started", "cancellation:research:NA:cancelled"]
      }
