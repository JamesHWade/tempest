# scripted STORM resumes its immutable full-run request

    Code
      baseline_snapshot_json(list(initial_completed_stages = baseline_succeeded_stages(
        fixture$first_events), resumed_program_stages = resumed$program_stages,
      resumed_source_ids = resumed$source_ids, resumed_claims = resumed$claims,
      resumed_citations = resumed$citations, resumed_outline_sections = resumed$
        outline_sections, resumed_report_sections = resumed$report_sections,
      terminal_status = resumed$terminal_status, resumed_event_sequence = resumed$
        event_sequence))
    Output
      {
        "initial_completed_stages": ["perspectives", "research"],
        "resumed_program_stages": ["perspectives", "personas", "query_decomposition", "extract_claims", "draft_outline", "verify_claim_support", "refined_outline", "section_writing", "lead_section"],
        "resumed_source_ids": "S3c9e65929df6",
        "resumed_claims": [
          {
            "claim_id": "C_0000000000000001",
            "claim_text": "STORM progress emits stage events.",
            "claim_type": "finding",
            "source_ids": "S3c9e65929df6",
            "evidence_span_ids": "E_0000000000000001",
            "confidence": "high",
            "verification_status": "supported",
            "support_score": 0.9
          }
        ],
        "resumed_citations": {
          "uses": "S3c9e65929df6",
          "definitions": [
            {
              "citation_id": "S3c9e65929df6",
              "reference": "Progress source. <https://example.org/progress> (retrieved 2026-01-01T00:00:00Z). ✓"
            }
          ]
        },
        "resumed_outline_sections": "Workflow evidence",
        "resumed_report_sections": ["Progress report", "At a glance", "Verified observations", "Evidence focus: Workflow evidence", "Verified observations", "References", "Execution review"],
        "terminal_status": "succeeded",
        "resumed_event_sequence": ["workflow:NA:NA:started", "stage:perspectives:NA:skipped", "stage:research:NA:skipped", "stage:outline:NA:started", "stage:verification:NA:started", "stage:verification:NA:succeeded", "step:persistence:outline_artifacts:started", "step:persistence:outline_artifacts:succeeded", "stage:outline:NA:succeeded", "stage:write:NA:started", "step:persistence:write_artifacts:started", "step:persistence:write_artifacts:succeeded", "stage:write:NA:succeeded", "stage:polish:NA:started", "step:persistence:polish_artifacts:started", "step:persistence:polish_artifacts:succeeded", "stage:polish:NA:succeeded", "artifact:polish:report_md:available", "workflow:NA:NA:succeeded"]
      }
