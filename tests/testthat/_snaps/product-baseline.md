# default dsprrr STORM semantic outcomes are frozen

    Code
      baseline_snapshot_json(semantics)
    Output
      {
        "completed_stages": ["perspectives", "research", "verification", "outline", "write", "polish"],
        "program_stages": ["perspectives", "personas", "query_decomposition", "extract_claims", "draft_outline", "verify_claim_support", "refined_outline", "section_writing", "lead_section"],
        "source_ids": "S3c9e65929df6",
        "claims": [
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
        "citations": {
          "uses": "S3c9e65929df6",
          "definitions": [
            {
              "citation_id": "S3c9e65929df6",
              "reference": "Progress source. <https://example.org/progress> (retrieved 2026-01-01T00:00:00Z). ✓"
            }
          ]
        },
        "outline_sections": "Workflow evidence",
        "outline_subsections": "Signals",
        "report_sections": ["Progress report", "References", "Execution review"],
        "terminal_status": "succeeded",
        "event_sequence": ["workflow:NA:NA:started", "stage:perspectives:NA:started", "step:persistence:perspectives_artifacts:started", "step:persistence:perspectives_artifacts:succeeded", "stage:perspectives:NA:succeeded", "stage:research:NA:started", "step:persistence:research_artifacts:started", "step:persistence:research_artifacts:succeeded", "stage:research:NA:succeeded", "stage:outline:NA:started", "stage:verification:NA:started", "stage:verification:NA:succeeded", "step:persistence:outline_artifacts:started", "step:persistence:outline_artifacts:succeeded", "stage:outline:NA:succeeded", "stage:write:NA:started", "step:persistence:write_artifacts:started", "step:persistence:write_artifacts:succeeded", "stage:write:NA:succeeded", "stage:polish:NA:started", "step:persistence:polish_artifacts:started", "step:persistence:polish_artifacts:succeeded", "stage:polish:NA:succeeded", "artifact:polish:report_md:available", "workflow:NA:NA:succeeded"]
      }
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
        "resumed_report_sections": ["Progress report", "References", "Execution review"],
        "terminal_status": "succeeded",
        "resumed_event_sequence": ["workflow:NA:NA:started", "stage:perspectives:NA:skipped", "stage:research:NA:skipped", "stage:outline:NA:started", "stage:verification:NA:started", "stage:verification:NA:succeeded", "step:persistence:outline_artifacts:started", "step:persistence:outline_artifacts:succeeded", "stage:outline:NA:succeeded", "stage:write:NA:started", "step:persistence:write_artifacts:started", "step:persistence:write_artifacts:succeeded", "stage:write:NA:succeeded", "stage:polish:NA:started", "step:persistence:polish_artifacts:started", "step:persistence:polish_artifacts:succeeded", "stage:polish:NA:succeeded", "artifact:polish:report_md:available", "workflow:NA:NA:succeeded"]
      }

# Co-STORM warmup and one moderator turn are frozen

    Code
      baseline_snapshot_json(semantics)
    Output
      {
        "session_id": "costorm-product-baseline",
        "source_ids": "S43d2118e4326",
        "claims": [
          {
            "claim_id": "C_0000000000000001",
            "claim_text": "Warmup research is preserved.",
            "claim_type": "finding",
            "source_ids": "S43d2118e4326",
            "evidence_span_ids": "E_0000000000000001",
            "confidence": "high",
            "verification_status": "unverified",
            "support_score": null
          },
          {
            "claim_id": "C_0000000000000002",
            "claim_text": "Moderator research is preserved.",
            "claim_type": "finding",
            "source_ids": "S43d2118e4326",
            "evidence_span_ids": "E_0000000000000002",
            "confidence": "high",
            "verification_status": "unverified",
            "support_score": null
          }
        ],
        "transcript": [
          {
            "speaker": "Dr. Baseline",
            "role": "assistant",
            "source_ids": "S43d2118e4326"
          },
          {
            "speaker": "user",
            "role": "user",
            "source_ids": []
          },
          {
            "speaker": "Moderator",
            "role": "assistant",
            "source_ids": "S43d2118e4326"
          }
        ],
        "mindmap": {
          "nodes": [
            {
              "id": "root",
              "label": "Co-STORM baseline",
              "parent": null,
              "source_ids": "S43d2118e4326"
            }
          ],
          "edges": []
        },
        "report_sections": ["Co\\-STORM baseline", "Findings", "References", "Execution review"],
        "report_citations": {
          "uses": "S43d2118e4326",
          "definitions": [
            {
              "citation_id": "S43d2118e4326",
              "reference": "Co\\-STORM baseline source. <https://example.org/costorm-product-baseline> (retrieved 2026-01-01T00:00:00Z)."
            }
          ]
        },
        "completed_stages": ["warmup", "dialogue", "report"],
        "suggestion_count": 1,
        "report_artifact_matches": true,
        "terminal_status": "running",
        "event_sequence": ["workflow:session:created:started", "stage:warmup:expert_fanout:started", "expert:warmup:expert_fanout:started", "tool:warmup:expert_question:started", "step:evidence:fact_extraction:started", "step:evidence:fact_extraction:succeeded", "step:mindmap:update:started", "step:mindmap:update:succeeded", "tool:warmup:expert_question:succeeded", "expert:warmup:expert_fanout:succeeded", "stage:warmup:expert_fanout:succeeded", "stage:dialogue:turn:started", "step:dialogue:user_turn:succeeded", "step:dialogue:moderator_response:started", "step:dialogue:moderator_response:succeeded", "step:evidence:fact_extraction:started", "step:evidence:fact_extraction:succeeded", "step:mindmap:update:started", "step:mindmap:update:succeeded", "stage:dialogue:turn:succeeded", "step:suggestions:question_generation:started", "step:suggestions:question_generation:succeeded", "stage:report:generate:started", "artifact:report:report_md:available", "stage:report:generate:succeeded"]
      }

# a resumed Co-STORM session can continue product dialogue

    Code
      baseline_snapshot_json(list(restored = before, continued = continued,
        continued_status = tempest_progress_state(restored_events)$status,
        continued_event_sequence = baseline_event_labels(restored_events)))
    Output
      {
        "restored": {
          "session_id": "costorm-product-baseline",
          "source_ids": "S43d2118e4326",
          "claims": [
            {
              "claim_id": "C_0000000000000001",
              "claim_text": "Warmup research is preserved.",
              "claim_type": "finding",
              "source_ids": "S43d2118e4326",
              "evidence_span_ids": "E_0000000000000001",
              "confidence": "high",
              "verification_status": "unverified",
              "support_score": null
            },
            {
              "claim_id": "C_0000000000000002",
              "claim_text": "Moderator research is preserved.",
              "claim_type": "finding",
              "source_ids": "S43d2118e4326",
              "evidence_span_ids": "E_0000000000000002",
              "confidence": "high",
              "verification_status": "unverified",
              "support_score": null
            }
          ],
          "transcript": [
            {
              "speaker": "Dr. Baseline",
              "role": "assistant",
              "source_ids": "S43d2118e4326"
            },
            {
              "speaker": "user",
              "role": "user",
              "source_ids": []
            },
            {
              "speaker": "Moderator",
              "role": "assistant",
              "source_ids": "S43d2118e4326"
            }
          ],
          "mindmap": {
            "nodes": [
              {
                "id": "root",
                "label": "Co-STORM baseline",
                "parent": null,
                "source_ids": "S43d2118e4326"
              }
            ],
            "edges": []
          },
          "report_sections": ["Co\\-STORM baseline", "Findings", "References", "Execution review"],
          "report_citations": {
            "uses": "S43d2118e4326",
            "definitions": [
              {
                "citation_id": "S43d2118e4326",
                "reference": "Co\\-STORM baseline source. <https://example.org/costorm-product-baseline> (retrieved 2026-01-01T00:00:00Z)."
              }
            ]
          }
        },
        "continued": {
          "session_id": "costorm-product-baseline",
          "source_ids": "S43d2118e4326",
          "claims": [
            {
              "claim_id": "C_0000000000000001",
              "claim_text": "Warmup research is preserved.",
              "claim_type": "finding",
              "source_ids": "S43d2118e4326",
              "evidence_span_ids": "E_0000000000000001",
              "confidence": "high",
              "verification_status": "unverified",
              "support_score": null
            },
            {
              "claim_id": "C_0000000000000002",
              "claim_text": "Moderator research is preserved.",
              "claim_type": "finding",
              "source_ids": "S43d2118e4326",
              "evidence_span_ids": "E_0000000000000002",
              "confidence": "high",
              "verification_status": "unverified",
              "support_score": null
            },
            {
              "claim_id": "C_0000000000000003",
              "claim_text": "Continued moderator research is preserved.",
              "claim_type": "finding",
              "source_ids": "S43d2118e4326",
              "evidence_span_ids": "E_0000000000000003",
              "confidence": "high",
              "verification_status": "unverified",
              "support_score": null
            }
          ],
          "transcript": [
            {
              "speaker": "Dr. Baseline",
              "role": "assistant",
              "source_ids": "S43d2118e4326"
            },
            {
              "speaker": "user",
              "role": "user",
              "source_ids": []
            },
            {
              "speaker": "Moderator",
              "role": "assistant",
              "source_ids": "S43d2118e4326"
            },
            {
              "speaker": "user",
              "role": "user",
              "source_ids": []
            },
            {
              "speaker": "Moderator",
              "role": "assistant",
              "source_ids": "S43d2118e4326"
            }
          ],
          "mindmap": {
            "nodes": [
              {
                "id": "root",
                "label": "Co-STORM baseline",
                "parent": null,
                "source_ids": "S43d2118e4326"
              }
            ],
            "edges": []
          },
          "report_sections": ["Co\\-STORM baseline", "Findings", "References", "Execution review"],
          "report_citations": {
            "uses": "S43d2118e4326",
            "definitions": [
              {
                "citation_id": "S43d2118e4326",
                "reference": "Co\\-STORM baseline source. <https://example.org/costorm-product-baseline> (retrieved 2026-01-01T00:00:00Z)."
              }
            ]
          }
        },
        "continued_status": "running",
        "continued_event_sequence": ["workflow:session:created:started", "stage:warmup:expert_fanout:started", "expert:warmup:expert_fanout:started", "tool:warmup:expert_question:started", "step:evidence:fact_extraction:started", "step:evidence:fact_extraction:succeeded", "step:mindmap:update:started", "step:mindmap:update:succeeded", "tool:warmup:expert_question:succeeded", "expert:warmup:expert_fanout:succeeded", "stage:warmup:expert_fanout:succeeded", "stage:dialogue:turn:started", "step:dialogue:user_turn:succeeded", "step:dialogue:moderator_response:started", "step:dialogue:moderator_response:succeeded", "step:evidence:fact_extraction:started", "step:evidence:fact_extraction:succeeded", "step:mindmap:update:started", "step:mindmap:update:succeeded", "stage:dialogue:turn:succeeded", "step:suggestions:question_generation:started", "step:suggestions:question_generation:succeeded", "stage:report:generate:started", "artifact:report:report_md:available", "stage:report:generate:succeeded", "stage:dialogue:turn:started", "step:dialogue:user_turn:succeeded", "step:dialogue:moderator_response:started", "step:dialogue:moderator_response:succeeded", "step:evidence:fact_extraction:started", "step:evidence:fact_extraction:succeeded", "step:mindmap:update:started", "step:mindmap:update:succeeded", "stage:dialogue:turn:succeeded"]
      }

# STORM cancellation is terminal and publishes no report

    Code
      baseline_snapshot_json(list(condition_class = class(condition),
      completed_stages = baseline_succeeded_stages(events), terminal_status = state$
        status, terminal = state$terminal, catalog_report_published = artifacts$
      exists("report_md"), program_stages = fixture$program_stages(), event_sequence = baseline_event_labels(
        events)))
    Output
      {
        "condition_class": ["tempest_run_cancelled", "tempest_run_error", "tempest_error", "interrupt", "rlang_error", "error", "condition"],
        "completed_stages": "perspectives",
        "terminal_status": "cancelled",
        "terminal": true,
        "catalog_report_published": false,
        "program_stages": ["perspectives", "personas"],
        "event_sequence": ["workflow:NA:NA:started", "stage:perspectives:NA:started", "step:persistence:perspectives_artifacts:started", "step:persistence:perspectives_artifacts:succeeded", "stage:perspectives:NA:succeeded", "stage:research:NA:started", "cancellation:research:NA:cancelled"]
      }
