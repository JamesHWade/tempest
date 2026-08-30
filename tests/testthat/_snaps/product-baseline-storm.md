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
        "report_sections": ["Progress report", "At a glance", "Verified observations", "Evidence focus", "Verified observations", "References", "Execution review"],
        "terminal_status": "succeeded",
        "event_sequence": ["workflow:NA:NA:started", "stage:perspectives:NA:started", "step:persistence:perspectives_artifacts:started", "step:persistence:perspectives_artifacts:succeeded", "stage:perspectives:NA:succeeded", "stage:research:NA:started", "step:persistence:research_artifacts:started", "step:persistence:research_artifacts:succeeded", "stage:research:NA:succeeded", "stage:outline:NA:started", "stage:verification:NA:started", "stage:verification:NA:succeeded", "step:persistence:outline_artifacts:started", "step:persistence:outline_artifacts:succeeded", "stage:outline:NA:succeeded", "stage:write:NA:started", "step:persistence:write_artifacts:started", "step:persistence:write_artifacts:succeeded", "stage:write:NA:succeeded", "stage:polish:NA:started", "step:persistence:polish_artifacts:started", "step:persistence:polish_artifacts:succeeded", "stage:polish:NA:succeeded", "artifact:polish:report_md:available", "workflow:NA:NA:succeeded"]
      }
