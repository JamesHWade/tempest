# a resumed published Co-STORM session is read-only

    Code
      baseline_snapshot_json(list(restored = before, restored_status = tempest:::tempest_session_manifest(
        restored)@status, workspace_state = tempest:::tempest_research_workspace_mutation_state(
        tempest:::tempest_session_workspace(restored)), continuation_error_class = class(
        continuation_error), moderator_call_count = length(fixture$resume_runtime$
        moderator_calls())))
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
              "verification_status": "supported",
              "support_score": 0.95
            },
            {
              "claim_id": "C_0000000000000002",
              "claim_text": "Moderator research is preserved.",
              "claim_type": "finding",
              "source_ids": "S43d2118e4326",
              "evidence_span_ids": "E_0000000000000002",
              "confidence": "high",
              "verification_status": "supported",
              "support_score": 0.95
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
                "id": "claim-49eac80939ae926c5f148a4766553ebebdf7690bcebabb908283559cb8ce8c2b",
                "label": "Moderator research is preserved.",
                "parent": "root",
                "source_ids": "S43d2118e4326"
              },
              {
                "id": "claim-6d6ddd56d054a332a4486e1a62e9bda62abfb00b91b6914e2975f1f89d123d07",
                "label": "Warmup research is preserved.",
                "parent": "root",
                "source_ids": "S43d2118e4326"
              },
              {
                "id": "question-78333868b6bdba8dc04928f436d5f854b6566fcc8cbe80347932dcda38999bd3",
                "label": "What should we inspect?",
                "parent": "root",
                "source_ids": []
              },
              {
                "id": "root",
                "label": "Co-STORM baseline",
                "parent": null,
                "source_ids": []
              }
            ],
            "edges": [
              {
                "from": "root",
                "to": "claim-49eac80939ae926c5f148a4766553ebebdf7690bcebabb908283559cb8ce8c2b",
                "relation": "contains"
              },
              {
                "from": "root",
                "to": "claim-6d6ddd56d054a332a4486e1a62e9bda62abfb00b91b6914e2975f1f89d123d07",
                "relation": "contains"
              },
              {
                "from": "root",
                "to": "question-78333868b6bdba8dc04928f436d5f854b6566fcc8cbe80347932dcda38999bd3",
                "relation": "contains"
              }
            ]
          },
          "report_sections": ["Co\\-STORM baseline", "Evidence", "References", "Execution review"],
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
        "restored_status": "succeeded",
        "workspace_state": "sealed",
        "continuation_error_class": ["tempest_session_error", "tempest_error", "rlang_error", "error", "condition"],
        "moderator_call_count": 0
      }
