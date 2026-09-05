# a briefing retains complete evidence across unchanged days and corrections

    Code
      host$capture_briefing_basis(store, selections)
    Condition
      Error in `FUN()`:
      ! Review the selection before consulting an inactive claim.

---

    Code
      host$read_briefing_basis(store, partial)
    Condition
      Error in `host$read_briefing_basis()`:
      ! The checkpoint is missing part of its selected evidence.

---

    Code
      host$capture_briefing_basis(store, corrected$selections)
    Condition
      Error in `FUN()`:
      ! Selected evidence changed after its receipt; review before checkpointing.

---

    Code
      host$read_briefing_basis(store, stale)
    Condition
      Error in `host$read_briefing_basis()`:
      ! The checkpoint revisions are not covered by the selected receipts.

