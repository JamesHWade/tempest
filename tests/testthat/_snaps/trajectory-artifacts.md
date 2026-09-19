# closed reviews reject obsolete schemas and inconsistent publication joins

    Code
      do.call(TempestTrajectoryReview, values)
    Condition
      Error:
      ! <tempest::TempestTrajectoryReview> object is invalid:
      - schema_version must be the current trajectory projection

---

    Code
      do.call(TempestTrajectoryReview, values)
    Condition
      Error:
      ! <tempest::TempestTrajectoryReview> object is invalid:
      - Trajectory joins omit a mandatory projected relation.

