# trajectory review data exposes the validated closed projection

    Code
      tempest_trajectory_review_data(lookalike)
    Condition
      Error in `tempest_trajectory_review_abort()`:
      ! `x` must be returned by `tempest_trajectory_review()`.
      x It is a <tempest::TempestTrajectoryReviewLookalike> object.

# trajectory review validation rechecks canonical collection invariants

    Code
      do.call(TempestTrajectoryReview, duplicate)
    Condition
      Error:
      ! <tempest::TempestTrajectoryReview> object is invalid:
      - Trajectory findings retained items must be unique.

---

    Code
      do.call(TempestTrajectoryReview, object_count)
    Condition
      Error:
      ! <tempest::TempestTrajectoryReview> object is invalid:
      - Trajectory findings total must be one nonnegative integer.

# trajectory promotion lanes rebind proposals and acceptance

    Code
      do.call(TempestTrajectoryReview, malformed)
    Condition
      Error:
      ! <tempest::TempestTrajectoryReview> object is invalid:
      - Trajectory accepted revisions retained items must be unique.

