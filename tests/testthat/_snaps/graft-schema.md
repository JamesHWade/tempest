# schema runtime rejects an incompatible Graft contract

    Code
      tempest:::tempest_graft_require()
    Condition
      Error in `tempest_promotion_abort()`:
      ! The installed Graft package reports consumer contract 99.0.0 and store format unknown, but Tempest requires contract >= 0.2.0 and < 0.6.0 and store format 3.1.0.

