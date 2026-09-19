# proposed claims cannot become accepted through workspace mutation

    Code
      S7::set_props(claim, accepted = TRUE)
    Condition
      Error:
      ! Can't find property <tempest::tempest_claim>@accepted

