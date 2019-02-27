SR$PF <- d_transcript_cum %>%
  
  # rhs
  unnest(
    rhs,
    .drop = FALSE
  ) %>%
  filter(
    str_detect(rhs, "pass|fail")
  ) %>%
  
  # lhs
  unnest(
    lhs,
    .drop = FALSE
  ) %>%
  filter(
    str_detect(lhs, "pass|fail")
  ) %>%
  
  # rule
  unite(
    col = "rule",
    lhs, rhs,
    sep = " => "
  ) %>%
  
  # rule support
  count(
    rule
  ) %>%
  mutate(
    support = n / n_students
  ) %>%
  
  # regular fucntions
  clean_rules %>%
  
  compute_rhs.support(
    data_support = d_support$PF_HL,
    type_rule    = rate.fail
  ) %>%
  
  compute_conf_lift %>%
  
  # lhs not, rhs low
  filter(
    str_detect(lhs, "fail"),
    str_detect(rhs, "fail"),
  )
