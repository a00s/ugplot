test_that("checkbox filters remain executable JavaScript after UI rendering", {
  app_ui <- ugplot_test_internal("ui")
  rendered_html <- htmltools::renderTags(app_ui)$html
  script_match <- regmatches(
    rendered_html,
    regexpr(
      "(?s)<script>\\s*function filterCheckboxGroup.*?</script>",
      rendered_html,
      perl = TRUE
    )
  )

  expect_length(script_match, 1L)
  expect_match(script_match, "setupTableListFilters", fixed = TRUE)
  expect_match(script_match, "values.length > 1", fixed = TRUE)
  expect_false(grepl("&gt;|&lt;", script_match))
})
