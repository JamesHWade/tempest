test_typed_web_resource <- function(
  url = "https://example.org/source",
  title = NULL,
  content = "Photosynthesis converts light to chemical energy.",
  snippet = content,
  retrieved_at = "2026-01-01T00:00:00Z"
) {
  tempest_resource(
    resource_kind = "web",
    locator = url,
    title = if (is.null(title)) url else title,
    media_type = "text/html",
    content = content,
    retrieved_at = retrieved_at,
    metadata = list(snippet = snippet)
  )
}

TestTempestResourceSubclass <- S7::new_class(
  "test_tempest_resource_subclass",
  parent = tempest:::TempestResource,
  properties = list(
    extra_state = S7::new_property(S7::class_any, default = NULL)
  )
)

test_subclassed_resource <- function(
  resource = test_typed_web_resource(),
  extra_state = new.env(parent = emptyenv())
) {
  fields <- tempest:::tempest_resource_data_fields()
  values <- stats::setNames(
    lapply(fields, \(field) S7::prop(resource, field)),
    fields
  )
  do.call(
    TestTempestResourceSubclass,
    c(values, list(extra_state = extra_state))
  )
}
