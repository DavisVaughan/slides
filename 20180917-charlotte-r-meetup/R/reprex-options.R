withr::with_options(
  new = c(
    reprex.highlight.hl_style  = "edit-eclipse",
    reprex.highlight.font      = "Fira Code",
    reprex.highlight.font_size = 24
  ),
  reprex::reprex(venue = "rtf")
)
