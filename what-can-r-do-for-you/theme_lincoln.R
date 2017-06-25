theme_lincoln <- function(base_size = 11, base_family = "") {
  
  # Lincoln colors
  steel_blue  <- "#37424A"
  red         <- "#822433"
  yellow      <- "#B89002"
  white       <- "#FFFFFF"
  grey        <- "grey80"

  # Starts with theme_grey and then modify some parts
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      
      # Base Inherited Elements
      line               =  element_line(colour = steel_blue, size = 0.5, linetype = 1,
                                         lineend = "butt"),
      rect               =  element_rect(fill = white, colour = steel_blue,
                                         size = 0.5, linetype = 1),
      text               =  element_text(family = base_family, face = "plain",
                                         colour = steel_blue, size = base_size,
                                         lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0,
                                         margin = margin(), debug = FALSE),
      
      # Axes
      axis.line          = element_blank(),
      axis.text          = element_text(size = rel(0.8)),
      axis.ticks         = element_line(color = grey, size = rel(1/3)),
      axis.title         = element_text(size = rel(1.0)),
      
      # Panel
      panel.background   = element_rect(fill = white, color = NA),
      panel.border       = element_rect(fill = NA, size = rel(1/2), color = steel_blue),
      panel.grid.major   = element_line(color = grey, size = rel(1/3)),
      panel.grid.minor   = element_line(color = grey, size = rel(1/3)),
      panel.grid.minor.x = element_blank(),
      panel.spacing      = unit(.75, "cm"),
      
      # Legend
      legend.key         = element_rect(fill = white, color = NA),
      legend.position    = "bottom",
      
      # Strip (Used with multiple panels)
      strip.background   = element_rect(fill = red, color = red),
      strip.text         = element_text(color = white, size = rel(0.8)),
      
      # Plot
      plot.title         = element_text(size = rel(1.2), hjust = 0, colour = red,
                                        margin = margin(t = 0, r = 0, b = 4, l = 0, unit = "pt")),
      plot.subtitle      = element_text(size = rel(0.9), hjust = 0, colour = red,
                                        margin = margin(t = 0, r = 0, b = 3, l = 0, unit = "pt")),
      
      # Complete theme
      complete = TRUE
    )
}



palette_lincoln <- function() {
  c(
    "#37424A", # steel blue
    "#822433", # red
    "#B89002", # yellow
    "#387B98"  # navy_blue
  )
}

scale_fill_lincoln <- function(..., theme = "light") {
  scale_fill_manual(values = palette_lincoln())
}
