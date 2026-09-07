##### NFL Flights Dummbells #####
## By: Stephan Teodosescu
## Updated September 2026


library(tidyverse)
library(janitor)
library(scales)
library(tidyverse)
library(ggtext)
library(gridtext)
library(extrafont)


### Inspiration comes from this post: https://mackinawstats.home.blog/2020/02/23/making-expected-goal-charts-in-r/

# Custom ggplot theme (inspired by Owen Phillips at the F5 substack blog)
theme_custom <- function () { 
  theme_minimal(base_size=11, base_family="Outfit") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
    )
}


# create aspect ration to use throughout
asp_ratio <- 1.618

# Function for plot with logo generation
add_logo <- function(plot_path, logo_path, logo_position, logo_scale = 10){
  
  # Requires magick R Package https://github.com/ropensci/magick
  
  # Useful error message for logo position
  if (!logo_position %in% c("top right", "top left", "bottom right", "bottom left")) {
    stop("Error Message: Uh oh! Logo Position not recognized\n  Try: logo_positon = 'top left', 'top right', 'bottom left', or 'bottom right'")
  }
  
  # read in raw images
  plot <- magick::image_read(plot_path)
  logo_raw <- magick::image_read(logo_path)
  
  # get dimensions of plot for scaling
  plot_height <- magick::image_info(plot)$height
  plot_width <- magick::image_info(plot)$width
  
  # default scale to 1/10th width of plot
  # Can change with logo_scale
  logo <- magick::image_scale(logo_raw, as.character(plot_width/logo_scale))
  
  # Get width of logo
  logo_width <- magick::image_info(logo)$width
  logo_height <- magick::image_info(logo)$height
  
  # Set position of logo
  # Position starts at 0,0 at top left
  # Using 0.01 for 1% - aesthetic padding
  
  if (logo_position == "top right") {
    x_pos = plot_width - logo_width - 0.01 * plot_width
    y_pos = 0.01 * plot_height
  } else if (logo_position == "top left") {
    x_pos = 0.01 * plot_width
    y_pos = 0.01 * plot_height
  } else if (logo_position == "bottom right") {
    x_pos = plot_width - logo_width - 0.01 * plot_width
    y_pos = plot_height - logo_height - 0.01 * plot_height
  } else if (logo_position == "bottom left") {
    x_pos = 0.01 * plot_width
    y_pos = plot_height - logo_height - 0.01 * plot_height
  }
  
  # Compose the actual overlay
  magick::image_composite(plot, logo, offset = paste0("+", x_pos, "+", y_pos))
  
}


# load data
flights <- read_csv('https://raw.githubusercontent.com/steodose/nfl-flights/refs/heads/main/inst/extdata/charter_flights.csv')

# data wrangling
flights %>%
  filter(season >= 2024) %>% #filter the year
  select(season, away_team_abbr, distance_mi) %>% #select columns of interest

  group_by(season, away_team_abbr) %>%
  summarise(miles = sum(distance_mi, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = season, values_from = miles, names_prefix = "yr_") %>%
  mutate(change = yr_2025 - yr_2024) -> charter_totals


## ---------- Build plotting frame ----------

# Helmets are pulled from the repo rather than a local path, so the script does
# not depend on the working directory -- same pattern as the CSV loaded above.
# A local relative path fails whenever R is not started in the project root,
# and ggpath only *warns* in that case, so the helmets silently go missing.
helmet_url <- "https://raw.githubusercontent.com/steodose/nfl-flights/main/helmets/"

# Order by the change itself, so segment length reads monotonically down the
# chart: biggest increases at the top, biggest reductions at the bottom.
charter_totals <- charter_totals %>%
  arrange(change) %>%
  mutate(
    away_team_abbr = fct_inorder(away_team_abbr),
    helmet = glue::glue("{helmet_url}{away_team_abbr}_right.png")
  )

# League average per season, drawn as reference lines.
league_avg <- charter_totals %>%
  summarise(across(c(yr_2024, yr_2025), mean))

# Colors
col_2024 <- "#A6ADB4"   # muted gray, prior season
col_2025 <- "#013369"   # NFL navy, current season
col_up   <- "#D50A0A"   # NFL red: flew further
col_down <- "#1B7F5E"   # green: flew less

# Abbreviations go on the right axis; helmets sit outside the panel on the far
# left, where the y axis used to be. clip = "off" lets the helmets draw into
# the plot margin, and the left margin is widened to make room for them.
# gridtext cannot render <img> tags on this machine, so ggpath draws them.
x_min     <- min(charter_totals$yr_2024, charter_totals$yr_2025)
x_max     <- max(charter_totals$yr_2024, charter_totals$yr_2025)
x_range   <- x_max - x_min
helmet_x  <- x_min - 0.085 * x_range   # left of the panel edge (2% expansion)


## ---------- Dumbbell plot ----------

p <- ggplot(charter_totals, aes(y = away_team_abbr)) +
  # Season means, behind everything else
  geom_vline(xintercept = league_avg$yr_2024, color = col_2024,
             linetype = "dashed", linewidth = 0.4) +
  geom_vline(xintercept = league_avg$yr_2025, color = col_2025,
             linetype = "dashed", linewidth = 0.4) +
  # Connector, sized to sit behind the points
  geom_segment(aes(x = yr_2024, xend = yr_2025, yend = away_team_abbr),
               color = "#C9CDD2", linewidth = 3, lineend = "round") +
  geom_point(aes(x = yr_2024), color = col_2024, size = 3.4) +
  geom_point(aes(x = yr_2025), color = col_2025, size = 3.4) +
  # Helmets in the left gutter
  ggpath::geom_from_path(aes(x = helmet_x, path = helmet), width = 0.034) +
  # Change annotation, clear of whichever point sits furthest right
  geom_text(
    aes(x = pmax(yr_2024, yr_2025),
        label = scales::label_number(scale = 1e-3, accuracy = 0.1, suffix = "K",
                                     style_positive = "plus")(change),
        color = change > 0),
    family = "Outfit", fontface = "bold", size = 2.9, hjust = -0.3
  ) +
  scale_color_manual(values = c(`TRUE` = col_up, `FALSE` = col_down), guide = "none") +
  scale_x_continuous(
    labels = scales::label_number(scale = 1e-3, suffix = "K"),
    breaks = scales::breaks_width(5000),
    expand = expansion(mult = c(0.02, 0.13))
  ) +
  # Team abbreviations move to the right-hand axis.
  scale_y_discrete(position = "right") +
  labs(
    title = "Frequent Flier Miles",
    subtitle = glue::glue(
      "Total charter miles flown, <span style='color:{col_2024}'>**2024**</span> vs ",
      "<span style='color:{col_2025}'>**2025**</span>. Bar length is the Y/Y change between ",
      "seasons \u2014 <span style='color:{col_up}'>**more**</span> or ",
      "<span style='color:{col_down}'>**less**</span> travel.<br>",
      "Dashed lines mark each season's league average. Regular season only, both legs of every trip counted."
    ),
    caption = "Data: Doug Kroll (JetTip) | Graphic: Stephan Teodosescu (Between the Pipes)",
    x = "Miles flown",
    y = NULL
  ) +
  coord_cartesian(clip = "off") +
  theme_custom() +
  theme(
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5,
                              margin = margin(b = 4)),
    plot.subtitle = element_markdown(size = 9, lineheight = 1.35,
                                     margin = margin(b = 16), hjust = 0.5),
    plot.caption = element_text(size = 7.5, color = "grey40", hjust = 1,
                                margin = margin(t = 10)),
    axis.text.y.right = element_text(size = 8.5, face = "bold", color = "grey15",
                                     margin = margin(l = 6), hjust = 0),
    axis.title.x = element_text(size = 8.5, color = "grey30", margin = margin(t = 8)),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(18, 14, 12, 52)   # wide left margin holds the helmets
  )

# view plot
p

ggsave("nfl_charter_dumbbells.png", plot = p,
       width = 8.5, height = 10, dpi = 300, device = ragg::agg_png)


## ---------- Add league logo ----------

nfl_logo <- tempfile(fileext = ".png")
download.file(nflreadr::load_teams()$team_league_logo[1], nfl_logo, quiet = TRUE, mode = "wb")

add_logo("nfl_charter_dumbbells.png", nfl_logo, "top right", logo_scale = 20) %>%
  magick::image_write("nfl_charter_dumbbells.png")
