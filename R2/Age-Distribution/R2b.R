library(readr)
library(dplyr)
library(ggplot2)
library(ggtext)
library(glue)
library(sysfonts)
library(showtext)

font_add_google("Roboto Slab", family = "rs")
font_add_google("Lato",        family = "Lato")
sysfonts::font_add('fb', '/home/stelios/Downloads/fontawesome-free-6.7.2-desktop/otfs/Font Awesome 6 Brands-Regular-400.otf')
sysfonts::font_add('fs', '/home/stelios/Downloads/fontawesome-free-6.7.2-desktop/otfs/Font Awesome 6 Free-Solid-900.otf')
showtext_auto()
showtext::showtext_opts(dpi = 300)

# --- Data ---
kaggle_2021 <- read_csv("R2/kaggle_survey_2021.csv")
kaggle_2021 <- kaggle_2021[-c(1), ]

kaggle_2021_compare <- kaggle_2021 %>%
  mutate(Q3 = if_else(Q3 != "Greece", "Other", Q3))

# --- make_plot ---
make_plot <- function(title_text, subtitle_text, caption_text,
                      x_lab, y_lab, fill_labels = NULL,
                      bar_alpha = 1,
                      bg, text_color, subtle_color, grid_color,
                      base_family = "Lato", base_size = 10) {

  my_theme <- theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.background    = element_rect(fill = bg, color = NA),
      panel.background   = element_rect(fill = bg, color = NA),
      panel.grid.major.y = element_line(color = grid_color, linewidth = 0.35),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      text               = element_text(color = text_color),
      axis.text          = element_text(color = subtle_color),
      axis.title         = element_text(color = text_color),
      axis.title.x       = element_blank(),
      plot.title         = element_markdown(
        size = base_size + 4, face = "bold", color = text_color,
        hjust = 0.5, margin = margin(t = 5, b = 5)
      ),
      plot.subtitle      = element_textbox_simple(
        family = base_family, size = base_size + 0.5, lineheight = 1.1,
        color = subtle_color, fill = NA, box.color = NA,
        margin = margin(t = 5, b = 5)
      ),
      plot.caption       = element_markdown(color = subtle_color, lineheight = 1.2),
      legend.position    = c(0.85, 0.85),
      legend.background  = element_blank(),
      legend.key         = element_blank(),
      legend.text        = element_text(color = text_color)
    )

  fill_scale <- if (!is.null(fill_labels)) {
    scale_fill_manual(values = c("Greece" = "#1f77b4", "Other" = "#ff7f0e"),
                      labels = fill_labels)
  } else {
    scale_fill_manual(values = c("Greece" = "#1f77b4", "Other" = "#ff7f0e"))
  }

  kaggle_2021_compare %>%
    select(Q3, Q1) %>%
    group_by(Q3, Q1) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(Q3) %>%
    mutate(total = sum(n),
           pct   = round(n / total * 100, digits = 1)) %>%
    select(Q3, Q1, pct) %>%
    ggplot(aes(x = Q1, y = pct + 1, fill = Q3)) +
    geom_bar(stat = "identity", position = "dodge", alpha = bar_alpha) +
    fill_scale +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title = title_text, subtitle = subtitle_text, caption = caption_text,
         x = x_lab, y = y_lab, fill = "") +
    my_theme
}

# --- English texts ---
en_title    <- "<b>Age Distribution of Kaggle Community</b>"
en_subtitle <- "Greek Kagglers show a bimodal age distribution, peaking at 25–29 and again at 45–49, while the global community is more concentrated in the 18–29 range."
en_caption  <- "<b>Sources:</b> Kaggle Survey 2021<br><b>Visualization:</b> <span style='font-family:fb;'>&#xf09b;</span> stesiam, 2023"

# --- Greek texts ---
gr_title    <- "<b>Ηλικιακή κατανομή χρηστών Kaggle</b>"
gr_subtitle <- "Οι Έλληνες χρήστες Kaggle εμφανίζουν δικόρυφη ηλικιακή κατανομή, με κορυφές στο 25–29 και στο 45–49, ενώ η παγκόσμια κοινότητα συγκεντρώνεται περισσότερο στην ηλικιακή ζώνη 18–29."
gr_caption  <- "<b>Πηγή:</b> Έρευνα χρηστών Kaggle 2021<br><b>Γράφημα:</b> <span style='font-family:fb;'>&#xf09b;</span> stesiam, 2023"

# --- Generate all 4 variants ---
variants <- list(
  list(lang = "en", theme = "light",
       x_lab = "Age Group",       y_lab = "Percentage (%)",
       bar_alpha = 0.5,
       bg = "white",    text_color = "#222222", subtle_color = "#444444", grid_color = "grey90",
       base_family = "Lato", base_size = 9),
  list(lang = "en", theme = "dark",
       x_lab = "Age Group",       y_lab = "Percentage (%)",
       bar_alpha = 0.5,
       bg = "#181818", text_color = "white",    subtle_color = "gray70",  grid_color = "grey25",
       base_family = "Lato", base_size = 9),
  list(lang = "el", theme = "light",
       x_lab = "Ηλικιακή ομάδα", y_lab = "Ποσοστό (%)",
       fill_labels = c("Greece" = "Ελλάδα", "Other" = "Υπόλοιπες Χώρες"),
       bar_alpha = 0.5,
       bg = "white",    text_color = "#222222", subtle_color = "#444444", grid_color = "grey90",
       base_family = "rs", base_size = 8),
  list(lang = "el", theme = "dark",
       x_lab = "Ηλικιακή ομάδα", y_lab = "Ποσοστό (%)",
       fill_labels = c("Greece" = "Ελλάδα", "Other" = "Υπόλοιπες Χώρες"),
       bar_alpha = 0.5,
       bg = "#181818", text_color = "white",    subtle_color = "gray70",  grid_color = "grey25",
       base_family = "rs", base_size = 8)
)

for (v in variants) {
  title_text    <- if (v$lang == "en") en_title    else gr_title
  subtitle_text <- if (v$lang == "en") en_subtitle else gr_subtitle
  caption_text  <- if (v$lang == "en") en_caption  else gr_caption

  plt <- make_plot(
    title_text    = title_text,
    subtitle_text = subtitle_text,
    caption_text  = caption_text,
    x_lab         = v$x_lab,
    y_lab         = v$y_lab,
    fill_labels   = v$fill_labels,
    bar_alpha     = v$bar_alpha,
    bg            = v$bg,
    text_color    = v$text_color,
    subtle_color  = v$subtle_color,
    grid_color    = v$grid_color,
    base_family   = v$base_family,
    base_size     = v$base_size
  )

  ggsave(
    filename = glue("R2/Age-Distribution/r2b-kaggle-age-dist-{v$theme}-{v$lang}.png"),
    plot     = plt,
    device   = "png",
    dpi      = 300,
    width    = 6,
    height   = 4
  )
}
