library(readr)
library(dplyr)
library(forcats)
library(ggplot2)
library(ggtext)
library(countrycode)
library(glue)
library(sysfonts)
library(showtext)

# --- Fonts ---
font_add_google("Outfit",      "Outfit")
font_add_google("Inter",       "Inter")
font_add_google("Lato",        "Lato")
font_add_google("Roboto Slab", "rs")
sysfonts::font_add('fb', '/home/stelios/Downloads/fontawesome-free-6.7.2-desktop/otfs/Font Awesome 6 Brands-Regular-400.otf')
showtext_auto()
showtext::showtext_opts(dpi = 300)

# --- Colors ---
bar_low      <- "#C4A8D8"
bar_high     <- "#5B2A86"
avg_color    <- "#D4615A"
greece_color <- "#0D5EAF"

# --- Themes ---
theme_light_custom <- function(base_size = 9, base_family = "Lato", title_family = "Outfit") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.background    = element_rect(fill = "#FAFAFA", color = NA),
      panel.background   = element_rect(fill = "#FAFAFA", color = NA),
      panel.grid.major.x = element_line(color = "#E5E5E5", linewidth = 0.3),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.title.x       = element_text(size = base_size - 0.5, color = "#888888",
                                        margin = margin(t = 8)),
      axis.title.y       = element_blank(),
      axis.text.y        = element_text(size = base_size - 0.5, color = "#333333",
                                        margin = margin(r = 4)),
      axis.text.x        = element_text(size = base_size - 1, color = "#999999"),
      axis.ticks         = element_blank(),
      plot.title         = element_markdown(family = title_family, size = base_size + 7,
                                            face = "bold", color = "#1a1a1a",
                                            lineheight = 1.15, hjust = 0.5,
                                            margin = margin(t = 10, b = 6)),
      plot.subtitle      = element_textbox_simple(family = base_family, size = base_size + 0.5,
                                                  lineheight = 1.35, color = "#555555",
                                                  halign = 0.5, margin = margin(b = 14),
                                                  padding = margin(0, 0, 0, 0),
                                                  width = unit(1, "npc")),
      plot.caption       = element_markdown(family = base_family, size = base_size - 1.5,
                                            color = "#AAAAAA", lineheight = 1.3,
                                            hjust = 0, margin = margin(t = 14, b = 5)),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.margin           = margin(t = 20, r = 30, b = 12, l = 10),
      legend.position       = "none"
    )
}

theme_dark_custom <- function(base_size = 9, base_family = "Lato", title_family = "Outfit") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.background    = element_rect(fill = "#141414", color = NA),
      panel.background   = element_rect(fill = "#141414", color = NA),
      panel.grid.major.x = element_line(color = "#2A2A2A", linewidth = 0.3),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.title.x       = element_text(size = base_size - 0.5, color = "#777777",
                                        margin = margin(t = 8)),
      axis.title.y       = element_blank(),
      axis.text.y        = element_text(size = base_size - 0.5, color = "#D0D0D0",
                                        margin = margin(r = 4)),
      axis.text.x        = element_text(size = base_size - 1, color = "#666666"),
      axis.ticks         = element_blank(),
      plot.title         = element_markdown(family = title_family, size = base_size + 7,
                                            face = "bold", color = "#F0F0F0",
                                            lineheight = 1.15, hjust = 0.5,
                                            margin = margin(t = 10, b = 6)),
      plot.subtitle      = element_textbox_simple(family = base_family, size = base_size + 0.5,
                                                  lineheight = 1.35, color = "#999999",
                                                  fill = NA, box.color = NA,
                                                  halign = 0.5, margin = margin(b = 14),
                                                  padding = margin(0, 0, 0, 0),
                                                  width = unit(1, "npc")),
      plot.caption       = element_markdown(family = base_family, size = base_size - 1.5,
                                            color = "#555555", lineheight = 1.3,
                                            hjust = 0, margin = margin(t = 14, b = 5)),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.margin           = margin(t = 20, r = 30, b = 12, l = 10),
      legend.position       = "none"
    )
}

# --- Data ---
kaggle_2021 <- read_csv("R2/kaggle_survey_2021.csv")
kaggle_2021 <- kaggle_2021[-1, ]

kaggle_2021$Q2 <- kaggle_2021$Q2 %>%
  fct_recode(
    "Other" = "Nonbinary",
    "Other" = "Prefer not to say",
    "Other" = "Prefer to self-describe"
  )

kaggle_2021$Q3 <- kaggle_2021$Q3 %>%
  fct_recode(
    "Hong Kong" = "Hong Kong (S.A.R.)",
    "Other"     = "I do not wish to disclose my location",
    "Iran"      = "Iran, Islamic Republic of...",
    "UAE"       = "United Arab Emirates",
    "UK"        = "United Kingdom of Great Britain and Northern Ireland",
    "USA"       = "United States of America",
    "Vietnam"   = "Viet Nam"
  )

data <- kaggle_2021 %>%
  group_by(Q3) %>%
  summarise(n = n(),
            Women     = sum(factor(Q2) == "Woman"),
            pct_women = Women / n * 100,
            .groups   = "drop") %>%
  filter(Q3 != "Other", n >= 50)

data$iso2c                     <- tolower(countrycode(data$Q3, "country.name", "iso2c"))
data$greekVariantsCountryNames <- countrycode(data$Q3, "country.name", "cldr.variant.el")

# --- Pre-computed stats ---
avg_pct     <- round(mean(data$pct_women), 2)
greece_pct  <- round(data$pct_women[data$Q3 == "Greece"], 1)
greece_rank <- sum(data$pct_women > data$pct_women[data$Q3 == "Greece"]) + 1
greece_name_gr <- data$greekVariantsCountryNames[data$Q3 == "Greece"]

top_country_en    <- data$Q3[which.max(data$pct_women)]
bottom_country_en <- data$Q3[which.min(data$pct_women)]
top_country_gr    <- data$greekVariantsCountryNames[which.max(data$pct_women)]
bottom_country_gr <- data$greekVariantsCountryNames[which.min(data$pct_women)]
n_countries       <- nrow(data)

# --- Axis label colors per variant ---
data <- data %>%
  mutate(
    label_color      = ifelse(Q3 == "Greece", greece_color, "#333333"),
    label_color_dark = ifelse(Q3 == "Greece", "#5B9BD5",    "#D0D0D0")
  )

en_axis_light <- setNames(data$label_color,      data$Q3)
en_axis_dark  <- setNames(data$label_color_dark, data$Q3)
gr_axis_light <- setNames(data$label_color,      data$greekVariantsCountryNames)
gr_axis_dark  <- setNames(data$label_color_dark, data$greekVariantsCountryNames)

# --- make_plot ---
make_plot <- function(country_col, greece_name,
                      title_text, subtitle_text, caption_text, x_lab,
                      axis_colors, label_color, avg_label,
                      theme_fn, title_family = "Outfit", base_family = "Lato", base_size = 9) {

  sorted_levels <- data %>% arrange(pct_women) %>% pull(.data[[country_col]]) %>% as.character()
  axis_colors   <- axis_colors[sorted_levels]

  greece_data <- data %>% filter(Q3 == "Greece")

  greece_label <- data %>% filter(.data[[country_col]] == greece_name) %>%
    mutate(lbl = paste0("#", greece_rank, " · ", round(pct_women, 1), "%"))

  ggplot(data) +
    geom_col(
      aes(x = pct_women, y = reorder(.data[[country_col]], pct_women), fill = pct_women),
      width = 0.72
    ) +
    scale_fill_gradient(low = bar_low, high = bar_high) +
    geom_col(
      data = greece_data,
      aes(x = pct_women, y = reorder(.data[[country_col]], pct_women)),
      fill = greece_color, width = 0.72
    ) +
    geom_text(
      data = data %>% filter(Q3 != "Greece"),
      aes(x     = pct_women + 0.5,
          y     = reorder(.data[[country_col]], pct_women),
          label = paste0(round(pct_women, 1), "%")),
      hjust = 0, family = title_family, fontface = "bold", size = 2.4, color = label_color
    ) +
    geom_text(
      data = greece_label,
      aes(x     = pct_women + 0.5,
          y     = reorder(.data[[country_col]], pct_women),
          label = lbl),
      hjust = 0, family = title_family, fontface = "bold", size = 2.4, color = greece_color
    ) +
    ggflags::geom_flag(aes(x = 0.5, y = .data[[country_col]], country = iso2c), size = 3.5) +
    scale_x_continuous(
      limits = c(0, 46),
      breaks = seq(0, 40, by = 5),
      expand = expansion(mult = c(0, 0))
    ) +
    labs(title = title_text, subtitle = subtitle_text, caption = caption_text, x = x_lab) +
    coord_cartesian(clip = "off") +
    theme_fn(base_size = base_size, base_family = base_family, title_family = title_family) +
    theme(axis.text.y = element_text(color = axis_colors))
}

# --- English texts ---
en_title    <- "Women Participation in Data Science by Country"
en_subtitle <- glue(
  "Globally, fewer than 1 in 5 data scientists are women ",
  "(avg. <span style='color:{avg_color};'>**{avg_pct}%**</span>). ",
  "<span style='color:{greece_color};'>**Greece**</span> ranks {greece_rank}th among {n_countries} countries, ",
  "with {greece_pct}% female participation."
)
en_caption  <- "Only countries with 50 respondents or more shown. Data: Kaggle Survey 2021 | <span style='font-family:fb;'>&#xf09b;</span> stesiam, 2024"

# --- Greek texts ---
gr_title    <- "Συμμετοχή Γυναικών στην Επιστήμη Δεδομένων"
gr_subtitle <- glue(
  "Παγκοσμίως, λιγότεροι από 1 στους 5 επιστήμονες δεδομένων είναι γυναίκες ",
  "(μ.ο. <span style='color:{avg_color};'>**{avg_pct}%**</span>). ",
  "Η <span style='color:{greece_color};'>**Ελλάδα**</span> κατατάσσεται {greece_rank}η μεταξύ {n_countries} χωρών, ",
  "με {greece_pct}% γυναικεία συμμετοχή."
)
gr_caption  <- "Εμφανίζονται χώρες με 50 συμμετέχοντες και άνω. Δεδομένα: Kaggle Survey 2021 | <span style='font-family:fb;'>&#xf09b;</span> stesiam, 2024"

# --- Generate all 4 variants ---
variants <- list(
  list(lang = "en", theme = "light",
       country_col = "Q3",                       greece_name = "Greece",
       axis_colors = en_axis_light, label_color = "#555555", avg_label = "Avg",
       theme_fn = theme_light_custom, title_family = "Outfit",  base_family = "Lato"),
  list(lang = "en", theme = "dark",
       country_col = "Q3",                       greece_name = "Greece",
       axis_colors = en_axis_dark,  label_color = "#AAAAAA", avg_label = "Avg",
       theme_fn = theme_dark_custom, title_family = "Outfit",  base_family = "Lato"),
  list(lang = "el", theme = "light",
       country_col = "greekVariantsCountryNames", greece_name = greece_name_gr,
       axis_colors = gr_axis_light, label_color = "#555555", avg_label = "Μ.Ο.",
       theme_fn = theme_light_custom, title_family = "Inter",  base_family = "Inter"),
  list(lang = "el", theme = "dark",
       country_col = "greekVariantsCountryNames", greece_name = greece_name_gr,
       axis_colors = gr_axis_dark,  label_color = "#AAAAAA", avg_label = "Μ.Ο.",
       theme_fn = theme_dark_custom, title_family = "Inter",  base_family = "Inter")
)

for (v in variants) {
  title_text    <- if (v$lang == "en") en_title    else gr_title
  subtitle_text <- if (v$lang == "en") en_subtitle else gr_subtitle
  caption_text  <- if (v$lang == "en") en_caption  else gr_caption

  plt <- make_plot(
    country_col   = v$country_col,
    greece_name   = v$greece_name,
    title_text    = title_text,
    subtitle_text = subtitle_text,
    caption_text  = caption_text,
    x_lab         = if (v$lang == "en") "Women (%)" else "Γυναίκες (%)",
    axis_colors   = v$axis_colors,
    label_color   = v$label_color,
    avg_label     = v$avg_label,
    theme_fn      = v$theme_fn,
    title_family  = v$title_family,
    base_family   = v$base_family
  )

  ggsave(
    filename = glue("R2/Women-in-DS/r2a-kaggle-women-ds-{v$theme}-{v$lang}.png"),
    plot     = plt,
    device   = "png",
    dpi      = 300,
    width    = 7,
    height   = 10
  )
}
