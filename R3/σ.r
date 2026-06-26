library(dplyr)
library(tidyr)

library(showtext)
library(sysfonts)
library(waffle)
library(ggplot2)
library(ggtext)
library(glue)

library(rJava)
library(tabulapdf)
library(pdftools)

# ── Typography ────────────────────────────────────────────────────────────────
font_add_google("Outfit", "Outfit")
font_add_google("Lato", "Lato")

# Font Awesome Solid — needed by geom_pictogram
# Update this path to wherever fa-solid-900.ttf lives on your system
# Find it with: find / -name "fa-solid-900.ttf" 2>/dev/null
sysfonts::font_add(
  'fs',
  '_extensions/quarto-ext/fontawesome/assets/webfonts/fa-solid-900.ttf'
)

showtext_auto()
showtext::showtext_opts(dpi = 300)

# ── Color palette (desaturated, editorial) ────────────────────────────────────
bsc_color <- "#B8432F"
msc_color <- "#C68958"
phd_color <- "#7B4FA0"

# ── Themes ────────────────────────────────────────────────────────────────────
theme_light_custom <- function(base_size = 10, base_family = "Lato") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.background = element_rect(fill = "#FAFAFA", color = NA),
      panel.background = element_rect(fill = "#FAFAFA", color = NA),
      panel.grid = element_blank(),

      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),

      plot.title.position = "plot",
      plot.caption.position = "plot",

      plot.title = element_markdown(
        family = "Outfit",
        size = base_size + 6,
        face = "bold",
        color = "#1a1a1a",
        hjust = 0.5,
        margin = margin(t = 10, b = 6)
      ),
      plot.subtitle = element_textbox_simple(
        family = base_family,
        size = base_size,
        lineheight = 1.35,
        color = "#555555",
        halign = 0.5,
        margin = margin(t = 2, b = 10),
        padding = margin(0, 0, 0, 0),
        width = unit(0.88, "npc")
      ),
      plot.caption = element_text(
        family = base_family,
        size = base_size - 2,
        color = "#AAAAAA",
        hjust = 0.5,
        margin = margin(t = 10, b = 5)
      ),

      plot.margin = margin(t = 15, r = 15, b = 10, l = 15),

      legend.position = "right",
      legend.text = element_text(
        size = base_size - 1,
        color = "#444444",
        margin = margin(t = -4, l = 5)
      )
    )
}

theme_dark_custom <- function(base_size = 10, base_family = "Lato") {
  bg <- "#141414"

  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.background = element_rect(fill = bg, color = NA),
      panel.background = element_rect(fill = bg, color = NA),
      panel.grid = element_blank(),

      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),

      plot.title.position = "plot",
      plot.caption.position = "plot",

      plot.title = element_markdown(
        family = "Outfit",
        size = base_size + 6,
        face = "bold",
        color = "#F0F0F0",
        hjust = 0.5,
        margin = margin(t = 10, b = 6)
      ),
      plot.subtitle = element_textbox_simple(
        family = base_family,
        size = base_size,
        lineheight = 1.35,
        color = "#999999",
        fill = NA,
        box.color = NA,
        halign = 0.5,
        margin = margin(t = 2, b = 10),
        padding = margin(0, 0, 0, 0),
        width = unit(0.88, "npc")
      ),
      plot.caption = element_text(
        family = base_family,
        size = base_size - 2,
        color = "#555555",
        hjust = 0.5,
        margin = margin(t = 10, b = 5)
      ),

      plot.margin = margin(t = 15, r = 15, b = 10, l = 15),

      legend.position = "right",
      legend.text = element_text(
        color = "#AAAAAA",
        size = base_size - 1,
        margin = margin(t = -4, l = 5)
      )
    )
}

# ── Data prep ─────────────────────────────────────────────────────────────────
# url = "https://www.unipi.gr/faculty/mbouts/anak/OS_22_23.pdf"
#
# download.file(url,
#               destfile = "R3/sg22.pdf",
#               method = "wget",
#               extra = "--no-check-certificate")
#
# pdf_subset('R3/sg22.pdf',
#            pages = 186:190, output = "R3/subset.pdf")
#
# statistics_tables <- extract_tables(
#   file   = "R3/subset.pdf",
#   method = "decide",
#   output = "tibble")

B <- statistics_tables[[2]] %>%
  setNames(c("Year", "BSc_students", "MScStudentsA", "MScStudentsB", "PhD")) %>%
  .[-1, ] %>%
  mutate(across(-Year, as.integer)) %>%
  mutate(
    MScStudentsB = replace_na(MScStudentsB, 0),
    MSc_Students = MScStudentsA + MScStudentsB
  ) %>%
  select(-c(MScStudentsA, MScStudentsB)) %>%
  relocate(MSc_Students, .after = BSc_students) %>%
  mutate(
    total = BSc_students + MSc_Students + PhD,
    BSc_students = BSc_students / total,
    MSc_Students = MSc_Students / total,
    PhD = PhD / total
  ) %>%
  mutate(across(!c(Year, total), ~ round(. * 100, 2))) %>%
  select(-total) %>%
  pivot_longer(cols = -Year, values_to = "Obs")

# ── Text elements ─────────────────────────────────────────────────────────────
title_text <- "Students' Ratio by Degree"

subtitle_text <- glue(
  "<span style='color:{bsc_color};'>**Undergraduates**</span> make up ~95% of the department. ",
  "<span style='color:{msc_color};'>**Postgraduates**</span> account for ~5%, ",
  "and <span style='color:{phd_color};'>**PhD**</span> candidates just 0.5%."
)

caption_text <- "Data: Study Guide of Statistics and Insurance Science | GitHub: stesiam, 2024"

# ── Build plot ────────────────────────────────────────────────────────────────
final_plot <- B %>%
  filter(Year == "2021-2022") %>%
  ggplot(aes(label = name, values = round(Obs))) +
  geom_pictogram(
    n_rows = 10,
    flip = TRUE,
    make_proportional = TRUE,
    family = "fs",
    size = 5,
    aes(color = name)
  ) +
  scale_label_pictogram(
    name = NULL,
    values = c(
      "BSc_students" = "graduation-cap",
      "MSc_Students" = "graduation-cap",
      "PhD" = "graduation-cap"
    ),
    labels = c(
      BSc_students = "Bachelor",
      MSc_Students = "Master",
      PhD = "PhD"
    )
  ) +
  scale_color_manual(
    name = NULL,
    values = c(
      BSc_students = bsc_color,
      MSc_Students = msc_color,
      PhD = phd_color
    ),
    labels = c(
      BSc_students = "Bachelor",
      MSc_Students = "Master",
      PhD = "PhD"
    )
  ) +
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text
  )

# ── Light version ─────────────────────────────────────────────────────────────
final_plot_light <- final_plot +
  theme_light_custom(base_family = "Lato")

# ── Dark version ──────────────────────────────────────────────────────────────
final_plot_dark <- final_plot +
  theme_dark_custom(base_family = "Lato")

# ── Save ──────────────────────────────────────────────────────────────────────
ggsave(
  filename = "R3/R3-Students/r3-en-light.png",
  plot = final_plot_light,
  device = "png",
  dpi = 300,
  width = 5,
  height = 5
)

ggsave(
  filename = "R3/R3-Students/r3-en-dark.png",
  plot = final_plot_dark,
  device = "png",
  dpi = 300,
  width = 5,
  height = 5
)
