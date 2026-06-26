library(readr)
library(dplyr)
library(stringr)
library(stringi)
library(glue)
library(showtext)
library(sysfonts)
library(ggplot2)
library(ggtext)
library(ggimage)

# ── Typography ────────────────────────────────────────────────────────────────
font_add_google("Outfit", "Outfit")
font_add_google("Inter",  "Inter")
font_add_google("Lato",   "Lato")
sysfonts::font_add('fb', '/home/stelios/Downloads/fontawesome-free-6.7.2-desktop/otfs/Font Awesome 6 Brands-Regular-400.otf')
showtext_auto()
showtext::showtext_opts(dpi = 300)

# ── Party logos (PNG thumbnails from Wikipedia) ───────────────────────────────
party_logos <- c(
  "ND"     = "https://upload.wikimedia.org/wikipedia/commons/thumb/d/df/New_Democracy_Logo_2018.svg/120px-New_Democracy_Logo_2018.svg.png",
  "PASOK"  = "https://upload.wikimedia.org/wikipedia/commons/thumb/b/b9/Panellinio_Sosialistiko_Kinima_Logo.svg/120px-Panellinio_Sosialistiko_Kinima_Logo.svg.png",
  "KKE"    = "https://upload.wikimedia.org/wikipedia/commons/thumb/b/b6/Kke_sima.svg/120px-Kke_sima.svg.png",
  "SYRIZA" = "https://upload.wikimedia.org/wikipedia/commons/thumb/b/b4/SYRIZA_logo_2020.svg/120px-SYRIZA_logo_2020.svg.png"
)

# ── Colors ────────────────────────────────────────────────────────────────────
party_colors_light <- c(
  "KKE"    = "#B8544E",
  "ND"     = "#1A6FA0",
  "PASOK"  = "#587D42",
  "SYRIZA" = "#A65B87"
)
party_colors_dark <- c(
  "KKE"    = "#E8857F",
  "ND"     = "#4BA3D4",
  "PASOK"  = "#9FD47E",
  "SYRIZA" = "#DA93BC"
)

# ── Themes ────────────────────────────────────────────────────────────────────
theme_light_custom <- function(base_size = 9, base_family = "Lato", title_family = "Outfit") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.background    = element_rect(fill = "#FAFAFA", color = NA),
      panel.background   = element_rect(fill = "#FAFAFA", color = NA),
      panel.grid.major.x = element_line(color = "#E5E5E5", linewidth = 0.3),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.title.y       = element_blank(),
      axis.title.x       = element_text(size = base_size - 0.5, color = "#737373",
                                        margin = margin(t = 8)),
      axis.text.y        = element_text(size = base_size - 0.5, color = "#333333",
                                        margin = margin(r = 4)),
      axis.text.x        = element_text(size = base_size - 1, color = "#737373"),
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
                                            color = "#737373", lineheight = 1.3,
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
      axis.title.y       = element_blank(),
      axis.title.x       = element_text(size = base_size - 0.5, color = "#808080",
                                        margin = margin(t = 8)),
      axis.text.y        = element_text(size = base_size - 0.5, color = "#D0D0D0",
                                        margin = margin(r = 4)),
      axis.text.x        = element_text(size = base_size - 1, color = "#808080"),
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
                                            color = "#808080", lineheight = 1.3,
                                            hjust = 0, margin = margin(t = 14, b = 5)),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.margin           = margin(t = 20, r = 30, b = 12, l = 10),
      legend.position       = "none"
    )
}

# ── Party recode (EL party names → abbreviations) ─────────────────────────────
recode_party_el <- function(party) {
  case_when(
    party == "ΝΕΑ ΔΗΜΟΚΡΑΤΙΑ"                                                                              ~ "ND",
    party %in% c(
      "ΠΑ.ΣΟ.Κ.",
      "ΠΑΣΟΚ-ΚΙΝΗΜΑ ΑΛΛΑΓΗΣ",
      "ΚΙΝΗΜΑ ΑΛΛΑΓΗΣ",
      "ΔΗΜΟΚΡΑΤΙΚΗ ΣΥΜΠΑΡΑΤΑΞΗ (ΠΑΝΕΛΛΗΝΙΟ ΣΟΣΙΑΛΙΣΤΙΚΟ ΚΙΝΗΜΑ - ΔΗΜΟΚΡΑΤΙΚΗ ΑΡΙΣΤΕΡΑ)"
    )                                                                                                      ~ "PASOK",
    party == "ΚΟΜΜΟΥΝΙΣΤΙΚΟ ΚΟΜΜΑ ΕΛΛΑΔΑΣ"                                                                ~ "KKE",
    party %in% c(
      "ΣΥΝΑΣΠΙΣΜΟΣ ΡΙΖΟΣΠΑΣΤΙΚΗΣ ΑΡΙΣΤΕΡΑΣ",
      "ΣΥΝΑΣΠΙΣΜΟΣ ΡΙΖΟΣΠΑΣΤΙΚΗΣ ΑΡΙΣΤΕΡΑΣ - ΠΡΟΟΔΕΥΤΙΚΗ  ΣΥΜΜΑΧΙΑ",
      "ΣΥΝΑΣΠΙΣΜΟΣ"
    )                                                                                                      ~ "SYRIZA",
    party %in% c(
      "ΑΝΕΞΑΡΤΗΤΟΙ ΕΛΛΗΝΕΣ - ΠΑΝΟΣ ΚΑΜΜΕΝΟΣ",
      "ΑΝΕΞΑΡΤΗΤΟΙ ΕΛΛΗΝΕΣ ΕΘΝΙΚΗ ΠΑΤΡΙΩΤΙΚΗ ΔΗΜΟΚΡΑΤΙΚΗ ΣΥΜΜΑΧΙΑ"
    )                                                                                                      ~ "ANEL",
    party == "ΛΑΪΚΟΣ ΣΥΝΔΕΣΜΟΣ - ΧΡΥΣΗ ΑΥΓΗ"                                                             ~ "XA",
    party == "ΛΑΪΚΗ ΕΝΟΤΗΤΑ"                                                                              ~ "LAE",
    party == "ΛΑ.Ο.Σ."                                                                                    ~ "LAOS",
    party == "ΔΗΜΟΚΡΑΤΙΚΗ ΑΡΙΣΤΕΡΑ"                                                                       ~ "DHMAR",
    party == "ΑΝΕΞΑΡΤΗΤΟΙ ΔΗΜΟΚΡΑΤΙΚΟΙ ΒΟΥΛΕΥΤΕΣ"                                                        ~ "ADP",
    party == "ΕΛΛΗΝΙΚΗ ΛΥΣΗ - ΚΥΡΙΑΚΟΣ ΒΕΛΟΠΟΥΛΟΣ"                                                       ~ "EL",
    party == "ΜέΡΑ25"                                                                                     ~ "MERA25",
    party == "ΕΝΙΑΙΑ ΔΗΜΟΚΡΑΤΙΚΗ ΑΡΙΣΤΕΡΑ- Ε.Δ.Α."                                                       ~ "EDA",
    party == "ΕΔΗΚ"                                                                                        ~ "EDIK",
    party == "ΕΘΝΙΚΗ ΠΑΡΑΤΑΞΙΣ"                                                                            ~ "ETHNIKI",
    party == "ΝΕΟΦΙΛΕΛΕΥΘΕΡΩΝ"                                                                             ~ "NEO",
    TRUE ~ party
  )
}

# ── Romanization (el-Latn → Latin-ASCII gives natural modern Greek spellings) ──
romanize <- function(x) {
  x |>
    stri_trans_general("el-Latn") |>
    stri_trans_general("Latin-ASCII")
}

# ── Label helpers ─────────────────────────────────────────────────────────────
make_label_vec <- function(x) {
  x       <- str_squish(str_replace_all(x, "\\(.*?\\)", ""))
  has_hyp <- str_detect(x, "^\\S+ - \\S+")
  surname <- if_else(has_hyp, str_extract(x, "^\\S+ - \\S+"),  str_extract(x, "^\\S+"))
  rest    <- if_else(has_hyp, str_replace(x, "^\\S+ - \\S+\\s+", ""),
                              str_replace(x, "^\\S+\\s+", ""))
  initial <- str_sub(str_extract(rest, "^\\S+"), 1, 1)
  paste(str_squish(surname), initial)
}

make_label_vec_long <- function(x) {
  x       <- str_squish(str_replace_all(x, "\\(.*?\\)", ""))
  has_hyp <- str_detect(x, "^\\S+ - \\S+")
  surname <- if_else(has_hyp, str_extract(x, "^\\S+ - \\S+"),  str_extract(x, "^\\S+"))
  rest    <- if_else(has_hyp, str_replace(x, "^\\S+ - \\S+\\s+", ""),
                              str_replace(x, "^\\S+\\s+", ""))
  prefix  <- str_sub(str_extract(rest, "^\\S+"), 1, 3)
  paste(str_squish(surname), prefix)
}

# ── Data (EL CSV is the single authoritative source) ─────────────────────────
url_el <- "https://github.com/stesiam/scrape-greek-parl-members-26/releases/download/v1.1.0/parlMembers-el.csv"

parl_el <- read_csv(url_el, show_col_types = FALSE) %>%
  filter(period_name != "ΙΘ΄")

# ── Build freqs ───────────────────────────────────────────────────────────────
freqs <- parl_el %>%
  mutate(party = recode_party_el(party)) %>%
  group_by(name) %>%
  summarise(n = n(), party = first(party), .groups = "drop") %>%
  mutate(
    name_roman = romanize(name),
    label_el   = make_label_vec(name),
    label_en   = make_label_vec(name_roman)
  ) %>%
  filter(n >= 9) %>%
  arrange(n, label_el)

# Add party logo (NA for minor parties — geom_image skips them)
freqs <- freqs %>% mutate(logo = party_logos[party])

# Resolve EL label collisions
dup_el <- freqs %>% count(label_el) %>% filter(n > 1) %>% pull(label_el)
freqs  <- freqs %>%
  mutate(label_el = if_else(label_el %in% dup_el, make_label_vec_long(name),        label_el))

# Resolve EN label collisions
dup_en <- freqs %>% count(label_en) %>% filter(n > 1) %>% pull(label_en)
freqs  <- freqs %>%
  mutate(label_en = if_else(label_en %in% dup_en, make_label_vec_long(name_roman), label_en))

# ── Axis-label colors ─────────────────────────────────────────────────────────
mp_colors_light <- case_when(
  freqs$party == "ND"     ~ party_colors_light[["ND"]],
  freqs$party == "PASOK"  ~ party_colors_light[["PASOK"]],
  freqs$party == "KKE"    ~ party_colors_light[["KKE"]],
  freqs$party == "SYRIZA" ~ party_colors_light[["SYRIZA"]],
  TRUE ~ "#737373"
)
mp_colors_dark <- case_when(
  freqs$party == "ND"     ~ party_colors_dark[["ND"]],
  freqs$party == "PASOK"  ~ party_colors_dark[["PASOK"]],
  freqs$party == "KKE"    ~ party_colors_dark[["KKE"]],
  freqs$party == "SYRIZA" ~ party_colors_dark[["SYRIZA"]],
  TRUE ~ "#AAAAAA"
)

axis_colors_light_en <- setNames(mp_colors_light, freqs$label_en)
axis_colors_light_el <- setNames(mp_colors_light, freqs$label_el)
axis_colors_dark_en  <- setNames(mp_colors_dark,  freqs$label_en)
axis_colors_dark_el  <- setNames(mp_colors_dark,  freqs$label_el)

# ── Subtitle stats ────────────────────────────────────────────────────────────
n_nd     <- sum(freqs$party == "ND")
n_pasok  <- sum(freqs$party == "PASOK")
n_kke    <- sum(freqs$party == "KKE")
n_syriza <- sum(freqs$party == "SYRIZA")

# ── Texts ─────────────────────────────────────────────────────────────────────
en_title    <- "Most Elected MPs in Greek Parliament"
en_subtitle <- glue(
  "Members of Parliament elected **9 or more times** since 1974 (1st–18th Parliament). ",
  "The {n_nd + n_pasok} MPs from ",
  "<span style='color:{party_colors_light[\"ND\"]};'>**New Democracy**</span> and ",
  "<span style='color:{party_colors_light[\"PASOK\"]};'>**PASOK**</span> dominate the list ",
  "alongside {n_kke} from ",
  "<span style='color:{party_colors_light[\"KKE\"]};'>**KKE**</span> and ",
  "{n_syriza} from ",
  "<span style='color:{party_colors_light[\"SYRIZA\"]};'>**SYRIZA**</span>."
)
en_caption  <- glue(
  "**Data:** Hellenic Parliament (hellenicparliament.gr) | ",
  "<span style='font-family:fb;'>&#xf09b;</span> stesiam, 2025"
)

gr_title    <- "Οι πιο πολυεκλεγμένοι βουλευτές"
gr_subtitle <- glue(
  "Βουλευτές που εκλέχτηκαν **9 φορές ή περισσότερο** από το 1974 (Α΄–ΙΗ΄ Βουλή). ",
  "Οι {n_nd + n_pasok} βουλευτές της ",
  "<span style='color:{party_colors_light[\"ND\"]};'>**Νέας Δημοκρατίας**</span> και του ",
  "<span style='color:{party_colors_light[\"PASOK\"]};'>**ΠΑΣΟΚ**</span> κυριαρχούν στη λίστα, ",
  "μαζί με {n_kke} από το ",
  "<span style='color:{party_colors_light[\"KKE\"]};'>**ΚΚΕ**</span> και ",
  "{n_syriza} από τον ",
  "<span style='color:{party_colors_light[\"SYRIZA\"]};'>**ΣΥΡΙΖΑ**</span>."
)
gr_caption  <- glue(
  "**Δεδομένα:** Ελληνικό Κοινοβούλιο (hellenicparliament.gr) | ",
  "<span style='font-family:fb;'>&#xf09b;</span> stesiam, 2025"
)

# ── make_plot ─────────────────────────────────────────────────────────────────
make_plot <- function(label_col,
                      title_text, subtitle_text, caption_text, y_lab,
                      party_colors, axis_colors, label_color,
                      theme_fn, title_family = "Outfit", base_family = "Lato",
                      base_size = 9) {

  sorted_levels <- freqs %>% arrange(n) %>% pull(.data[[label_col]])
  ac            <- axis_colors[sorted_levels]

  ggplot(freqs, aes(x = factor(.data[[label_col]], levels = sorted_levels), y = n, fill = party)) +
    geom_col(width = 0.68, color = NA) +
    geom_image(aes(image = logo, y = 0), size = 0.018, nudge_y = -0.5, na.rm = TRUE) +
    geom_text(
      aes(label = n, y = n + 0.25),
      hjust = 0, size = 2.5,
      family = title_family, fontface = "bold", color = label_color
    ) +
    scale_fill_manual(values = party_colors) +
    scale_y_continuous(
      limits = c(0, 17), breaks = seq(0, 15, by = 3),
      expand = expansion(mult = c(0.06, 0.02))
    ) +
    labs(title = title_text, subtitle = subtitle_text, caption = caption_text, y = y_lab) +
    coord_flip(clip = "off") +
    theme_fn(base_size = base_size, base_family = base_family, title_family = title_family) +
    theme(axis.text.y = element_text(color = ac))
}

# ── Variants ──────────────────────────────────────────────────────────────────
plot_height <- max(10, round(nrow(freqs) * 0.2))

variants <- list(
  list(lang = "en", theme = "light",
       label_col    = "label_en",
       y_lab        = "Times elected",
       party_colors = party_colors_light, axis_colors = axis_colors_light_en,
       label_color  = "#555555",
       theme_fn     = theme_light_custom, title_family = "Outfit", base_family = "Lato"),
  list(lang = "en", theme = "dark",
       label_col    = "label_en",
       y_lab        = "Times elected",
       party_colors = party_colors_dark,  axis_colors = axis_colors_dark_en,
       label_color  = "#AAAAAA",
       theme_fn     = theme_dark_custom,  title_family = "Outfit", base_family = "Lato"),
  list(lang = "el", theme = "light",
       label_col    = "label_el",
       y_lab        = "Αριθμός εκλογών",
       party_colors = party_colors_light, axis_colors = axis_colors_light_el,
       label_color  = "#555555",
       theme_fn     = theme_light_custom, title_family = "Inter",  base_family = "Inter"),
  list(lang = "el", theme = "dark",
       label_col    = "label_el",
       y_lab        = "Αριθμός εκλογών",
       party_colors = party_colors_dark,  axis_colors = axis_colors_dark_el,
       label_color  = "#AAAAAA",
       theme_fn     = theme_dark_custom,  title_family = "Inter",  base_family = "Inter")
)

for (v in variants) {
  title_text    <- if (v$lang == "en") en_title    else gr_title
  subtitle_text <- if (v$lang == "en") en_subtitle else gr_subtitle
  caption_text  <- if (v$lang == "en") en_caption  else gr_caption

  plt <- make_plot(
    label_col     = v$label_col,
    title_text    = title_text,
    subtitle_text = subtitle_text,
    caption_text  = caption_text,
    y_lab         = v$y_lab,
    party_colors  = v$party_colors,
    axis_colors   = v$axis_colors,
    label_color   = v$label_color,
    theme_fn      = v$theme_fn,
    title_family  = v$title_family,
    base_family   = v$base_family
  )

  ggsave(
    filename = glue("R4/r4-{v$theme}-{v$lang}.png"),
    plot     = plt,
    device   = "png",
    dpi      = 300,
    width    = 7,
    height   = plot_height
  )
}
