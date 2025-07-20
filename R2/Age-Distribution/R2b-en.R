library(readr)
library(dplyr)
library(forcats)
library(ggplot2)
library(ggtext)
library(glue)
library(sysfonts)
library(showtext)

font_add_google("Roboto Slab", family = "rs")
font_add_google("Lato", family = "Lato")

sysfonts::font_add('fb', '/home/stelios/Downloads/fontawesome-free-6.7.2-desktop/otfs/Font Awesome 6 Brands-Regular-400.otf')
sysfonts::font_add('fs', '/home/stelios/Downloads/fontawesome-free-6.7.2-desktop/otfs/Font Awesome 6 Free-Solid-900.otf')

showtext_auto()
showtext::showtext_opts(dpi = 300)

theme_light_custom <- function(base_size = 10, base_family = "Lato") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid = element_blank(),
      
      text = element_text(color = "#222222"),
      axis.text = element_text(color = "#333333"),
      axis.title = element_text(color = "#111111"),
      axis.title.x = element_blank(),
      plot.title = element_markdown(size = base_size + 4, face = "bold", color = "#000000",
                                    hjust = 0.5, margin = margin(t=5, b=5)),
      plot.subtitle = element_textbox_simple(family = base_family, size = base_size + 0.5, lineheight = 1.1,
                                             color = "#444444", fill = NA, box.color = NA,
                                             margin = margin(t = 5, b = 5)),
      plot.caption = element_markdown(color = "#444444", lineheight = 1.2),
      legend.position = c(0.85, 0.85)
    )
}

theme_dark_custom <- function(base_size = 10, base_family = "Lato") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.background = element_rect(fill = "#181818", color = NA),
      panel.background = element_rect(fill = "#181818", color = NA),
      panel.grid = element_blank(),
      
      text = element_text(color = "white"),
      axis.text = element_text(color = "gray80"),
      axis.title = element_text(color = "gray90"),
      axis.title.x = element_blank(),
      
      plot.title = element_markdown(size = base_size + 4, face = "bold", color = "white",
                                    hjust = 0.5, margin = margin(t=5, b=5)),
      plot.subtitle = element_textbox_simple(family = base_family, size = base_size + 0.5, lineheight = 1.1,
                                             color = "gray80", fill = NA, box.color = NA,
                                             margin = margin(t = 5, b = 10)
      ),
      plot.caption = element_markdown(color = "gray70", lineheight = 1.2),
      legend.position = c(0.85, 0.85)
    )
}

kaggle_2021 = read_csv("R2/kaggle_survey_2021.csv")

# Delete second line
kaggle_2021 = kaggle_2021[-c(1),]

kaggle_2021_compare = kaggle_2021 %>%
  mutate(Q3 = if_else(Q3 != "Greece", "Other", Q3))



title_text = glue("<b>Age Distribution of Kaggle Community</b>")
subtitle_text = glue("Greek Kagglers have a proportionally lower presence at younger ages 
(<24 years) compared to the rest of the world and almost zero presence at older ages (>60 years). 
For the most part, Greek analysts on the  platform belong to the 25-29, 30-34 and 
45-49 age groups. On the other hand, the rest of Kaggle\\'s users have a tremendous 
superiority to the three smaller age groups (18-21, 22-24, 25-29).")
caption_text = glue("<b> Sources: </b> Kaggle Survey 2021<br><b>Visualization:</b> <span style='font-family:fb;'  >&#xf09b;</span> stesiam, 2023")



final_plot = kaggle_2021_compare %>%
  select(Q3, Q1) %>%
  group_by(Q3,Q1) %>%
  summarise(n = n()) %>%
  group_by(Q3) %>%
  mutate(total = sum(n),
         pct = round(n/total *100, digits = 1)) %>%
  select(Q3, Q1, pct) %>%
  ggplot(aes(x = Q1, y = pct+1, fill = Q3)) +
  geom_bar(stat = "identity", alpha = 0.5, position = "dodge") +
  scale_fill_manual(values = c("Greece" = "#1f77b4", "Other" = "#ff7f0e"))+ 
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    x = "Age Group",
    y = "Percentage (%)",
    fill = ""
  )

final_plot_light = final_plot +
  theme_light_custom(base_family = "Lato", base_size = 9) 

final_plot_dark = final_plot +
  theme_dark_custom(base_family = "Lato", base_size = 9)

ggsave(
  filename = "R2/Age-Distribution/r2a-kaggle-women-ds-en-light.png",
  plot = final_plot_light,
  device = "png",
  dpi = 300,
  width = 6,
  height = 4)

ggsave(
  filename = "R2/Age-Distribution/r2a-kaggle-women-ds-en-dark.png",
  plot = final_plot_dark,
  device = "png",
  dpi = 300,
  width = 6,
  height = 4)

