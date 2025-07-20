library(dplyr)
library(tidyr)

library(showtext)
library(sysfonts)
library(hrbrthemes)
library(waffle)
library(ggplot2)
library(ggtext)
library(glue)

library(rJava)
library(tabulapdf)
library(pdftools)

font_add_google("Roboto Slab", family = "rs")
font_add_google("Lato", "Lato")

sysfonts::font_add('fb', '_extensions/quarto-ext/fontawesome/assets/webfonts/fa-brands-400.ttf')
sysfonts::font_add('fs', '_extensions/quarto-ext/fontawesome/assets/webfonts/fa-solid-900.ttf')

showtext_auto()
showtext::showtext_opts(dpi = 300)

theme_light_custom <- function(base_size = 10, base_family = "rs") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      text = element_text(color = "#222222"),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.title = element_markdown(size = base_size + 4, face = "bold", color = "#000000",
                                    hjust = 0.5, margin = margin(t=5, b=5)),
      plot.subtitle = element_textbox_simple(family = base_family, size = base_size + 0.5, lineheight = 1.1, halign = 0.5,
                                             color = "#444444",
                                             margin = margin(t = 5, b = 5)),
      plot.caption = element_markdown(color = "#444444", hjust = 0.5, lineheight = 1.1),
      legend.position = "right",
      legend.text = element_text(size = base_size-1,face =  "bold", margin = margin(t = -5, l = 5)),
    )
}

theme_dark_custom <- function(base_size = 10, base_family = "rs") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.background = element_rect(fill = "#181818", color = NA),
      panel.background = element_rect(fill = "#181818", color = NA),
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.title = element_markdown(size = base_size + 4, face = "bold", color = "white",
                                    hjust = 0.5, margin = margin(t=5, b=5)),
      plot.subtitle = element_textbox_simple(family = base_family, size = base_size + 0.5, lineheight = 1.1,
                                             color = "gray80", fill = NA, box.color = NA, halign = 0.5,
                                             margin = margin(t = 5, b = 5)
      ),
      plot.caption = element_markdown(color = "gray70", hjust = 0.5, lineheight = 1.1),
      legend.position = "right",
      legend.text = element_text(color = "gray70", size = base_size - 1,face =  "bold", margin = margin(t = -5, l = 5))
    )
}


# url = "https://www.unipi.gr/faculty/mbouts/anak/OS_22_23.pdf"
# 
#  download.file(url,
#                destfile = "R3/sg22.pdf",
#                method = "wget",
#                extra = "--no-check-certificate")
#  
#  pdf_subset('R3/sg22.pdf',
#             pages = 186:190,  output = "R3/subset.pdf")
# 
#  statistics_tables <- extract_tables(
#    file   = "R3/subset.pdf", 
#    method = "decide", 
#    output = "tibble")

B = statistics_tables[[2]] %>%
  setNames(c("Year", "BSc_students", "MScStudentsA", "MScStudentsB", "PhD")) %>%
  .[-1,] %>%
  mutate_at(., vars(-Year), as.integer) %>%
  mutate(
    MScStudentsB = tidyr::replace_na(MScStudentsB, 0),
    MSc_Students = MScStudentsA + MScStudentsB
  ) %>%
  dplyr::select(-c("MScStudentsA", "MScStudentsB")) %>%
  relocate(., MSc_Students, .after = "BSc_students") %>%
  mutate(
    total = BSc_students + MSc_Students + PhD,
    BSc_students = BSc_students/total,
    MSc_Students = MSc_Students/total,
    PhD = PhD/total
  ) %>%
  mutate_at(vars(!c(Year, total)), ~round(.*100, 2)) %>%
  dplyr::select(c(-total)) %>%
  tidyr::pivot_longer(., cols = !c(Year), values_to = "Obs")

title_text = glue("Students\\' Ratio by Degree")
subtitle_text = glue("**<span style = 'color:#a40000; font-weight: bold'>Undergraduates</span>** 
                     are about the 95% of the total student population of our department. **<span style = 'color:#c68958; font-weight: bold'>Postgraduates</span>** 
                     are the 5% and **<span style = 'color:purple; font-weight: bold'>PhD</span>** 
                     candidates are just 0.5% of total student population.")
caption_text = glue("<b> Data: </b> Study Guide of Statistics and Insurance Science <br><span style='font-family:fb;'  >&#xf09b;</span> <b>stesiam</b>, 2024")

final_plot = B %>%
  dplyr::filter(Year == "2021-2022") %>%
  ggplot(., aes(label=name, values=round(Obs))) +
  geom_pictogram(n_rows = 10,
                 flip = TRUE,
                 make_proportional = TRUE, 
                 family = "fs", size = 5, 
                 aes(color = name))  +
  scale_label_pictogram(
    name = NULL,
    values = c(
      "BSc_students" = "graduation-cap", 
      "MSc_Students" = "graduation-cap", 
      "PhD" = "graduation-cap"
    ),
    labels = c(
      BSc_students = "Bachelor students",
      MSc_Students = "Master students",
      PhD = "PhD students"
    )
  ) +
  scale_color_manual(
    name = NULL,
    values = c(
      BSc_students = "#a40000",
      MSc_Students = "#c68958", 
      PhD = "purple"
    ),
    labels = c(
      BSc_students = "Bachelor students",
      MSc_Students = "Master students",
      PhD = "PhD students"
    )
  )  +
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text
  )


final_plot_light = final_plot +
  theme_light_custom(base_family = "Lato") 

final_plot_dark = final_plot +
  theme_dark_custom(base_family = "Lato")

ggsave(
  filename = "R3/R3-Students/r3-en-light.png",
  plot = final_plot_light,
  device = "png",
  dpi = 300,
  width = 5,
  height = 5)

ggsave(
  filename = "R3/R3-Students/r3-en-dark.png",
  plot = final_plot_dark,
  device = "png",
  dpi = 300,
  width = 5,
  height = 5)

