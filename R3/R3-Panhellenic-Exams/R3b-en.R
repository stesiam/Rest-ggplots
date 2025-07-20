library(dplyr)
library(tidyr)

library(showtext)
library(sysfonts)
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


theme_light_custom <- function(base_size = 8, base_family = "Lato") {
  theme_void(base_size = base_size, base_family = base_family) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid = element_blank(),
      
      text = element_text(color = "#222222"),
      axis.title.x = element_blank(),
      plot.title = element_markdown(family = base_family, size = base_size + 4, face = "bold", color = "#000000",
                                    hjust = 0.5, margin = margin(t=5, b = 5)),
      plot.subtitle = element_textbox_simple(family = base_family, size = base_size + 0.5, lineheight = 1.1,
                                             color = "#444444", margin = margin(b = 10),
                                             padding = margin(5,5,5,5)),
      plot.caption = element_markdown(color = "#444444", lineheight = 1.2, margin = margin(b = 3, r = 5)),
      legend.position = "none"
    )
}

theme_dark_custom <- function(base_size = 8, base_family = "Lato") {
  theme_void(base_size = base_size, base_family = base_family) +
    theme(
      plot.background = element_rect(fill = "#181818", color = NA),
      panel.background = element_rect(fill = "#181818", color = NA),
      panel.grid = element_blank(),
      
      text = element_text(color = "white"),
      axis.title.x = element_blank(),
      
      plot.title = element_markdown(family = base_family, size = base_size + 4, face = "bold", color = "white",
                                    hjust = 0.5, margin = margin(t=5, b = 5)),
      plot.subtitle = element_textbox_simple(family = base_family, size = base_size + 0.5, lineheight = 1.1,
                                             color = "gray80", fill = NA, box.color = NA, margin = margin(b = 10),
                                             padding = margin(5,5,5,5)),
      plot.caption = element_markdown(color = "gray70", lineheight = 1.2, margin = margin(b = 3, r = 5)),
      legend.position = "none"
    )
}

url = "https://www.unipi.gr/faculty/mbouts/anak/OS_22_23.pdf"
 
# download.file(url,
#                 destfile = "R3/sg22.pdf",
#                 method = "wget",
#                 extra = "--no-check-certificate")
#   
# pdf_subset('R3/sg22.pdf',
#              pages = 186:190,  output = "R3/subset.pdf")
#  
# statistics_tables <- extract_tables(
#    file   = "R3/subset.pdf", 
#     method = "decide", 
#     output = "tibble")

admitted_students = statistics_tables[[1]] %>%
  .[-1,] %>%
  setNames(c("Year", "Main_exams", "Transfer", "Entry_exams", "Other", "Total")) %>%
  mutate(across(c(Main_exams, Transfer, Entry_exams, Other, Total), ~ as.integer(.))) %>%
  drop_na() %>%
  dplyr::filter(Transfer >=0) %>%
  select(Year, Main_exams, Total) %>%
  mutate(MainExamPct = round((Main_exams / Total)*100, digits = 0),
         OtherPct = 100 - MainExamPct) %>%
  select(Year, MainExamPct, OtherPct) %>%
  pivot_longer(
    cols = !Year, 
    names_to = "Admission_Type", 
    values_to = "count"
  )

admitted_students1 =
  admitted_students %>%
  group_by(Year) %>%
  mutate(lab.ypos = cumsum(count) - 0.5*count)

title_text = glue("**Proportion of Admissions through Panhellenic Exams**")
subtitle_text = glue("It seems that <span style = 'color:#1f77b4; font-weight: bold'>Panhellenic Exams</span> is the prevalent way to be admitted to Statistics Department. Although that was an expected result. It is interesting to study the proportion over the years. The most extraordinary result is in 2011 when almost everyone came through Panhellenic Exams.")
caption_text = glue("<b> Data: </b> Study Guide of Statistics and Insurance Science <br><span style='font-family:fb;'  >&#xf09b;</span> <b>stesiam</b>, 2024")



final_plot = ggplot(data = admitted_students1, aes(x = "", y = count, fill = Admission_Type)) +
  geom_bar(stat="identity", width=1) +
  geom_richtext(aes(y = lab.ypos, label = ifelse(Admission_Type == "MainExamPct", paste0(round(count), "%"), "")), color = "white", fontface = "bold",
                fill = NA, label.color = NA, family = "Lato")+
  scale_fill_manual(values = c("MainExamPct" = "#1f77b4", "OtherPct" = "#ff7f0e"))+ 
  facet_wrap(~Year, nrow = 3) +
  coord_polar("y", start=0) +
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text
  )

final_plot_light = final_plot +
  theme_light_custom(base_size = 9)

final_plot_dark = final_plot +
  theme_dark_custom(base_size = 9)

ggsave(
  filename = "R3/R3-Panhellenic-Exams/r3b-en-light.png",
  plot = final_plot_light,
  device = "png",
  height = 4.5,
  width = 6)

ggsave(
  filename = "R3/R3-Panhellenic-Exams/r3b-en-dark.png",
  plot = final_plot_dark,
  device = "png",
  height = 4.5,
  width = 6)

