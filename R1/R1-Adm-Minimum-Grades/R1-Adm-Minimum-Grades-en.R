library(dplyr)
library(ggplot2)
library(ggtext)
library(geomtextpath)
library(glue)
library(sysfonts)
library(showtext)

font_add_google("Roboto Slab", family = "clim")
font_add_google("Raleway", family = "mont")
font_add_google("Lato", family = "Lato")

sysfonts::font_add_google("Gentium Book Basic", "gp")


# for Quarto website
# sysfonts::font_add('fb', '_extensions/quarto-ext/fontawesome/assets/webfonts/fa-brands-400.ttf')
# sysfonts::font_add('fs', '_extensions/quarto-ext/fontawesome/assets/webfonts/fa-solid-900.ttf')

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
                                    hjust = 0.5, margin = margin(t=5)),
      plot.subtitle = element_textbox_simple(family ="mont", size = base_size + 0.5, lineheight = 1.1,
                                             color = "#444444",
                                             margin = margin(t = 5, b = 5)),
      plot.caption = element_markdown(color = "#444444"),
      legend.position = "none"
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
                                    hjust = 0.5, margin = margin(t=5)),
      plot.subtitle = element_textbox_simple(family = "mont", size = base_size + 0.5, lineheight = 1.1,
        color = "gray80", fill = NA, box.color = NA,
        margin = margin(t = 5, b = 5)
      ),
      plot.caption = element_markdown(color = "gray70"),
      legend.position = "none"
    )
}

tmimata_statistikis = data.frame(
  "Year" = rep(c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022),4),
  "University" = c(rep("AUEB",10), rep("UniPi",10), rep("Aegean",10), rep("UoWM",10)),
  "UniversityGR" = c(rep("ΟΠΑ",10), rep("ΠαΠει",10), rep("Αιγαίου",10), rep("ΠαΔΜ",10)),
  "Theseis" = c(99, 112, 103, 99, 103, 104, 113, 102, 103, 101,
                189, 202, 198, 190, 198, 198, 216, 194, 154, 152, 
                144, 137, 163, 156, 162, 178, 198, 171, 179, 165,
                NA, NA, NA, NA, NA, NA, 268, 225, 316, 256),
  "Vasi" = c(12788, 13277, 13488, 13158, 13511, 13354, 13337, 13425, 13316, 13828,
             12666, 13270, 13795, 12703, 12543, 11967, 11710, 12100, 11673, 12460,
             9618, 10636, 10574, 10471, 10692, 8525, 5148, 3925, 7797, 8425,
             NA, NA, NA, NA, NA, NA, 4846, 3950, 7867, 7740)
)

title_text = glue("<b>Minimum Admission Scores by Department</b>")
subtitle_text = glue("The admission base is the lowest score that a student must achieve in order to be admitted to a particular department. Over time, the statistics 
                     departments with the highest base are the **Athens University of Economics and Business** 
                     (<img  src='https://upload.wikimedia.org/wikipedia/el/2/2c/AUEBEMBLEM.png' height='8'>  AUEB) and the **University of Piraeus**
                     ( <img  src='https://upload.wikimedia.org/wikipedia/en/7/7e/UNIPI.jpg' height='8'> UniPi). Finally, we note that all the schools of Statistics have a relatively low admission base.")
caption_text = glue("<b> Sources:</b> [1] Ministry of Education, Religious Affairs and Sports, [2] aeitei.gr<br><b>Graph:</b> <span style='font-family:fb;'  >&#xf09b;</span> stesiam, 2024")

final_plot_light = ggplot2::ggplot(data = tmimata_statistikis, aes(x = Year, y = Vasi/1000, 
                                                             color= University, 
                                                             label = University),
                             group = University) +
  geom_point(data = tmimata_statistikis |> dplyr::filter(Year == 2022),
             size = 9) +
  geom_textline(size = 3, vjust = -0.1,
                linewidth = 1.5, fontface = "bold", hjust = 0.5, family = "Lato") +
  geom_text(data = tmimata_statistikis %>% group_by(University) %>% filter(Year == max(Year)), aes(x = Year, y = Vasi/1000, label = round(Vasi/1000, digits = 1)),
            fontface = "bold", color = "white", size = 3) +
  scale_x_continuous("Year", breaks = 2013:2023) +
  scale_y_continuous(breaks = seq(0, 15, 2)) +
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    x = "Year",
    y = "Minimum Score",
    color = "University"
  ) +
  theme_light_custom()

final_plot_dark = ggplot2::ggplot(data = tmimata_statistikis, aes(x = Year, y = Vasi/1000, 
                                                                   color= University, 
                                                                   label = University),
                                   group = University) +
  geom_point(data = tmimata_statistikis |> dplyr::filter(Year == 2022),
             size = 9) +
  geom_textline(size = 3, vjust = -0.1,
                linewidth = 1.5, fontface = "bold", hjust = 0.5, family = "Lato") +
  geom_text(data = tmimata_statistikis %>% group_by(University) %>% filter(Year == max(Year)), aes(x = Year, y = Vasi/1000, label = round(Vasi/1000, digits = 1)),
            fontface = "bold", color = "white", size = 3) +
  scale_x_continuous("Year", breaks = 2013:2023) +
  scale_y_continuous(breaks = seq(0, 15, 2)) +
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    x = "Year",
    y = "Minimum Score",
    color = "University"
  ) +
  theme_dark_custom()

ggsave(
  filename = "R1/R1-Adm-Minimum-Grades/r1-adm-grades-en-light.png",
  plot = final_plot_light,
  device = "png",
  height = 4,
  width = 6)


ggsave(
  filename = "R1/R1-Adm-Minimum-Grades/r1-adm-grades-en-dark.png",
  plot = final_plot_dark,
  device = "png",
  height = 4,
  width = 6)

