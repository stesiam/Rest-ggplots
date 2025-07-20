library(dplyr)
library(ggplot2)
library(ggtext)
library(glue)
library(sysfonts)
library(showtext)

font_add_google("Roboto Slab", family = "clim")
font_add_google("Raleway", family = "mont")
font_add_google("Lato", family = "Lato")

sysfonts::font_add('fb', '/home/stelios/Downloads/fontawesome-free-6.7.2-desktop/otfs/Font Awesome 6 Brands-Regular-400.otf')
sysfonts::font_add('fs', '/home/stelios/Downloads/fontawesome-free-6.7.2-desktop/otfs/Font Awesome 6 Free-Solid-900.otf')

showtext_auto()
showtext::showtext_opts(dpi = 300)


theme_light_custom <- function(base_size = 8, base_family = "Lato") {
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
      plot.subtitle = element_textbox_simple(family ="mont", size = base_size + 0.5, lineheight = 1.1,
                                             color = "#444444",
                                             margin = margin(t = 5, b = 5)),
      plot.caption = element_markdown(color = "#444444"),
      legend.position = "none"
    )
}

theme_dark_custom <- function(base_size = 8, base_family = "Lato") {
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
  "Theseis" = c(99, 112, 103, 99, 103, 104, 113, 102, 103, 101,
                189, 202, 198, 190, 198, 198, 216, 194, 154, 152, 
                144, 137, 163, 156, 162, 178, 198, 171, 179, 165,
                NA, NA, NA, NA, NA, NA, 268, 225, 316, 256),
  "Vasi" = c(12788, 13277, 13488, 13158, 13511, 13354, 13337, 13425, 13316, 13828,
             12666, 13270, 13795, 12703, 12543, 11967, 11710, 12100, 11673, 12460,
             9618, 10636, 10574, 10471, 10692, 8525, 5148, 3925, 7797, 8425,
             NA, NA, NA, NA, NA, NA, 4846, 3950, 7867, 7740)
)

title_text = glue("<b>Available Admission Seats on Statistics Departments</b>")
subtitle_text = glue("The number of places available for those wishing to study in the
<span style='color:#7cae00;'>Statistics Department of the Athens University of Economics and Business (AUEB)</span>
appears to be quite limited for candidates from General High Schools, offering just over 100 spots.
The departments of <span style='color:#f8766d;'>University of the Aegean</span> and
<span style='color:#00bfc4;'>University of Piraeus</span> have, at least in recent years,
offered the same number of places, with the Piraeus department significantly reducing
its intake compared to 2013. Finally, the newly established department of
<span style='color:#c77cff;'>Western Macedonia</span> announces significantly more places,
with large fluctuations from year to year—possibly due to previous years' positions remaining unfilled.")

caption_text = glue("<b> Sources:</b> [1] Ministry of Education, Religious Affairs and Sports, [2] aeitei.gr<br><b>Graph:</b> <span style='font-family:fb;'  >&#xf09b;</span> stesiam, 2024")


final_plot = ggplot2::ggplot(data = tmimata_statistikis, aes(x = Year, y = Theseis, color= University)) +
  geom_line() +
  geom_point(size = 8) +
  geom_richtext(aes(x = Year, y = Theseis, label = Theseis),
                color = "white", size = 3.2, fontface = "bold",
                fill = NA, label.color = NA) +
  scale_x_continuous("Έτος", breaks = 2013:2022) +
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    x = "Year",
    y = "# of Seats",
    color = "University"
  )

final_plot_light = final_plot +
  theme_light_custom()

final_plot_dark = final_plot +
  theme_dark_custom()

ggsave(
  filename = "R1/R1-Adm-Seats/r1-adm-seats-en-light.png",
  plot = final_plot_light,
  device = "png",
  height = 4,
  width = 6)

ggsave(
  filename = "R1/R1-Adm-Seats/r1-adm-seats-en-dark.png",
  plot = final_plot_dark,
  device = "png",
  height = 4,
  width = 6)