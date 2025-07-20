library(dplyr)
library(ggplot2)
library(ggtext)
library(glue)
library(sysfonts)
library(showtext)

font_add_google("Roboto Slab", family = "rs")

#sysfonts::font_add_google("Gentium Book Basic", "gp")
# for Quarto website
# sysfonts::font_add('fb', '_extensions/quarto-ext/fontawesome/assets/webfonts/fa-brands-400.ttf')
# sysfonts::font_add('fs', '_extensions/quarto-ext/fontawesome/assets/webfonts/fa-solid-900.ttf')

sysfonts::font_add('fb', '/home/stelios/Downloads/fontawesome-free-6.7.2-desktop/otfs/Font Awesome 6 Brands-Regular-400.otf')
sysfonts::font_add('fs', '/home/stelios/Downloads/fontawesome-free-6.7.2-desktop/otfs/Font Awesome 6 Free-Solid-900.otf')

showtext_auto()
showtext::showtext_opts(dpi = 300)


theme_light_custom <- function(base_size = 8, base_family = "rs") {
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
      plot.subtitle = element_textbox_simple(family ="rs", size = base_size + 0.5, lineheight = 1.1,
                                             color = "#444444",
                                             margin = margin(t = 5, b = 5)),
      plot.caption = element_markdown(color = "#444444"),
      legend.position = "none"
    )
}

theme_dark_custom <- function(base_size = 8, base_family = "rs") {
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
      plot.subtitle = element_textbox_simple(family = "rs", size = base_size + 0.5, lineheight = 1.1,
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

title_text = glue("<b>Θέσεις εισαγωγής σε τμήματα Στατιστικής (ΓΛ90)</b>")
subtitle_text = glue("Oι θέσεις για όσους θέλουν να φοιτήσουν στο τμήμα 
                      **<span style='color:#7cae00;'>Στατιστικής του ΟΠΑ</span>** φαίνεται 
                      να είναι αρκετά περιορισμένες για τους
                     υποψήφιους των Γενικών Λυκείων προσφέροντας λίγες παραπάνω από 100. 
                     Τα τμήματα **<span style='color:#f8766d;'>Αιγαίου</span>** και 
                     **<span style='color:#00bfc4;'>Πειραιά</span>** τουλάχιστον
                     τα τελευταία χρόνια ταυτίζονται ως προς τους πόσους θα δεχτούν 
                     (από τη κατηγορία ΓΛ90) με το τμήμα του Πειραιά να μειώνει
                     σημαντικά τους εισακτέους του, σε σύγκριση με το 2013. Τέλος,
                     το νεοσυσταθέν τμήμα της **<span style='color:#c77cff;'>Δυτικής Μακεδονίας</span>** 
                     ανακοινώνει σημαντικά περισσότερες θέσεις και με μεγάλες αποκλίσεις από 
                     έτος σε έτος που μπορεί να οφείλεται σε μη πλήρωση αυτών από
                     προηγούμενες χρονιές.")

caption_text = glue("<b> Πηγές:</b> [1] Υπουργείο Παιδείας, Θρησκευμάτων και Αθλητισμού, [2] aeitei.gr<br><b>Γράφημα:</b> <span style='font-family:fb;'  >&#xf09b;</span> stesiam, 2024")


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
    x = "Έτος",
    y = "Διαθέσιμες θέσεις",
    color = "Πανεπιστήμιο"
  )

final_plot_light = final_plot +
  theme_light_custom()

final_plot_dark = final_plot +
  theme_dark_custom()

ggsave(
  filename = "R1/R1-Adm-Seats/r1-adm-seats-el-light.png",
  plot = final_plot_light,
  device = "png",
  height = 4,
  width = 6)

ggsave(
  filename = "R1/R1-Adm-Seats/r1-adm-seats-el-dark.png",
  plot = final_plot_dark,
  device = "png",
  height = 4,
  width = 6)
