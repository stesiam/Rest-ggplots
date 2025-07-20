library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(glue)
library(keyring)

library(showtext)
library(sysfonts)
library(ggplot2)
library(ggtext)
library(ggimage)

font_add_google("Roboto Slab", family = "rs")
font_add_google("Lato", "Lato")

sysfonts::font_add('fb', '_extensions/quarto-ext/fontawesome/assets/webfonts/fa-brands-400.ttf')
sysfonts::font_add('fs', '_extensions/quarto-ext/fontawesome/assets/webfonts/fa-solid-900.ttf')

showtext_auto()
showtext::showtext_opts(dpi = 300)

theme_light_custom <- function(base_size = 8, base_family = "Lato") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid = element_blank(),
      
      axis.title.y = element_blank(),
      
      text = element_text(color = "#222222"),
      plot.title = element_markdown(family = base_family, size = base_size + 4, face = "bold", color = "#000000",
                                    hjust = 0.5, margin = margin(t=5, b = 5)),
      plot.subtitle = element_textbox_simple(family = base_family, size = base_size + 0.5, lineheight = 1.1,
                                             color = "#444444", margin = margin(b = 10),
                                             padding = margin(5,5,5,5)),
      plot.caption = element_markdown(color = "#444444", lineheight = 1.2, margin = margin(b = 3, r = 5)),
      legend.position = "none",
      axis.text.x = ggtext::element_markdown(),
      axis.text.y = ggtext::element_markdown()
    )
}

theme_dark_custom <- function(base_size = 8, base_family = "Lato") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.background = element_rect(fill = "#181818", color = NA),
      panel.background = element_rect(fill = "#181818", color = NA),
      panel.grid = element_blank(),
      
      axis.title.y = element_blank(),
      
      text = element_text(color = "white"),
      
      plot.title = element_markdown(family = base_family, size = base_size + 4, face = "bold", color = "white",
                                    hjust = 0.5, margin = margin(t=5, b = 5)),
      plot.subtitle = element_textbox_simple(family = base_family, size = base_size + 0.5, lineheight = 1.1,
                                             color = "gray80", fill = NA, box.color = NA, margin = margin(b = 10),
                                             padding = margin(5,5,5,5)),
      plot.caption = element_markdown(color = "gray70", lineheight = 1.2, margin = margin(b = 3, r = 5)),
      legend.position = "none",
      axis.text.x = ggtext::element_markdown(),
      axis.text.y = ggtext::element_markdown()
    )
}



parliament <- read_csv("R4/greek_parliament.csv")

## Recoding parliament$Party
parliament$Party[parliament$Party == "ANEXARTITOI DIMOKRATIKOI VOULEFTES"] <- "ADP"
parliament$Party[parliament$Party == "ANEXARTITOI ELLINES (Independent Hellenes)"] <- "ANEL"
parliament$Party[parliament$Party == "ANEXARTITOI ELLINES (Independent Hellenes) National Patriotic Democratic Alliance"]<- "ANEL"
parliament$Party[parliament$Party == "Coalition of the Left and Progress"] <- "SYRIZA"
parliament$Party[parliament$Party == "Communist Party of Greece (Interior)"] <- "KKE (interior)"
parliament$Party[parliament$Party == "DEMOCRATIC COALITION (Panhellenic Socialist Movement Democratic Left )"] <- "PASOK"
parliament$Party[parliament$Party == "DHM.AR (Democratic Left)"] <- "DHMAR"
parliament$Party[parliament$Party == "DI.ANA."] <- "DIANA"
parliament$Party[parliament$Party == "DI.K.KI."] <- "DIKKI"
parliament$Party[parliament$Party == "INDEPENDENT"] <- "INDEPENDENT"
parliament$Party[parliament$Party == "KOMMOUNISTIKO KOMMA ELLADAS"] <- "KKE"
parliament$Party[parliament$Party == "LA.O.S."] <- "LAOS"
parliament$Party[parliament$Party == "LAIKI ENOTITA"] <- "LAE"
parliament$Party[parliament$Party == "LAIKOS SYNDESMOS - CHRYSI AVGI (People’s Association – Golden Dawn)"] <- "XA"
parliament$Party[parliament$Party == "NEA DIMOKRATIA"] <- "ND"
parliament$Party[parliament$Party == "PA.SO.K. (Panhellenic Socialist Movement)"] <- "PASOK"
parliament$Party[parliament$Party == "POL.A."] <- "POLA"
parliament$Party[parliament$Party == "SYNASPISMOS RIZOSPASTIKIS ARISTERAS"] <- "SYRIZA"
parliament$Party[parliament$Party == "TO POTAMI (The River)"] <- "POTAMI"
parliament$Party[parliament$Party == "ΟΟ.ΕΟ."] <- "EO"

names = c("Panhellenic Socialistic Mpvement", "New Democracy", "Communist Party of Greece", "Communist Party οf Greece (interior)",
          "Independent", "Coalition of the Radical Left", "Democratic Renewal", "Alternative Ecologists", "Political Spring",
          "Democratic Social Movement", "Popular Orthodox Rally", "Democratic Left", "Independent Greeks", "Golden Dawn", "Independent Democratic MPs",
          "Popular Unity", "The River")

parties = data.frame(
  Party = unique(parliament$Party)
) |>
  mutate(Color = case_when(
    Party == "PASOK" ~ "#95bb72",
    Party == "ND" ~ "#0492c2",
    Party == "KKE" ~ "#FF6666",
    Party == "SYRIZA" ~ "#e27bb1",
    Party == "KKE (interior)" ~ "#FF3366",
    Party == "INDEPENDENT" ~ "#ffffff",
    Party == "DIANA" ~ "orange",
    TRUE ~ "#808080"
  ))

kke_color = "#FF6666"
nd_color = "#0492c2"
pasok_color = "#95bb72"
syriza_color = "#e27bb1"

parties$names = names

parties$Party_el= c("ΠΑΣΟΚ", "ΝΔ", "ΚΚΕ", "ΚΚΕ (εσωτερικού)", "Ανεξάρτητοι", "ΣΥΡΙΖΑ", "ΔΗΑΝΑ", "ΕΟ", "ΠΟΛΑΝ", "ΔΗΚΚΙ",
                    "ΛΑΟΣ", "ΔΗΜΑΡ", "ΑΝΕΛ", "ΧΑ", "ΑΔΒ", "ΛΑΕ", "ΠΟΤΑΜΙ")

parties$names_el = c("Πανελλήνιο Σοσιαλιστικό Κίνημα", "Νέα Δημοκρατία", "Κομμουνιστικό Κόμμα Ελλάδας", 
                     "ΚΚΕ (εσωτερικού)","Ανεξάρτητοι", "Συνασπισμός Ριζοσπαστικής Αριστεράς", "Δημοκρατική Ανανέωση", 
                     "Εναλλακτικοί Οικολόγοι", "Πολιτική Άνοιξη",
                     "Δημοκρατικό Κοινωννικό Κίνημα", "Λαϊκός Ορθόδοξος Συναγερμός", "Δημοκρατική Αριστερά", "Ανεξάρτητοι Έλληνες",
                     "Χρυσή Αυγή", "Ανεξ Δημ. Βουλευτές", "Λαϊκή Ενότητα", "Το Ποτάμι")

pattern <- "^([^-\\s]+)(?:\\s*-\\s*([^-\\s]+))?\\s+([^-\\s]+)(?:\\s*-\\s*([^-\\s]+))?(?:\\s+([^-\\s]+))?$"

parties = data.frame(
  Party = c("PASOK", "ND", "KKE", "SYRIZA"),
  logo = c(
    "https://upload.wikimedia.org/wikipedia/commons/b/b9/Panellinio_Sosialistiko_Kinima_Logo.svg",
    "https://upload.wikimedia.org/wikipedia/commons/thumb/d/df/New_Democracy_Logo_2018.svg/250px-New_Democracy_Logo_2018.svg.png",
    "https://upload.wikimedia.org/wikipedia/commons/thumb/b/b6/Kke_sima.svg/120px-Kke_sima.svg.png",
    "https://upload.wikimedia.org/wikipedia/commons/b/b4/SYRIZA_logo_2020.svg"
  )
)

party_images = c("https://upload.wikimedia.org/wikipedia/commons/thumb/d/df/New_Democracy_Logo_2018.svg/250px-New_Democracy_Logo_2018.svg.png")
mps_clean_names = parliament %>%
  mutate(FullName = str_trim(str_replace(FullName, "\\(.*", ""))) %>%
  extract(FullName, into = c("surname1", "surname2", "name1", "name2", "father_name"), 
          regex = pattern, remove = FALSE)

freqs_mps_elected = parliament %>%
  group_by(FullName) %>%
  summarise(n = n(), Party = first(Party)) %>%
  dplyr::filter(n >=9) %>%
  mutate(FullNameGR = deeplr::translate2(FullName, target_lang = "EL", source_lang = "EN",auth_key = key_get("deepl"))) %>%
  mutate(FullNameGR = str_trim(str_replace(FullNameGR, "\\(.*", ""))) %>%
  extract(FullNameGR, into = c("surname1", "surname2", "name1", "name2", "father_name"), 
          regex = pattern, remove = FALSE) %>%
  unite(col = full_surname, surname1, surname2, sep = " ") %>%
  mutate(initials = str_sub(name1,start = 1, end=1)) %>%
  unite(col = "FullNameEdit", full_surname, initials, sep = "") %>%
  select(FullNameEdit, n, Party, FullNameGR) %>%
  arrange(-n)

title_text = glue("**Πόσες φορές εκλέχτηκαν ως βουυλευτές; (1981 - 2019)**")
subtitle_text = glue("Μία λίστα με τους βουλευτές του ελληνικού κοινοβουλίου που 
                      έχουν εκλεγεί περισσσότερες φορές (9 φορές ή παραπάνω).
                       Η συντριπτική πλειοψηφία αυτών ανήκουν ή άνηκαν στη <span style = 'color:#0492c2;'>Νέα Δημοκρατία</span> 
                       (<span style = 'color:#0492c2;'>ΝΔ</span>) και το
                     <span style = 'color:#95bb72;'>Πανελλήνιο Σοσιαλιστικό Κίνημα</span> 
                     (<span style = 'color:#95bb72;'>ΠΑΣΟΚ</span>). Μόλις τέσσερις βουλευτές ανήκουν σε
                     άλλα κόμματα, δύο από το <span style = 'color:#FF6666;'>Κομμουνιστικό Κόμμα Ελλάδας</span> (ΚΚΕ) και 
                     οι υπόλοιποι δύο από το <span style = 'color:#e27bb1;'>Συνασπισμό Ριζοσπαστικής Αριστεράς</span> (ΣΥΡΙΖΑ)")
caption_text = glue("<b> Δεδομένα: </b> Ιστοσελίδα Ελληνικού Κοινοβουλίου (hellenicparliament.gr)<br>
                    <span style='font-family:fb;'  >&#xf09b;</span> <b>stesiam</b>, 2022")

freqs_mps_elected_combined = left_join(freqs_mps_elected, parties, by="Party")

final_plot = ggplot(data = freqs_mps_elected_combined, aes(x = reorder(FullNameEdit, n), y = n, fill = Party ))+
  geom_bar(stat = "identity",width = 0.88) +
  geom_text(aes(label=n), hjust = 1.5, vjust=0.5, color="white", size=3, family = "Lato")+
  scale_fill_manual(values = c("KKE" = kke_color,"ND" = nd_color, "PASOK" = pasok_color, "SYRIZA" = syriza_color)) +
  labs(title = title_text,
       subtitle = subtitle_text,
       caption = caption_text,
       y = "Times elected") +
  scale_y_continuous(limits = c(0,15)) +
  geom_image(aes(image = logo, y = 0), size = 0.02,nudge_y = -0.4) +
  coord_flip()  +
  theme(
    axis.text.x = ggtext::element_markdown()
  )

final_plot_light = final_plot + 
  geom_richtext(y = 12.5, x = "Παυλίδης Α", label = glue("εκλέχτηκαν<br><span style='font-size: 20pt;'><span style='font-family:fs;'  >&#xf772;</span> 9 φορές</span> <br>"),
                fill = NA, label.color = NA, lineheight = 2,
                label.padding = grid::unit(rep(0, 4), "pt")) +
  geom_richtext(y = 13, x = "Παπαρήγα Α", label = glue("εκλέχτηκαν<br><span style='font-size: 20pt;'><span style='font-family:fs;'  >&#xf772;</span> 10 φορές</span>"),
                fill = NA, label.color = NA, lineheight = 2,
                label.padding = grid::unit(rep(0, 4), "pt")) +
  geom_richtext(y = 13.5, x = "Νικολόπουλος Ν", label = glue("εκλέχτηκαν<br><span style='font-size: 20pt;'><span style='font-family:fs;'  >&#xf772;</span> 11 <br> φορές</span>"),
                fill = NA, label.color = NA, lineheight = 2,
                label.padding = grid::unit(rep(0, 4), "pt")) +
  geom_richtext(y = 14, x = "Μεϊμαράκης Ε", label = glue("<span style='font-size: 20pt;'><span style='font-family:fs;'  >&#xf772;</span> 12</span>"),
                fill = NA, label.color = NA, hjust=0.5,
                label.padding = grid::unit(rep(0, 4), "pt")) +
  geom_richtext(y = 14.5, x = "Παπανδρέου Γ", label = glue("<span style='font-size: 20pt;'><span style='font-family:fs;'  >&#xf772;</span> 13</span>"),
                fill = NA, label.color = NA, hjust=0.5,
                label.padding = grid::unit(rep(0, 4), "pt")) +
  theme_light_custom(base_size = 9, base_family = "rs") 

final_plot_dark = final_plot +
  geom_richtext(y = 12.5, x = "Παυλίδης Α", label = glue("εκλέχτηκαν<br><span style='font-size: 20pt;'><span style='font-family:fs;'  >&#xf772;</span> 9 φορές</span> <br>"),
                fill = NA, label.color = NA, color = "gray90",  lineheight = 2,
                label.padding = grid::unit(rep(0, 4), "pt")) +
  geom_richtext(y = 13, x = "Παπαρήγα Α", label = glue("εκλέχτηκαν<br><span style='font-size: 20pt;'><span style='font-family:fs;'  >&#xf772;</span> 10 φορές</span> <br>"),
                fill = NA, label.color = NA, color = "gray90",  lineheight = 2,
                label.padding = grid::unit(rep(0, 4), "pt")) +
  geom_richtext(y = 13.5, x = "Νικολόπουλος Ν", 
                label = glue("εκλέχτηκαν<br><span style='font-size: 20pt;'><span style='font-family:fs;'  >&#xf772;</span> 11 <br> φορές</span>"),
                fill = NA, label.color = NA, color = "gray90", lineheight = 2,
                label.padding = grid::unit(rep(0, 4), "pt")) +
  geom_richtext(y = 14, x = "Μεϊμαράκης Ε", label = glue("<span style='font-size: 20pt;'><span style='font-family:fs;'  >&#xf772;</span> 12</span>"),
                fill = NA, label.color = NA, hjust=0.5, color = "gray90",
                label.padding = grid::unit(rep(0, 4), "pt")) +
  geom_richtext(y = 14.5, x = "Παπανδρέου Γ", label = glue("<span style='font-size: 20pt;'><span style='font-family:fs;'  >&#xf772;</span> 13</span>"),
                fill = NA, label.color = NA, hjust=0.5, color = "gray90",
                label.padding = grid::unit(rep(0, 4), "pt")) +
  theme_dark_custom(base_size = 9, base_family = "rs")


ggsave(
  filename = "R4/r4-el-light.png",
  plot = final_plot_light,
  device = "png",
  dpi = 300,
  width = 7,
  height = 12)

ggsave(
  filename = "R4/r4-el-dark.png",
  plot = final_plot_dark,
  device = "png",
  dpi = 300,
  width = 7,
  height = 12)
