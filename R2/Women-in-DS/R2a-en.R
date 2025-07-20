library(readr)
library(dplyr)
library(forcats)
library(ggplot2)
library(ggtext)
library(countrycode)
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
                                    hjust = 0.5, margin = margin(t=5, b=5)),
      plot.subtitle = element_textbox_simple(family = base_family, size = base_size + 0.5, lineheight = 1.1,
                                             color = "gray80", fill = NA, box.color = NA,
                                             margin = margin(t = 5, b = 10)
      ),
      plot.caption = element_markdown(color = "gray70"),
      legend.position = "none"
    )
}

kaggle_2021 = read_csv("R2/kaggle_survey_2021.csv")

# Delete second line
kaggle_2021 = kaggle_2021[-c(1),]

# Recoding Q2 variable

kaggle_2021$Q2 = kaggle_2021$Q2 %>%
  fct_recode(
    "Other" = "Nonbinary",
    "Other" = "Prefer not to say",
    "Other" = "Prefer to self-describe"
  )

## Recoding kaggle_2021$Q3

kaggle_2021$Q3 <- kaggle_2021$Q3 %>%
  fct_recode(
    "Hong Kong" = "Hong Kong (S.A.R.)",
    "Other" = "I do not wish to disclose my location",
    "Iran" = "Iran, Islamic Republic of...",
    "UAE" = "United Arab Emirates",
    "UK" = "United Kingdom of Great Britain and Northern Ireland",
    "USA" = "United States of America",
    "Vietnam" = "Viet Nam"
  )

data = kaggle_2021 %>%
  group_by(Q3) %>%
  summarise(n = n(),
            Women = sum(factor(Q2) == "Woman"),
            pct_women = Women/n *100) %>%
  dplyr::filter(Q3 != "Other")

data$iso2c <- countrycode(data$Q3, "country.name", "iso2c")
data$iso2c = tolower(data$iso2c)

title_text = glue("<b>Women participation in DS community per country</b>")
# subtitle_text = glue("Based on Kaggle's 2021 Survey women are underrepresented in DS. 
#                      The country with the highest women participation is Tunisia 
#                      and Peru is one with the lowest. Last but not least,
#                      **<span style= 'color: #001489;'>Greece</span>** has a relatively 
#                      disappointing rate of women participation holding 38th place with 15.7\\%, 
#                      given the fact that the average is {round(mean(data$pct_women),digits = 2)} %")
caption_text = glue("<b> Sources: </b> Kaggle Survey 2021<br><b>Visualization:</b> <span style='font-family:fb;'  >&#xf09b;</span> stesiam, 2024")
 
  
  
final_plot = ggplot(data = data) +
  geom_col(aes(x = pct_women, y = reorder(Q3, pct_women), fill = pct_women)) +
  scale_fill_gradient2(low="purple", high="purple4")+
  geom_label(data = subset(data, pct_women == max(pct_women) | Q3 == "Greece" | pct_women == min(pct_women)), aes(x = pct_women - 1.5, y = reorder(Q3, pct_women), label = paste0(round(pct_women, digits = 1), "%")), family = "Lato", size = 2.3) +
  ggflags::geom_flag(x = 0.3, aes(y = Q3,
                                  country = iso2c), 
                     size = 4) +
  scale_x_continuous(limits = c(0,40)) +
  labs(
    title = title_text,
    caption = caption_text,
    x = "Percentage (%) of Women",
    y = "Countries"
  ) +
  coord_cartesian(expand = FALSE)


final_plot_light = final_plot +
  geom_vline(xintercept = mean(data$pct_women), linetype = "dashed", color = "pink1") +
  geom_text(aes(x= mean(data$pct_women)+1, label=paste0("Average: ", round(mean(data$pct_women), digits = 2), " %"), y = "Colombia"), angle=270,
            family = "Lato", color = "black") +
  geom_richtext(x = 39, y = "Singapore",color = "black", hjust=1, 
                fill = NA, label.color = NA, # remove background and outline
                label.padding = grid::unit(rep(0, 4), "pt"),
                label = glue("Based on Kaggle\\'s 2021 Survey, <br>  women are underrepresented<br>in data science community."))+
  geom_richtext(x = 39, y = "Iraq",color = "black", hjust=1, 
                fill = NA, label.color = NA, # remove background and outline
                label.padding = grid::unit(rep(0, 4), "pt"),
                label = glue("The country with the highest <br>women participation is Tunisia and<br>Peru is one with the lowest")) +
  geom_richtext(x = 39, y = "Ukraine",color = "black", hjust=1, 
                fill = NA, label.color = NA, # remove background and outline
                label.padding = grid::unit(rep(0, 4), "pt"),
                label = glue("Last but not least,
                     **<span style= 'color: #0D5EAF;'>Greece</span>**<br>has a relatively 
                     disappointing rate of women<br> participation holding 38th place with 15.7\\%,<br> 
                     given the fact that the average is {round(mean(data$pct_women),digits = 2)} %"))  +
  theme_light_custom()

final_plot_dark = final_plot +
  geom_vline(xintercept = mean(data$pct_women), linetype = "dashed", color = "pink1") +
  geom_text(aes(x= mean(data$pct_women)+1, label=paste0("Average: ", round(mean(data$pct_women), digits = 2), " %"), y = "Colombia"), angle=270,
            family = "Lato", color = "gray90") +
  geom_richtext(x = 40, y = "Singapore",color = "white", hjust=1, 
                fill = NA, label.color = NA, # remove background and outline
                label.padding = grid::unit(rep(0, 4), "pt"),
                label = glue("Based on Kaggle\\'s 2021 Survey, <br>  women are underrepresented<br>in data science community."))+
  geom_richtext(x = 40, y = "Iraq",color = "white", hjust=1, 
                fill = NA, label.color = NA, # remove background and outline
                label.padding = grid::unit(rep(0, 4), "pt"),
                label = glue("The country with the<br> highest women participation is Tunisia and<br>Peru is one with the lowest")) +
  geom_richtext(x = 40, y = "Ukraine",color = "white", hjust=1, 
                fill = NA, label.color = NA, # remove background and outline
                label.padding = grid::unit(rep(0, 4), "pt"),
                label = glue("Last but not least,
                     **<span style= 'color: #0D5EAF;'>Greece</span>**<br>has a relatively 
                     disappointing rate of women<br> participation holding 38th place with 15.7\\%,<br> 
                     given the fact that the average is {round(mean(data$pct_women),digits = 2)} %")) +
  theme_dark_custom()

ggsave(
  filename = "R2/Women-in-DS/r2a-kaggle-women-ds-en-light.png",
  plot = final_plot_light,
  device = "png",
  dpi = 300,
  width = 7,
  height = 12)

ggsave(
  filename = "R2/Women-in-DS/r2a-kaggle-women-ds-en-dark.png",
  plot = final_plot_dark,
  device = "png",
  dpi = 300,
  width = 7,
  height = 12)
