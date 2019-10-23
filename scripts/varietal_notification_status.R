# Load libraries
require(tidyverse)
require(colorspace)

## Wheat purchase data import ##
varietal_status <- readxl::read_xlsx("F:/Dworkspace/data/misc/varietal_release_registration_status.xlsx")

# wheat purchase visualization
varietal_status <- varietal_status %>% 
  slice(-nrow(.)) %>% 
  arrange(-`Total number of varieties`) %>%
  mutate(Registration = if_else(!is.na(Registration), Registration, 0)) %>% 
  mutate(`Denotified` = if_else(!is.na(`Denotified`), `Denotified`, 0)) %>% 
  select(-SN, -`Total number of varieties`) %>% 
  gather(key = "State", value = "Number of varieties", -Crop) 

ggvarstatus <- varietal_status %>% 
  ggplot(aes(Crop, `Number of varieties`)) +
  geom_col(na.rm = TRUE, 
           show.legend = TRUE, aes(fill = `State`),
           inherit.aes = TRUE, position = position_dodge()) +
  scale_y_continuous(breaks = function(x){seq(0, x[2]+20, 30)}) +
  scale_x_discrete(limits = varietal_status %>%
                     group_by(Crop) %>%
                     summarise_at("Number of varieties", function(x)sum(x, na.rm = TRUE)) %>%
                     arrange(-`Number of varieties`) %>% pull(Crop)) +
  ylab("Number of varieties") +
  ggtitle("Varieties of different crops notified and denotified as of 2074-12-12", subtitle = "Source: SQCC, 2018") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.6, size = 8), 
        legend.text = element_text(size = 8))
ggvarstatus

ggsave("varietal_status_2074-12-12.png", ggvarstatus, width = 10, units = "in")
