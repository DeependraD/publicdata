# Load libraries
require(tidyverse)
require(colorspace)
require(gganimate)

## Import datasets
field_std <- readxl::read_xlsx("./data/seed_certification_requirements.xlsx", "field_standards")
seed_std_np <- readxl::read_xlsx("./data/seed_certification_requirements.xlsx", "seed_standards_nepali")

## Field standards for certification
# Isolation distance, Maximum off type, Maximum disease
field_params <- c("Isolation", "Maximum off type", "Maximum disease")

field_std_crops <- field_std %>% 
  filter(Crop %in% c("Wheat (Triticum aestivum)", "Maize (Zea mays)", "Paddy (Oryza sativa)", "Lentil")) %>% 
  droplevels.data.frame() %>%
  select(Crop, contains("Maximum disease")) %>% 
  gather(key = "Seed_class", value = "Maximum disease", -Crop)

field_std_crops %>% 
  ggplot(aes(Crop, `Maximum disease`)) +
  geom_col(na.rm = TRUE, 
           show.legend = TRUE, aes(fill = Seed_class),
           inherit.aes = TRUE, position = position_dodge()) +
  scale_y_continuous(breaks = function(x){seq(0, x[2], length.out = 10)}) +
  scale_x_discrete(limits = field_std_crops %>%
                     arrange(-`Maximum disease`) %>% 
                     pull("Crop")) +
  ylab(field_params[3]) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 75, vjust = 0.6, size = 8), 
        legend.text = element_text(size = 8))

# ggsave(filename = "seed_certification_requirements_isolation.png", width = 6, height = 8, units = "in")
# ggsave(filename = "seed_certification_requirements_max_off.png", width = 6, height = 8, units = "in")
ggsave(filename = "seed_certification_requirements_max_disease.png", width = 6, height = 8, units = "in")
