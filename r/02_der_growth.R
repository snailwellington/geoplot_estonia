
library(tidyverse)
der <- readRDS(file = "data/der_plot_data.RDS")
cus_dblue <- "#002c77"

der_growth <- der %>% 
  mutate(op_year = lubridate::year(operational),
         prod_type = ifelse(is.na(prod_type) == TRUE, "Teadmata",prod_type)) %>% 
  filter(is.na(op_year) != TRUE)


growth_plot <- der_growth %>% 
  group_by(op_year,prod_type) %>% 
  summarise(count = n())


ggplot(growth_plot, aes(op_year,count, fill = prod_type))+
  geom_col(color = cus_dblue, alpha = 0.99)+
  labs(title = "ELV-ga liitunud mikro- ja väiketootjad",
       x = "Liitumise aasta",
       y = "Liitumiste hulk, tükkides",
       fill = "Tootja tüüp")+
  scale_fill_brewer(palette = "Set3")+
  scale_x_continuous(breaks = seq(2012,lubridate::year(Sys.Date()),1), limits = c(2011,lubridate::year(Sys.Date())+1))+
  theme(text = element_text(size = 28),
        panel.grid.major.y = element_line(linetype = 3, size = 1, color = "grey30"))


