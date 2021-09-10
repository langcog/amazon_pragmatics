library(tidyverse)
library(ggthemes)
library(ggrepel)
d <- read_csv("~/Downloads/roy_et_al_data.csv")

ms <- d %>%
  arrange(aoa) %>%
  mutate(aoa = floor(aoa),
         vocab = 1:n(),
         vocab_grp = cut(vocab, breaks = seq(0,700,50), 
                         labels = seq(25, 675, 50)),
         category = fct_recode(small.cat, 
                           Predicates = "Adjectives", 
                           Predicates = "Verbs")) %>%
  group_by(vocab_grp, category) %>%
  summarise(n = n()) %>%
  group_by(vocab_grp) %>%
  mutate(prop = n / sum(n))
    
  
ggplot(ms, aes(x = vocab_grp, y = prop, col = category)) + 
  geom_point() + 
  geom_smooth(se = FALSE, span = 1, aes(group = category)) + 
  theme_few() + 
  scale_color_solarized(guide = FALSE) +
  geom_label_repel(data = filter(ms, vocab_grp == "675"), 
                   aes(label = category)) + 
  ylab("Proportion of Vocabulary") + 
  xlab("Vocabulary Size")
  