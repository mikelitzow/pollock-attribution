# evaluate the effect of SST on pollock age structure as estimated by acoustic trawl survey

library(tidyverse)
theme_set(theme_bw())

# load data
dat <- read.csv("./data/estimated_millions_at_age_tab1.10_GOA_2021assessment.csv")

str(dat)

names(dat) <- c("year", "age1", "age2", "age3", "age4", "age5", "age6", "age7",
                "age8", "age9", "age10", "age11", "age12", "age13", "age14", "age15", "total")

# calculate age proportions
age.prop <- dat %>%
  select(-year, -total)

# remove age 1-3 (not mature!) from dat$total
for(i in 1:nrow(dat)){
dat$total[i] <- sum(dat[i,(5:16)])
}

for(i in 1:nrow(age.prop)){
age.prop[i,] <- age.prop[i,] / dat$total[i]
}

# limit to mature animals, age 3+
age.prop <- age.prop %>%
  select(-age1, -age2, -age3)

# check
rowSums(age.prop) # looks right

# plot to check
plot <- age.prop %>%
  mutate(year = dat$year) %>%
  pivot_longer(cols = -year)


ggplot(plot, aes(name, value)) +
  geom_bar(stat = "identity") +
  facet_wrap(~year)


## use Shannon-Weiner to calculate age diversity

# drop estimates of 0
plot.trimmed <- plot %>%
  dplyr::filter(value > 0)

plot.trimmed$product <- plot.trimmed$value * log(plot.trimmed$value)

shannon <- plot.trimmed %>%
  group_by(year) %>%
  summarise(shannon = -sum(product))

# plot to check
ggplot(shannon, aes(year, shannon)) +
  geom_line() +
  geom_point()

# save
write.csv(shannon, "./data/assessment_report_age_diversity.csv")
