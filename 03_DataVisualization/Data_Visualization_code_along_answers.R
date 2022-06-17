

# Code along 1 ------------------------------------------------------------

ggplot(homes) +
  aes(x = Age, y = FinSqFt) +
  geom_point(shape = 2)


# Code along 2 ------------------------------------------------------------

ggplot(homes) +
  aes(x = LotSize, y = TotalValue) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~HSDistrict)


# Code along 3 ------------------------------------------------------------

ggplot(homes) +
  aes(x = LotSize, y = TotalValue) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ HSDistrict) +
  coord_cartesian(xlim = c(0, 5),
                  ylim = c(0, 1e6)) +
  scale_y_continuous(labels = dollar)


# Code along 4 ------------------------------------------------------------

ggplot(homes) +
  aes(x = Condition, fill = HSDistrict) +
  geom_bar(position = "fill")


# Code along 5 ------------------------------------------------------------


ggplot(homes) +
  aes(x = ESDistrict, y = Age) +
  geom_boxplot() +
  coord_flip()


# Code along 6 ------------------------------------------------------------


ggplot(homes_mean_fb) +
  aes(x = YearBuilt, y = FullBath) +
  geom_line()
ggplotly()


