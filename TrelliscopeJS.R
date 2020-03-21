devtools::install_github("hafen/trelliscopejs")
install.packages(c("gapminder", "housingData", "rbokeh"))

library(trelliscopejs)
library(gapminder)

# Gapminder data
qplot(year, lifeExp, data = gapminder) +
  xlim(1948, 2011) + ylim(10, 95) + theme_bw() +
  facet_trelliscope(~ country + continent, nrow = 2, ncol = 7, width = 300)

# Housing data with dplyr and rbokeh
library(rbokeh)
library(dplyr)
library(housingData)

lm_coefs <- function(x, y)
  coef(lm(y ~ x))

d <- housing %>%
  group_by(county, state) %>%
  summarise(
    slope = lm_coefs(time, medListPriceSqft)[2],
    mean_list = mean(medListPriceSqft, na.rm = TRUE),
    mean_sold = mean(medSoldPriceSqft, na.rm = TRUE),
    n_obs = length(which(!is.na(medListPriceSqft))),
    zillow_link = cog_href(
      sprintf("http://www.zillow.com/homes/%s_rb/",
              gsub(" ", "-", paste(county, state)))[1]),
    panel = panel(
      figure(xlab = "time", ylab = "median list / sq ft", toolbar = NULL) %>%
        ly_points(time, medListPriceSqft,
                  hover = data_frame(time = time, mean_list = medListPriceSqft)))
  ) %>%
  filter(n_obs > 1)

# one of our columns is an rbokeh object.
# We can now pipe this data frame into trelliscope()

d %>%
  trelliscope(name = "list_vs_time")

