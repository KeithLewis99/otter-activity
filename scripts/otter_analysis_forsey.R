# but at some point, we realized that there were issues with the data (we were lacking much of it) that had been provided and it was decided that it was easier just to start over again.  So, Cote found David Forsey who kindly agreed to work on the project and redo all the distance analyses.  

# otter_analysis.R is DEPRECATED but this will follow the same basic format.  The big change is 1) the data used and 2) using loops to generate folders

df1 <- read.csv("data_derived/df_all_new.csv")

str(df1)

#libraries ----
library(ggplot2) 
library(GGally) 
library(car)
library(dplyr)
library(DHARMa)
library(purrr)

# Latrine Presence ----
## EDA ----
### 1. outliers x and y ----
## a few outliers at high end for all EVs but nothing terribly note worthy or of concern given the data set involved - perhaps dcabin and dstreammouth
# contingency tables
df1 |>
  group_by(location, type) |>
  summarise(count = n())


# not really outliers, just getting a feel for the data
with(df1, table(location, type, road_resource_dist))
ggplot(df1, aes(x = road_national_dist)) + geom_histogram() + facet_grid(location ~ type)
ggplot(df1, aes(x = road_resource_dist)) + geom_histogram() + facet_grid(location ~ type)
ggplot(df1, aes(x = all_road_dist)) + geom_histogram() + facet_grid(location ~ type)

ggplot(df1, aes(x = river_mout_dist)) + geom_histogram() + facet_grid(location ~ type)
ggplot(df1, aes(x = fresh_lake_dist)) + geom_histogram() + facet_grid(location ~ type)
ggplot(df1, aes(x = river_dist)) + geom_histogram() + facet_grid(location ~ type)

ggplot(df1, aes(x = cabin_dist)) + geom_histogram() + facet_grid(location ~ type)


library(tidyverse)

vars <- c(
  "road_national_dist",
  "road_resource_dist",
  "all_road_dist",
  "river_mout_dist",
  "fresh_lake_dist",
  "river_dist",
  "cabin_dist"
)

plots <- vars %>%
  set_names() %>%
  map(~ ggplot(df1, aes(x = .data[[.x]])) +
        geom_histogram() +
        facet_grid(location ~ type) +
        labs(x = .x))
plots[[1]]

if(!dir.exists("figs/outliers"))dir.create("figs/outliers")
folder <- "figs/outliers/"
walk2(
  plots,
  names(plots),
  ~ ggsave(
    filename = paste0(folder, .y, ".png"),
    plot = .x,
    width = 8,
    height = 6
  )
)

## much overlap but cabins are often kilometers away in TN
## no real difference between Y and N
ggplot(df1, aes(location, cabin_dist)) + 
  geom_violin() + 
  facet_grid(~type)

# values not available for AB - so we can make comparisons for TNNP only
# forage area probably a bit bigger for Y
ggplot(df1, aes(location, dforagearea)) + 
  geom_violin() + 
  facet_grid(~type)

# about the same for all
ggplot(df1, aes(location, river_dist)) + geom_violin() + facet_grid(location ~ type)







# Distance at least 1 km greater in general for TN but a few values very close for Y
ggplot(df1, aes(location, droad)) + 
  geom_violin() + 
  facet_grid(~type)

# about the same
ggplot(df1, aes(location, dstreammouth)) + 
  geom_violin() + 
  facet_grid(~type)



ggplot(df_latPlot, aes(name, value, fill = name)) + geom_violin() + facet_grid(location ~ type)

# plot all togehter
ggplot(df_latPlot |> filter(location == "Terra Nova"), aes(name, value, fill = name)) + geom_violin() + facet_grid(. ~ type)

ggplot(df_latPlot |> filter(location == "Alexander Bay"), aes(name, value, fill = name)) + geom_violin() + facet_grid(. ~ type)

ggplot(df_latPlot, aes(name, value, fill = name)) + geom_boxplot() + facet_grid(location ~ type)

df_latPlot |>
  filter(location == "Terra Nova") |>
  ggplot(aes(name, value, fill = name)) + geom_violin() + facet_grid(~ type)


# Cleavland dot plots
ggplot(df1, aes(droad, y = seq(1, length(droad),1), fill = location, colour = location)) + geom_point() + ylab("ID") + facet_grid(~type)

ggplot(df1, aes(dcabin, y = seq(1, length(droad),1), fill = location, colour = location)) + geom_point() + ylab("ID") + facet_grid(~type)

ggplot(df1, aes(dlogging, y = seq(1, length(droad),1), fill = location, colour = location)) + geom_point() + ylab("ID") + facet_grid(~type)

ggplot(df1, aes(dstreammouth, y = seq(1, length(droad),1), fill = location, colour = location)) + geom_point() + ylab("ID") + facet_grid(~type)

ggplot(df1, aes(dfreshwater, y = seq(1, length(droad),1), fill = location, colour = location)) + geom_point() + ylab("ID") + facet_grid(~type)

# again, no data for AB so comparisons are for TNNP only
ggplot(df1, aes(dforagearea, y = seq(1, length(droad),1), fill = location, colour = location)) + geom_point() + ylab("ID") + facet_grid(~type)


### 4. zero trouble y ----
table(df1$lat.pres, df1$location)
length(df1$droad[df1$droad < 10])
length(df1$droad[df1$dcabin < 10])
length(df1$droad[df1$dstreammouth < 10])
# not valid to compare dforage area bc no values for AB

length(df1$droad[df1$dlogging < 10])
plot(density(df1$dlogging))


### 5/6. collinearity x/relationshiop y/x ----
Scatter_Matrix <- ggpairs(df1,columns = c(2:7, 9), 
                          title = "Scatter Plot Matrix for latrine Dataset", 
                          axisLabels = "show") 
ggsave("figs/Scatter plot matrix.png", Scatter_Matrix, width = 7, 
       height = 7, units = "in") 
Scatter_Matrix

# Correlations where r > 0.5 for droad:dcabin, dstreammouth:dfreshwater 
## Correlations where r > 0.3 but r < 0.5 for dlogging:droad  and dlogging:dcabin (but probably won't use logging)
## and rest are generally pretty low

### 7.interactions (coplots)----
# not sure quite how to do this yet
coplot(droad ~ dcabin |lat.pres*location, data = df1)
coplot(droad ~ dlogging |lat.pres*location, data = df1)
coplot(droad ~ dstreammouth |lat.pres*location, data = df1)


ggplot(df1, aes(droad, as.numeric(lat.pres), color=location)) +
  stat_smooth(method="glm", formula=y~x,
              alpha=0.2, size=2, aes(fill=location)) +
  geom_point(position=position_jitter(height=0.03, width=0)) +
  xlab("Dist to Road") + ylab("Pr (latrine)")


# Model-1 ----
m1 <- glm(lat.pres ~ droad + location + dlogging, 
          family = binomial(link = "logit"),
          data = df1)

test <- df1 |>
  filter(!is.na(latitude.x))
m1 <- glm(lat.pres ~ droad + dlogging, 
          family = binomial(link = "logit"),
          data = test)

#DHARMA
## https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html
## 2, 3, 8

## diagnostics ----
### 2 & 3 ----
## resids normal, homogeneous variance
# homogeneity/normality/linearity
m1_simres <- simulateResiduals(m1)
# str(bt_den.glmm1_simres,1)
plot(m1_simres)
# The normality is great; homogeneity OK

# but why the warnings - I tried to look at the plot function and found that you can see the resids with the below

# see resids
tmp <- residuals(m1_simres)
plot(density(tmp))

# the first plot is produced with the plotQQunif function and it uses Scaled residuals but I was not able to perfectly reproduce it with the gap::qqunif - however, the warnings are from plot2
plotQQunif(m1_simres)
gap::qqunif(m1_simres$scaledResiduals)

# the second plot which is made with "plotResiduals" and produces all the warnings - perahps because deviations 
plotResiduals(m1_simres)

tmp1 <- rank(m1_simres$fittedModel$fitted.values)
tmp2 <- scale(tmp1,center=min(tmp1),scale=diff(range(tmp1)))
plot(tmp2, m1_simres$scaledResiduals)


### 8 independence ----
### temporal independence
# No temporal independence as these are just use/non-use sites

### spatial independence
# recalculate resids with stations as the grouping variable
m1_simres_recalcSpace <- recalculateResiduals(m1_simres)

m1_simres_recalcSpace$scaledResiduals
# m1_simres_recalcSpace <- recalculateResiduals(m1_simres, group = as.factor(df1$type))

testSpatialAutocorrelation(m1_simres, x = unique(test$longitude.x), y = unique(test$latitude.x))

testSpatialAutocorrelation(m1_simres_recalcSpace, x = unique(df1$longitude.x), y = unique(df1$latitude.x))

testSpatialAutocorrelation(m1_simres_recalcSpace, x = unique(test$longitude.x), y = unique(test$latitude.x))


spatialAutoCorrBase_fun(bt.np, bt_den.glmm1_simres_recalcSpace)  
bt.np.density.all <- spatialData_join(bt.np.density.station[-4,], bt_den.glmm1_simres_recalcSpace, coords.np)

spatialAutoCorrGG_fun(bt.np.density.all)


### vif ----
#https://cran.r-project.org/web/packages/regressinator/vignettes/logistic-regression-diagnostics.html
# 5 (again) multicollinearity

vif(m1)



# Model Summary ----
summary(m1)
coef(m1)
fitted(m1)
predict(m1)
residuals(m1)
residuals(m1, type = "working")

library(ggeffects)

ggpredict(m1)
ggpredict(m1) |> plot()
ggpredict(m1, terms = "droad") |> plot()
ggpredict(m1, terms = "dlogging") |> plot()
ggpredict(m1, terms = "location") |> plot()
ggpredict(m1, terms = c("location", "droad")) |> plot()

# # # this is pretty useless as it always has been 
# par(mfrow=c(2,2))
# plot(m1) 
# graphics.off()
# 
# plot(predict(m1), resid(m1, type = "working"))
# 
# #https://sscc.wisc.edu/sscc/pubs/RegDiag-R/logistic-regression.html
# # linearity
# library(ggplot2)
# df1 |> 
#   mutate(comp_res = coef(m1)["droad"]*droad + residuals(m1, type = "working")) |> 
#   ggplot(aes(x = droad, y = comp_res)) +
#   geom_point() +
#   geom_smooth(color = "red", method = "lm", linetype = 2, se = F) +
#   geom_smooth(se = F)
# 
# 
# m2 <- glm(lat.pres ~ I(droad^3) + location, 
#           family = binomial(link = "logit"),
#           data = df1)
# summary(m2)
# 
# 
# # this didn't work at all
# df1 |> 
#   mutate(comp_res = coef(m2)["I(droad^3)"]*droad^3 + residuals(m1, type = "working")) |> 
#   ggplot(aes(x = droad^3, y = comp_res)) +
#   geom_point() +
#   geom_smooth(color = "red", method = "lm", linetype = 2, se = F) +
#   geom_smooth(se = F)
# 
# df1 |> 
#   mutate(comp_res = as.numeric(location) + residuals(m1, type = "working")) |> 
#   ggplot(aes(x = location, y = comp_res)) +
#   geom_point() +
#   geom_hline(yintercept = 0, color = "red") +
#   stat_summary(geom = "line", fun = mean, color = "blue", size = 1.5)




# Model-2 ----
library(glmmTMB)
m2 <- glmmTMB(lat.pres ~ droad + location + dlogging, 
          family = binomial(link = "logit"),
          data = df1)

# homogeneity/normality/linearity
m2_simres <- simulateResiduals(m2)
# str(bt_den.glmm1_simres,1)
plot(m2_simres)

# END ----