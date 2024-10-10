source("otter_data.R")

#libraries ----
library(ggplot2) 
library(GGally) 
library(car)

# Latrine Presence ----
## EDA ----
# outliers x and y
ggplot(df_latT, aes(site.location, droad)) + 
  geom_violin() + 
  facet_grid(~latrine.present)


ggplot(df_latPlot, aes(name, value, fill = name)) + geom_violin() + facet_grid(site.location ~ latrine.present)

ggplot(df_latPlot, aes(name, value, fill = name)) + geom_boxplot() + facet_grid(site.location ~ latrine.present)

df_latPlot |>
  filter(site.location == "Terra Nova") |>
  ggplot(aes(name, value, fill = name)) + geom_violin() + facet_grid(~ latrine.present)

# Cleavland dot plots
ggplot(df_latT, aes(droad, y = seq(1, length(droad),1), fill = site.location, colour = site.location)) + geom_point() + facet_grid(~latrine.present)


# zero trouble y
plot(density)

# collinearity x/relationshiop y/x
Scatter_Matrix <- ggpairs(df_latT,columns = c(2:7, 9), 
                          title = "Scatter Plot Matrix for latrine Dataset", 
                          axisLabels = "show") 
ggsave("figs/Scatter plot matrix.png", Scatter_Matrix, width = 7, 
       height = 7, units = "in") 
Scatter_Matrix

# interactions

## Confirmatory ----
m1 <- glm(lat.pres ~ droad + site.location + dlogging, 
          family = binomial(link = "logit"),
          data = df_latT)
summary(m1)
coef(m1)
fitted(m1)
predict(m1)
residuals(m1)
residuals(m1, type = "working")

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
# df_latT |> 
#   mutate(comp_res = coef(m1)["droad"]*droad + residuals(m1, type = "working")) |> 
#   ggplot(aes(x = droad, y = comp_res)) +
#   geom_point() +
#   geom_smooth(color = "red", method = "lm", linetype = 2, se = F) +
#   geom_smooth(se = F)
# 
# 
# m2 <- glm(lat.pres ~ I(droad^3) + site.location, 
#           family = binomial(link = "logit"),
#           data = df_latT)
# summary(m2)
# 
# 
# # this didn't work at all
# df_latT |> 
#   mutate(comp_res = coef(m2)["I(droad^3)"]*droad^3 + residuals(m1, type = "working")) |> 
#   ggplot(aes(x = droad^3, y = comp_res)) +
#   geom_point() +
#   geom_smooth(color = "red", method = "lm", linetype = 2, se = F) +
#   geom_smooth(se = F)
# 
# df_latT |> 
#   mutate(comp_res = as.numeric(site.location) + residuals(m1, type = "working")) |> 
#   ggplot(aes(x = site.location, y = comp_res)) +
#   geom_point() +
#   geom_hline(yintercept = 0, color = "red") +
#   stat_summary(geom = "line", fun = mean, color = "blue", size = 1.5)




#https://cran.r-project.org/web/packages/regressinator/vignettes/logistic-regression-diagnostics.html
# multicollinearity




vif(m1)


# outliers



#DHARMA
## https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html

## diagnostics ----
library(DHARMa)
# homogeneity/normality/linearity
m1_simres <- simulateResiduals(m1)
# str(bt_den.glmm1_simres,1)
plot(m1_simres)
# The normality is great; homogeneity OK

# independence
### temporal independence
# No temporal independence as these are just use/non-use sites

### spatial independence
# join coordinates with data set - may not be required
#bt.np <- left_join(bt.np, coords, by = c("Station_new" = "Station"))
#str(bt.np)
#bt.np[,c(2, 4, 6, 7, 11:13, 17, 21, 22)]

# just coords for non-pool
# coords.np <- as.data.frame(coords[c(1:4, 7:8, 10, 12:13, 15:18) ,]) 
# nrow(coords.np)

# recalculate resids with stations as the grouping variable
bt_den.glmm1_simres_recalcSpace <- recalculateResiduals(bt_den.glmm1_simres, group = as.factor(bt.np$Station_new))
unique(bt.np$Station_new) # OK - there are only 13 values in this because this is no pools and there are 4 pools + 1 destroyed pool so 18-5=13.
#str(bt_den.glmm1_simres_recalcSpace)

testSpatialAutocorrelation(bt_den.glmm1_simres_recalcSpace, x = unique(bt.np$X), y = unique(bt.np$Y))

spatialAutoCorrBase_fun(bt.np, bt_den.glmm1_simres_recalcSpace)  
bt.np.density.all <- spatialData_join(bt.np.density.station[-4,], bt_den.glmm1_simres_recalcSpace, coords.np)

spatialAutoCorrGG_fun(bt.np.density.all)

