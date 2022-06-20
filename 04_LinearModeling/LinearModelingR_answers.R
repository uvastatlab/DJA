## CODE ALONG 1


# 1. Insert a code chunk below and model `log(TotalValue)` as function of `FullBath` and `FinSqFt.` Call your model `m3`

m3 <- lm(log(TotalValue) ~ FullBath + FinSqFt, data = homes)


# 2. Insert a code chunk below and check the Residuals vs Fitted plot

plot(m3, which = 1)


# 3. How do we interpret the FullBath coefficient?

round(exp(coef(m3)), 3)

# adding a full bath increases total value about 15% 

# 4. Insert a code chunk below and simulate data from the model and compare to the observed `TotalValue`. Does this look like a good model?
  
sim3 <- simulate(m3, nsim = 50)
plot(density(log(homes$TotalValue)))
for(i in 1:50)lines(density(sim2[[i]]), lty = 2, col = "grey80")



## CODE ALONG 2

# 1. Insert a code chunk below and model `log(TotalValue)` as function of `FullBath`, `FinSqFt` and `Cooling.` Call your model `m5`. 

m5 <- lm(log(TotalValue) ~ FullBath + FinSqFt + Cooling, data = homes)


# 2. What is the interpretation of Cooling? What is the baseline or reference level?
  
round(exp(coef(m5)), 4)

# having central air increases total value by about 26%



## CODE ALONG 3

# 1. Insert a code chunk below and model `log(TotalValue)` as function of `FullBath`, `FinSqFt`, `Cooling`, and the interaction of `FinSqFt` and `Cooling`. Call your model `m7`. Is the interaction warranted?
  
m7 <- lm(log(TotalValue) ~ FullBath + FinSqFt + Cooling + FinSqFt:Cooling, 
         data = homes)
anova(m7)


# 2. Visualize the interaction using the `ggpredict` function. Perhaps use `[1000:4000 by=500]` to set the range of `FinSqFt` on the x-axis.


plot(ggpredict(m7, terms = c("FinSqFt[1000:4000 by=500]", "Cooling")))


