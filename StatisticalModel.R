source("DataPreparation.R")
require(ggplot2)
require(sandwich)
require(msm)
# Load data
sm.data = get.data()


# Question 1: Are soccer referees more likely to give red cards to dark skin toned
#             players than light skin toned players?

summary(m1 <- glm(totalReds ~ rating + BMI, family="poisson", data=sm.data, offset=log(games)))

#for confidence interval
m_ci = confint(m1)

# test our model
chi.sq.m1 <- sum(residuals(m1, type = "pearson")^2) 
p.value.m1 <- pchisq(chi.sq.m1, m1$df.residual)
p.value.m1
sm.data$preds <- predict(m1, sm.data, type = 'response')
sm.data$preds

# plot predicitons against actual values with ratings
plot <- ggplot(sm.data, aes(x = rating, y = preds, colour = rating)) +
  geom_point(aes(y = totalReds), alpha=.5, position=position_jitter(h=.2)) +
  geom_line(size = 1) +
  labs(x = "Skin tone rating", y = "Predicted red card total")
# plot ratings against predictions
plot2 <- ggplot(sm.data, aes(x = rating, y = preds, colour = rating)) +
  geom_point() 

# plot totalReds against predictions of total reds
plot3 <- ggplot(sm.data, aes(x = totalReds, y = preds, colour = rating)) +
  geom_point() 

cov.m1 <- vcovHC(m1, type="HC0")
std.err.m1 <- sqrt(diag(cov.m1))
r.est.m1 <- cbind(Estimate= coef(m1), "Robust SE" = std.err.m1,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err.m1), lower.tail=FALSE),
               LL = coef(m1) - 1.96 * std.err.m1,
               UL = coef(m1) + 1.96 * std.err.m1)

# incident rate ratio
s.m1 <- deltamethod(list(~ exp(x1), ~ exp(x2), ~ exp(x3)), 
                 coef(m1), cov.m1)
s.m1

## exponentiate old estimates dropping the p values
rexp.est.m1 <- exp(r.est.m1[, -3])
## replace SEs with estimates for exponentiated coefficients
rexp.est.m1[, "Robust SE"] <- s.m1


# Question 2: Are soccer referees from countries high in skin-tone predjudice more likely to
#             award red cards to dark skin toned players?

summary(m2 <- glm(totalReds ~ rating + meanIAT *rating + meanExp *rating, family="poisson", data=sm.data, offset=log(games)))

# for confidence interval
m2_ci = confint(m2)

#testing our model
chi.sq.m2 <- sum(residuals(m2, type = "pearson")^2) 
p.value.m2 <- pchisq(chi.sq.m2, m2$df.residual)

cov.m2 <- vcovHC(m2, type="HC0")
std.err.m2 <- sqrt(diag(cov.m2))
r.est.m2 <- cbind(Estimate= coef(m2), "Robust SE" = std.err.m2,
                  "Pr(>|z|)" = 2 * pnorm(abs(coef(m2)/std.err.m2), lower.tail=FALSE),
                  LL = coef(m2) - 1.96 * std.err.m2,
                  UL = coef(m2) + 1.96 * std.err.m2)

s.m2 <- deltamethod(list(~ exp(x1), ~ exp(x2), ~ exp(x3), ~ exp(x4)), 
                 coef(m2), cov.m2)

## exponentiate old estimates dropping the p values
 rexp.est.m2 <- exp(r.est.m2[, -3])
##replace SEs with estimates for exponentiated coefficients
rexp.est.m2[, "Robust SE"] <- s.m2



