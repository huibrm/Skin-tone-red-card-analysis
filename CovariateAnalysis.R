#source("ExploratoryAnalysis.R")
library(survival)

red.data = get.player.total.reds()

df = get.data()

View(summary(lm(df$totalReds ~ df$rating)))




ref.data = df %>% group_by(refID) %>% 
                  summarise(totalRedCards = sum(totalReds), averageSkinTone = mean(rating)) %>% 
                  filter(totalRedCards > 0)

plot(ref.data$averageSkinTone, ref.data$totalRedCards)

#proportion of dark skin tone to white tone

newdf = df %>% group_by(club) %>% mutate(avgSkinTone = mean(rating), skin.tone.difference.by.team = rating - avgSkinTone) %>% filter(totalReds >0)

plot(newdf$skin.tone.difference.by.team, newdf$totalReds)

t.test(newdf$skin.tone.difference.by.team, newdf$totalReds)

#not statistically significant
#if a ref tends to give red cards to darker skin toned players, it is absolute, not relative to the team.
#they will not target the darkest players on the 

pm <- glm(totalReds ~ playerPosition, family="poisson", data=red.data)

cov.pm <- vcovHC(pm, type="HC0")
std.err <- sqrt(diag(cov.pm))
r.est <- cbind(Estimate= coef(pm), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(pm)/std.err), lower.tail=FALSE),
               LL = coef(pm) - 1.96 * std.err,
               UL = coef(pm) + 1.96 * std.err)

r.est


# As it stands, this is essentially a test file. Once we get anything else we need out of it, it can be deleted.

