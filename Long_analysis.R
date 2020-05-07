long <- read.table(file="/Users/rodolphethiebaut/Dropbox/_Biblio/Coronavirus/Immunology/Long/Long_data.txt",header=TRUE,sep="\t")
require(dplyr)
require(ggplot2)
require(lme4)
require(lmerTest)

long <- mutate(long, logIgG=log2(IgG), logIgM=log2(IgM),T=Days, T2=Days*Days/100)

long %>% ggplot(aes(logIgG)) + geom_histogram()
long %>% ggplot(aes(logIgM)) + geom_histogram()
long %>% ggplot(aes(Days)) + geom_histogram()

m <- lmer(logIgG ~ T + T2 + (1|ID)+(0+T|ID)+(0+T2|ID), long, REML = 0)
m <- lmer(logIgG ~ T + T2 + (1|ID)+(0+T2|ID), long, REML = 0)
m <- lmer(logIgG ~ T + T2 + (1|ID), long, REML = 0)
summary(m)

mm <- lmer(logIgM ~ T + T2 + (1|ID)+(0+T|ID)+(0+T2|ID), long, REML = 0)
mm <- lmer(logIgM ~ T + T2 + (1|ID)+(0+T|ID), long, REML = 0)
summary(mm)

confint(m,level=0.95,method=c("profile"))

pred <- predict(mm)

ggplot(long, aes(x=Days, y=logIgG)) +
  geom_point(aes())+
  geom_line(aes(group=ID), alpha=0.5, linetype=3) +
  geom_line(aes(y = pred),size=1) +
  facet_wrap(~ID)

ggplot(long, aes(x=Days, y=logIgM)) +
  geom_point(aes())+
  geom_line(aes(group=ID), alpha=0.5, linetype=3) +
  geom_line(aes(y = pred),size=1) +
  facet_wrap(~ID)


hist(residuals(m, type=c("pearson")))
qqnorm(residuals(m, type=c("pearson")))
ggplot(aes(x=residuals(m))) + theme_classic() +  geom_histogram(colour="black", fill="grey")

require(ciTools)
new_dat <- long[sample(NROW(long), 20),]
new_dat <- add_ci(new_dat, m, alpha = 0.5) %>%
  add_pi(m, alpha = 0.5)

m <- lmer(logIgG ~ Age+Age:T+T + Age:T2+T2 + (1|ID)+(0+T|ID)+(0+T2|ID), long, REML = 0)
m <- lmer(logIgG ~ Male+Male:T+T + Male:T2+T2 + (1|ID)+(0+T|ID)+(0+T2|ID), long, REML = 0)

mm <- lmer(logIgM ~ Age+Age:T+T + Age:T2+T2 + (1|ID)+(0+T|ID), long, REML = 0)
mm <- lmer(logIgM ~ Male+Male:T+T + Male:T2+T2 + (1|ID)+(0+T|ID), long, REML = 0)

