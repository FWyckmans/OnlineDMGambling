################################################ Mixed Regression
# Initialization
source("DMG_Init.R")

Datapath = "Output/"
Output_path = "Output/"

OutliersRemove = F

############## DataFrame
##### Import
d <- read.delim(paste0(Datapath, "dTotFE.txt"))

# Remove participants who bombed
ToRem = c()
d <- d%>%
  filter(!NS %in% ToRem)

# d$ICJE[is.na(d$ICJE)] <- 0
# d <- filter(d, !is.na(ICJE))

############## Analyses
##### w analyses
# Long df
ImpCol = c("NS", "ICJE", "Grp", "GrpC", "Grp3", "Grp3C")

dw <- GatherParam(d, "w", "Ind_Craving", ImpCol)

# Data cleaning
dw <- OutliersScale(dw, c("Ind_Craving", "w", "ICJE"), OutRem = OutliersRemove)

# Analyses
RL <- lmer(zw ~ Ind_Craving*Grp + (1|NS),
           data = dw, control = lmerControl(check.nobs.vs.nRE = 'ignore'))

summary(RL)

RLlme <- lme(zw ~ Ind_Craving*Grp, random = ~ 1 + TypeVidC|NS,
           data = dw)
summary(RLlme)

# Graphic
Inter_w <- interact_plot(RL, pred = Ind_Craving, modx = Grp, plot.points = T,
                         interval = T,
                         x.label = "Craving (z-score)",
                         y.label = "ω (z-score)",
                         # modx.values = 'plus-minus',
                         modx.labels = c("PG", "G"),
                         legend.main = "Grp",
                         colors = "Qual1")
Inter_w

plot(RL)
qqnorm(resid(RL))
qqline(resid(RL))

##### w MIXED ANOVA  # Does not seem to work
# Long df
ImpCol = c("NS", "ICJE", "Grp", "GrpC", "Grp3", "Grp3C")

dw <- GatherParam(d, "w", "dCraving", ImpCol)
mM <- aov(w ~ TypeVid*Grp3 + Error(NS/TypeVid), data = dw)
summary(mM)

##### w analyses only PG
# Long df
ImpCol = c("NS", "ICJE", "Grp", "GrpC", "Grp3", "Grp3C")

dw <- GatherParam(d, "w", "dCraving", ImpCol)
dw <- filter(dw, Grp == "PG")

# Data cleaning
dw <- OutliersScale(dw, c("dCraving", "w"), OutRem = OutliersRemove)

# Analyses
RL <- lmer(zw ~ zdCraving + (1|TypeVid),
           data = dw)
summary(RL)

# Graphic
Inter_w <- interact_plot(RL, pred = zdCraving, modx = GrpC, plot.points = T,
                         interval = T,
                         x.label = "Craving (z-score)",
                         y.label = "ω (z-score)",
                         modx.labels = c("PG", "HC"),
                         legend.main = "Grp",
                         colors = "Qual1")
Inter_w

plot(RL)
qqnorm(resid(RL))
qqline(resid(RL))

##### w analyses only HC who gamble a bit
# Long df
ImpCol = c("NS", "ICJE", "Grp", "GrpC", "Grp3", "Grp3C")

dw <- GatherParam(d, "w", "dCraving", ImpCol)
dw <- filter(dw, Grp == "HC")
dw <- filter(dw, !is.na(dw$ICJE))

# Data cleaning
dw <- OutliersScale(dw, c("dCraving", "w"), OutRem = OutliersRemove)

# Analyses
RL <- lmer(zw ~ zdCraving + (1|TypeVid),
           data = dw)
summary(RL)

# Graphic
Inter_w <- interact_plot(RL, pred = zdCraving, modx = GrpC, plot.points = T,
                         interval = T,
                         x.label = "Craving (z-score)",
                         y.label = "ω (z-score)",
                         modx.labels = c("PG", "HC"),
                         legend.main = "Grp",
                         colors = "Qual1")
Inter_w

plot(RL)
qqnorm(resid(RL))
qqline(resid(RL))