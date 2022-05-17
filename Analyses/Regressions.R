################################################ Linear Regression
# Initialization
source("DMG_Init.R")

Datapath = "Output/"
Output_path = "Output/"

OutliersRemove = F

############## DataFrame
##### Import
d <- read.delim(paste0(Datapath, "dFINAL.txt"))

##### w analyses
# Data cleaning
Dw <- OutliersScale(d, c("dIndCraving", "dw"), OutRem = OutliersRemove)

# Analyses
LinReg <- lm(zdw ~ zdIndCraving*GrpC, data = Dw)
summary(LinReg)

# Graphic
Inter_w <- interact_plot(LinReg, pred = zdIndCraving, modx = GrpC, plot.points = T,
                         interval = T,
                         x.label = "Craving (z-score)",
                         y.label = "ω (z-score)",
                         modx.labels = c("PG", "HC"),
                         legend.main = "Grp",
                         colors = "Qual1")
Inter_w

##### MBc analyses
# Data cleaning
DMBc <- OutliersScale(d, c("dCraving", "dMBc"), OutRem = OutliersRemove)

# Analyses
LinReg <- lm(zdMBc ~ zdCraving*GrpC, data = DMBc)
summary(LinReg)

# Graphic
Inter_MBc <- interact_plot(LinReg, pred = zdCraving, modx = GrpC, plot.points = T,
                         interval = T,
                         x.label = "Craving (z-score)",
                         y.label = "MB (z-score)",
                         modx.labels = c("PG", "HC"),
                         legend.main = "Grp",
                         colors = "Qual1")
Inter_MBc

##### MFc analyses
# Data cleaning
DMFc <- OutliersScale(d, c("dCraving", "dMFc"), OutRem = OutliersRemove)

# Analyses
LinReg <- lm(zdMFc ~ zdCraving*GrpC, data = DMFc)
summary(LinReg)

# Graphic
Inter_MFc <- interact_plot(LinReg, pred = zdCraving, modx = GrpC, plot.points = T,
                         interval = T,
                         x.label = "Craving (z-score)",
                         y.label = "MF (z-score)",
                         modx.labels = c("PG", "HC"),
                         legend.main = "Grp",
                         colors = "Qual1")
Inter_MFc

##### beta1 analyses
# Data cleaning
Dbeta1 <- OutliersScale(d, c("dCraving", "dbeta1"), OutRem = OutliersRemove)

# Analyses
LinReg <- lm(zdbeta1 ~ zdCraving*GrpC, data = Dbeta1)
summary(LinReg)

# Graphic
Inter_beta1 <- interact_plot(LinReg, pred = zdCraving, modx = GrpC, plot.points = T,
                         interval = T,
                         x.label = "Craving (z-score)",
                         y.label = "beta1 (z-score)",
                         modx.labels = c("PG", "HC"),
                         legend.main = "Grp",
                         colors = "Qual1")
Inter_beta1

##### beta2 analyses
# Data cleaning
Dbeta2 <- OutliersScale(d, c("dCraving", "dbeta2"), OutRem = OutliersRemove)

# Analyses
LinReg <- lm(zdbeta2 ~ zdCraving*GrpC, data = Dbeta2)
summary(LinReg)

# Graphic
Inter_beta2 <- interact_plot(LinReg, pred = zdCraving, modx = GrpC, plot.points = T,
                         interval = T,
                         x.label = "Craving (z-score)",
                         y.label = "beta2 (z-score)",
                         modx.labels = c("PG", "HC"),
                         legend.main = "Grp",
                         colors = "Qual1")
Inter_beta2

##### a1 analyses
# Data cleaning
Da1 <- OutliersScale(d, c("dCraving", "da1"), OutRem = OutliersRemove)

# Analyses
LinReg <- lm(zda1 ~ zdCraving*GrpC, data = Da1)
summary(LinReg)

# Graphic
Inter_a1 <- interact_plot(LinReg, pred = zdCraving, modx = GrpC, plot.points = T,
                             interval = T,
                             x.label = "Craving (z-score)",
                             y.label = "a1 (z-score)",
                             modx.labels = c("PG", "HC"),
                             legend.main = "Grp",
                             colors = "Qual1")
Inter_a1

##### a2 analyses
# Data cleaning
Da2 <- OutliersScale(d, c("dCraving", "da2"), OutRem = OutliersRemove)

# Analyses
LinReg <- lm(zda2 ~ zdCraving*GrpC, data = Da2)
summary(LinReg)

# Graphic
Inter_a2 <- interact_plot(LinReg, pred = zdCraving, modx = GrpC, plot.points = T,
                             interval = T,
                             x.label = "Craving (z-score)",
                             y.label = "a2 (z-score)",
                             modx.labels = c("PG", "HC"),
                             legend.main = "Grp",
                             colors = "Qual1")
Inter_a2

##### lambda analyses
# Data cleaning
Dlambda <- OutliersScale(d, c("dCraving", "dlambda"), OutRem = OutliersRemove)

# Analyses
LinReg <- lm(zdlambda ~ zdCraving*GrpC, data = Dlambda)
summary(LinReg)

# Graphic
Inter_lambda <- interact_plot(LinReg, pred = zdCraving, modx = GrpC, plot.points = T,
                             interval = T,
                             x.label = "Craving (z-score)",
                             y.label = "lambda (z-score)",
                             modx.labels = c("PG", "HC"),
                             legend.main = "Grp",
                             colors = "Qual1")
Inter_lambda

##### pi analyses
# Data cleaning
Dpi <- OutliersScale(d, c("dCraving", "dpi"), OutRem = OutliersRemove)

# Analyses
LinReg <- lm(zdpi ~ zdCraving*GrpC, data = Dpi)
summary(LinReg)

# Graphic
Inter_pi <- interact_plot(LinReg, pred = zdCraving, modx = GrpC, plot.points = T,
                              interval = T,
                              x.label = "Craving (z-score)",
                              y.label = "pi (z-score)",
                              modx.labels = c("PG", "HC"),
                              legend.main = "Grp",
                              colors = "Qual1")
Inter_pi



