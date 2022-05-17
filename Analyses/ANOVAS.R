# Computation
##### Initialization
source("DMG_Init.R")
Datapath = "Output/"

##### DatFrame
# Import
d <- read.delim(paste0(Datapath, "dTot.txt"))

# Prepare for mixed effect
dL <- gather(d, key = "Measure", val = "Value", c(MB_AfterNeutr, MB_AfterGamb, MF_AfterNeutr, MF_AfterGamb))
dLMB = filter(dL, (Measure == "MB_AfterNeutr" | Measure == "MB_AfterGamb"))
dLMB <- AddDummyCol(dLMB, c("MeasureC", "SampleC"), 1)
dLMB$MeasureC[dLMB$Measure == "MB_AfterGamb"] = -1
dLMB$SampleC[dLMB$Grp == "PG"] = -1

dLMF = filter(dL, (Measure == "MF_AfterNeutr" | Measure == "MF_AfterGamb"))
dLMF <- AddDummyCol(dLMF, c("MeasureC", "SampleC"), 1)
dLMF$MeasureC[dLMF$Measure == "MF_AfterGamb"] = -1
dLMF$SampleC[dLMF$Grp == "PG"] = -1

############## ANOVA
# https://www.datanovia.com/en/fr/lessons/anova-mixte-dans-r/#two-way-mixed
##### MB
dLMB$Measure <- factor(dLMB$Measure)
dLMB$Grp <- factor(dLMB$Grp)

# Visu
bxp <- ggboxplot(
  dLMB, x = "Measure", y = "Value",
  color = "Grp", palette = "jco"
)
bxp

# Normality
dLMB %>%
  group_by(Measure, Grp) %>%
  shapiro_test(Value)

ggqqplot(dLMB, "Value", ggtheme = theme_bw()) +
  facet_grid(Measure ~ Grp)

# Homogeneity of variances
dLMB %>%
  group_by(Measure) %>%
  levene_test(Value ~ Grp)

# Homogeneity of covariance
box_m(dLMB[, "Value", drop = FALSE], dLMB$Grp)

# ANOVA
res.aov <- anova_test(
  data = dLMB, dv = Value, wid = NS,
  between = Grp, within = Measure
)
get_anova_table(res.aov)

one.way <- dLMB %>%
  group_by(Measure) %>%
  anova_test(dv = Value, wid = NS, between = Grp) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

one.way2 <- dLMB %>%
  group_by(Grp) %>%
  anova_test(dv = Value, wid = NS, within = Measure) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way2

##### MF
dLMF$Measure <- factor(dLMF$Measure)
dLMF$Grp <- factor(dLMF$Grp)

# Visu
bxp <- ggboxplot(
  dLMF, x = "Measure", y = "Value",
  color = "Grp", palette = "jco"
)
bxp

# Normality
dLMF %>%
  group_by(Measure, Grp) %>%
  shapiro_test(Value)

ggqqplot(dLMF, "Value", ggtheme = theme_bw()) +
  facet_grid(Measure ~ Grp)

# Homogeneity of variances
dLMF %>%
  group_by(Measure) %>%
  levene_test(Value ~ Grp)

# Homogeneity of covariance
box_m(dLMF[, "Value", drop = FALSE], dLMF$Grp)

# ANOVA
res.aov <- anova_test(
  data = dLMF, dv = Value, wid = NS,
  between = Grp, within = Measure
)
get_anova_table(res.aov)

one.way <- dLMF %>%
  group_by(Measure) %>%
  anova_test(dv = Value, wid = NS, between = Grp) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

one.way2 <- dLMF %>%
  group_by(Grp) %>%
  anova_test(dv = Value, wid = NS, within = Measure) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way2
