# Computation
##### Initialization
source("DMG_Init.R")
Datapath = "Output/"

##### DatFrame
# Import
dRegLog <- read.delim(paste0(Datapath, "dRegLog.txt"))
dRegLogGamb <- filter(dRegLog, Time == "AfterGambling")
dRegLogNeu <- filter(dRegLog, Time == "AfterNeutral")

##### Logistic Regressions
## With Time
RL <- glmer(Stay ~ PrReward*PrTransition*Time*Grp + (PrReward*PrTransition*Time|subjID),
            family = binomial, data = dRegLog, control = glmerControl(optimizer = "bobyqa"),
            nAGQ = 1)

summary(RL)
exp(coef(RL))
pred = predict(RL)

##### Evaluation
Conf_Mat <- ConfMat(d = dRegLog, pred = pred, DV = "Stay")
RL_Metric <- RLMetrics(Conf_Mat, pred = pred, T)

## Without Time
# After Gambling
RLGamb <- glmer(Stay ~ PrReward*PrTransition*Grp + (PrReward*PrTransition|subjID),
            family = binomial, data = dRegLogGamb, control = glmerControl(optimizer = "bobyqa"),
            nAGQ = 1)

summary(RLGamb)
exp(coef(RLGamb))
pred = predict(RLGamb)

# Evaluation
Conf_MatGamb <- ConfMat(d = dRegLogGamb, pred = predict(RLGamb), DV = "Stay")
RL_MetricGamb <- RLMetrics(Conf_MatGamb, pred = predict(RLGamb), T)

# After Neutral
RLNeu <- glmer(Stay ~ PrReward*PrTransition*Alc + (PrReward*PrTransition|subjID),
               family = binomial, data = dRegLogNeu, control = glmerControl(optimizer = "bobyqa"),
               nAGQ = 1)

summary(RLNeu)
exp(coef(RLNeu))
pred = predict(RLNeu)

# Evaluation
Conf_MatNeu <- ConfMat(d = dRegLogNeu, pred = predict(RLNeu), DV = "Stay")
RL_MetricNeu <- RLMetrics(Conf_MatNeu)
