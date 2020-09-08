TG <- ToothGrowth

summary(TG)

OJ <- TG$len[TG$supp == 'OJ']
VC <- TG$len[TG$supp == 'VC']

mOJ <- mean(OJ)
vOJ <- var(OJ)
nOJ <- length(OJ)
OJ_confidence_interval <- mOJ + c(-1, 1)*qnorm(0.975)*sqrt(vOJ/nOJ)

mVC <- mean(VC)
vVC <- var(VC)
nVC <- length(VC)
VC_confidence_interval <- mVC + c(-1, 1)*qnorm(0.975)*sqrt(vVC/nVC)

pooled_variance <- sqrt(vOJ/nOJ + vVC/nVC)
difference_confidence_interval <- mOJ - mVC + c(-1, 1)*qnorm(0.975)*pooled_variance
difference_confidence_interval_90 <- mOJ - mVC + c(-1, 1)*qnorm(0.95)*pooled_variance

mns <- NULL
vars <- NULL
lens <- NULL
for (i in levels(as.factor(TG$dose))) {
    mns <- c(mns, mean(TG$len[TG$dose == i]))
    vars <- c(vars, var(TG$len[TG$dose == i]))
    lens <- c(lens, length(TG$len[TG$dose == i]))
    
}

confidence_interval_05 <- mns[1] + c(-1, 1)*qnorm(0.975)*sqrt(vars[1]/lens[1])
confidence_interval_1 <- mns[2] + c(-1, 1)*qnorm(0.975)*sqrt(vars[2]/lens[2])
confidence_interval_2 <- mns[3] + c(-1, 1)*qnorm(0.975)*sqrt(vars[3]/lens[3])
difference_confidence_interval_05_1 <- mns[2] - mns[1] + 
    c(-1, 1)*qnorm(0.975)*sqrt(vars[1]/lens[1] + vars[2]/lens[2])
difference_confidence_interval_1_2 <- mns[3] - mns[2] + 
    c(-1, 1)*qnorm(0.975)*sqrt(vars[2]/lens[2] + vars[3]/lens[3])
difference_confidence_interval_05_2 <- mns[3] - mns[1] + 
    c(-1, 1)*qnorm(0.975)*sqrt(vars[1]/lens[1] + vars[3]/lens[3])
