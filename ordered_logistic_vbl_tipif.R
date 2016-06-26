library(plyr) #aplicar función revalue
library(nnet)
install.packages("mlogit")
library(mlogit)
install.packages("rms")
library(rms)
library(MASS)
library(Hmisc)

# BASE DE DATOS

Data.csv <- read.csv('fp6-final_2.0.csv', header= T , sep=";" , dec=",")

attach(Data.csv)

# LIMPIAR VARIABLES INCOMPLETAS

Data.csv$HRST04_13 <- NULL
Data.csv$RDPersonn00_13 <- NULL

# TIPIFICAR AS VARIABLES

Data.csv$PIB_11_tipf <- (GDP_11 - phi_PIB_11)/sd_PIB_11
phi_PIB_11 <- mean(GDP_11)
sd_PIB_11 <- sd(GDP_11)

Data.csv$fp6_tipf <- (Data.csv$num_fp6 - phi_fp6)/sd_fp6
phi_fp6 <- mean(Data.csv$num_fp6)
sd_fp6 <- sd(Data.csv$num_fp6)

Data.csv$Int_life_tipf <- (Data.csv$INTERCULTURAL_LIFE_YES - phi_Int_life)/sd_Int_life
phi_Int_life <- mean(Data.csv$INTERCULTURAL_LIFE_YES)
sd_Int_life<- sd(Data.csv$INTERCULTURAL_LIFE_YES)

Data.csv$IDPIB_11_tipf <- (RDGDP_11 - phi_IDPIB_11)/sd_IDPIB_11
phi_IDPIB_11 <- mean(RDGDP_11)
sd_IDPIB_11 <- sd(RDGDP_11)

Data.csv$MulAc_tipf <- (MulAC06 - phi_MulAc)/sd_MulAc
phi_MulAc <- mean(MulAC06)
sd_MulAc <- sd(MulAC06)

Data.csv$RIS_ID_NEG_tipf <- (RIS_RD_BUS - phi_RIS_ID_NEG)/sd_RIS_ID_NEG
phi_RIS_ID_NEG <- mean(RIS_RD_BUS)
sd_RIS_ID_NEG <- sd(RIS_RD_BUS)

Data.csv$RIS_ID_PUB_tipf <- (RIS_RD_PUB - phi_RIS_ID_PUB)/sd_RIS_ID_PUB
phi_RIS_ID_PUB <- mean(RIS_RD_PUB)
sd_RIS_ID_PUB <- sd(RIS_RD_PUB)

# ESTIMACIÓN REGRESIÓN GDPpc COMO DEPENDENTE

reg_GDPpc <- lm(PIB_11_tipf ~ IDPIB_11_tipf + MulAc_tipf + fp6_tipf + Int_life_tipf, data=Data.csv, model=T)
reg_GDPpc.2 <- lm(PIB_11_tipf ~ MulAc_tipf + fp6_tipf + Int_life_tipf, data=Data.csv, model=T)
reg_GDPpc.3 <- lm(PIB_11_tipf ~ IDPIB_11_tipf+ fp6_tipf + Int_life_tipf, data=Data.csv, model=T)
summary(reg_GDPpc)
summary(reg_GDPpc.2)
summary(reg_GDPpc.3)
# CORRECIÓN HETEROCEDASTICIDADE WHITE
install.packages("lmSupport")
library("lmSupport")
modelCorrectSE(reg_GDPpc)
modelCorrectSE(reg_GDPpc.2)
modelCorrectSE(reg_GDPpc.3)
# ESTIMACIÓN MODELO LOGIT ORDENADO:


est.polr_1<- polr(ris_typo_fa ~ IDPIB_11_tipf + MulAc_tipf + fp6_tipf + Int_life_tipf, data = Data.csv, Hess=TRUE)
est.polr_2<- polr(ris_typo_fa ~ fp6_tipf + MulAc_tipf + Int_life_tipf, data = Data.csv, Hess=TRUE)
est.polr_3<- polr(ris_typo_fa ~ IDPIB_11_tipf + fp6_tipf + Int_life_tipf, data = Data.csv, Hess=TRUE)
## store table
ctable_1 <- coef(summary(est.polr_1))
ctable_2 <- coef(summary(est.polr_2))
ctable_3 <- coef(summary(est.polr_3))
## calculate and store p values
p_1 <- pnorm(abs(ctable_1[, "t value"]), lower.tail = FALSE) * 2
p_2 <- pnorm(abs(ctable_2[, "t value"]), lower.tail = FALSE) * 2
p_3 <- pnorm(abs(ctable_3[, "t value"]), lower.tail = FALSE) * 2
## combined table
ctable.p_1 <- cbind(ctable_1, "p value" = p_1)
ctable.p_2 <- cbind(ctable_2, "p value" = p_2)
ctable.p_3 <- cbind(ctable_3, "p value" = p_3)

# Odds ratio e intervalos de confianza:

ci_plr_1 <- confint(est.polr_1)
OR_plr_1 <- exp(cbind(OR = coef(est.polr_1), ci_plr_1))

ci_plr_2 <- confint(est.polr_2)
OR_plr_2 <- exp(cbind(OR = coef(est.polr_2), ci_plr_2))

ci_plr_3 <- confint(est.polr_3)
OR_plr_3 <- exp(cbind(OR = coef(est.polr_3), ci_plr_3))


# ESTIMACIÓN DO MODELO CON VARIABLE DEPENDENTE PATENTES:


# Creación dunha variable en niveis a partir da variable continua Pat
Data.csv$pat_09_12 <- cut(Data.csv$Pat_Media_09_12,
                          breaks=c(-Inf,87.009, 174.018, 261.027,Inf),
                          labels=c(1,2,3, 4))

# Creación dunha variable en niveis a partir da variable continua Pat_2012
Data.csv$pat_2012 <- cut(Data.csv$Pat_2012,
                         breaks=c(-Inf,0.306, 1.771, 9.998,Inf),
                         labels=c(1,2,3, 4))

# Creación dunha variable en niveis a partir da variable continua Pat_2009
Data.csv$pat_2009 <- cut(Data.csv$Pat_2009,
                         breaks=c(-Inf,108.875, 217.75, 326.625,Inf),
                         labels=c(1,2,3, 4))
# Creación dunha variable en niveis a partir da variable continua Pat_2009 
# seguindo a distribución de cuartiles da variable Pat_2009
Data.csv$pat_2009 <- cut(Data.csv$Pat_2009,
                         breaks=c(-Inf,1.208, 5.440, 26.820,Inf),
                         labels=c(1,2,3, 4))

# Creación dunha variable en niveis a partir da variable continua Pat_RIS

Data.csv$pat_RIS_PAT_AP <- cut(Data.csv$RIS_PAT_AP,
                               breaks=c(-Inf,0.19475, 0.3895, 0.58425,Inf),
                               labels=c(1,2,3, 4))

# MODELO CON VARIABLE DEPENDENTE PATENTES:

est.polr_pat1 <- polr(pat_2009 ~ PIB_11_tipf + DumCapital + IDPIB_11_tipf + MulAc_tipf + fp6_tipf + Int_life_tipf, data = Data.csv, Hess=TRUE)
est.polr_pat2 <- polr(pat_2009 ~ fp6_tipf+ MulAc_tipf + Int_life_tipf, data = Data.csv, Hess=TRUE)
est.polr_pat3 <- polr(pat_2009 ~ IDPIB_11_tipf + fp6_tipf + Int_life_tipf, data = Data.csv, Hess=TRUE)
## store table
ctable_pat1 <- coef(summary(est.polr_pat1))
ctable_pat2 <- coef(summary(est.polr_pat2))
## calculate and store p values
p_pat1 <- pnorm(abs(ctable_pat1[, "t value"]), lower.tail = FALSE) * 2
p_pat2 <- pnorm(abs(ctable_pat2[, "t value"]), lower.tail = FALSE) * 2
## combined table
ctable.p_pat1 <- cbind(ctable_pat1, "p value" = p_pat1)
ctable.p_pat2 <- cbind(ctable_pat2, "p value" = p_pat2)

# Odds ratio e intervalos de confianza:

ci_plr_pat1 <- confint(est.polr_pat1)
OR_plr_pat1 <- exp(cbind(OR = coef(est.polr_pat1), ci_plr_pat1))

ci_plr_pat2 <- confint(est.polr_pat2)
OR_plr_pat2 <- exp(cbind(OR = coef(est.polr_pat2), ci_plr_pat2))


# MODELO CON VARIABLE DEPENDENTE PATENTES 2012:

est.polr_pat3 <- polr(pat_2012 ~ IDPIB_11_tipf + MulAc_tipf + fp6_tipf + Int_life_tipf, data = Data.csv, Hess=TRUE)
est.polr_pat4 <- polr(pat_2012 ~ fp6_tipf+ MulAc_tipf + Int_life_tipf, data = Data.csv, Hess=TRUE)
est.polr_pat5 <- polr(pat_2012 ~ IDPIB_11_tipf + fp6_tipf + Int_life_tipf, data = Data.csv, Hess=TRUE)
## store table
ctable_pat3 <- coef(summary(est.polr_pat3))
ctable_pat4 <- coef(summary(est.polr_pat4))
ctable_pat5 <- coef(summary(est.polr_pat5))
## calculate and store p values
p_pat3 <- pnorm(abs(ctable_pat3[, "t value"]), lower.tail = FALSE) * 2
p_pat4 <- pnorm(abs(ctable_pat4[, "t value"]), lower.tail = FALSE) * 2
p_pat5 <- pnorm(abs(ctable_pat5[, "t value"]), lower.tail = FALSE) * 2
## combined table
ctable.p_pat3 <- cbind(ctable_pat3, "p value" = p_pat3)
ctable.p_pat4 <- cbind(ctable_pat4, "p value" = p_pat4)
ctable.p_pat5 <- cbind(ctable_pat5, "p value" = p_pat5)
# Odds ratio e intervalos de confianza:

ci_plr_pat3 <- confint(est.polr_pat3)
OR_plr_pat3 <- exp(cbind(OR = coef(est.polr_pat3), ci_plr_pat3))

ci_plr_pat4 <- confint(est.polr_pat4)
OR_plr_pat4 <- exp(cbind(OR = coef(est.polr_pat4), ci_plr_pat4))

ci_plr_pat5 <- confint(est.polr_pat5)
OR_plr_pat5 <- exp(cbind(OR = coef(est.polr_pat5), ci_plr_pat5))

# PREDICIÓN DOS ACERTADOS:

sf <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)),
    'Y>=4' = qlogis(mean(y >= 4)))
}

s <- with(Data.csv, summary(as.numeric(ris_typo_fa) ~ IDPIB_11_tipf + MulAc_tipf + fp6_tipf + Int_life_tipf, fun=sf))



# CALCULO DE CORRELACIÓNS:

x <-cbind(Data.csv$PIB_11_tipf,  
          Data.csv$pat_2012,
          Data.csv$ris_typo_fa,
          Data.csv$IDPIB_11_tipf, 
          Data.csv$MulAc_tipf, 
          Data.csv$Int_life_tipf, 
          Data.csv$fp6_tipf)

cor(x, use= "complete")