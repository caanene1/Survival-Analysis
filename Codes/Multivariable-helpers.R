# Required packages
library(survival)
library(forestmodel)
library(survminer)
library(prodlim)

## Compelet surival analysis, models, plots and assumption checks ##
# Multivariate surival function
AmultiSurv <- function(x) {
# Rename columns
colnames(x)[1:2] <- c("time", "cens")
# Formular
forma <- as.formula(paste("Surv(time, cens) ~ ",
                          paste(unique(colnames(x[-c(1,2)])), collapse = " + ")))
#
mcox <- coxph(forma, x)
return(mcox)
}
#
# Usage 
cmod <- AmultiSurv(foo)
summary(cmod)
# Insert scores as linear predictor for each case
foo$Score <- predict(cmod, type = "lp")
# Extract coef for other uses
cTable <- as.data.frame(cbind(cmod$coef))
names(cTable) <- "Coef"
#

# Testing assumptions and printing HR 
# Unified function
ANENEFOHR <- function(model) {
# Forest HR
fm <- forest_model(model, 
format_options = list(colour = "darkblue",
shape = 20, text_size = 3, banded = TRUE))
# 
# Testing assumption of proportional hazards
phTest <- cox.zph(model)
# If not significant for each variable then PH assumption is meet, 
# else use variable*time for affected variables to reconstruct the model
# 
# Testing influential observations
IO <- ggcoxdiagnostics(model, type = "deviance", 
                 ox.scale = "observation.id", 
                 hline = TRUE, point.col = "navy",
                 point.size = 0.2, point.shape = 4, 
                 ggtheme = theme_bw(15))
# If symmetric i.e around 0 then non influential observation,
# else consider adjusting variables and cases, must use scaled variables
Res <- list("FM" = fm, "PHtest" = phTest, "IOtest" = IO)
return(Res)
}
# Usage
DD <- ANENEFOHR(cmod)
DD$FM
DD$PHtest
DD$IOtest

# Ploting kp
aKP <- function(KPobj) {
  plot(KPobj,
       confint.citype = "shadow",
       confInt.density = 12,
       confint.lwd = 1,
       lwd = 1.5,
       atrisk.labels = paste("", c("High","Low"),": "),
       col = c("red", "black"),
       atrisk.title = "At Risk",
       xlab = "Survival Months....",  # label for x-axis
       legend.x = "bottomleft", # positition of legend
       legend.cex = 0.85, # font size of legend
       legend.title = "Risk Groups\n", # 
       marktime = TRUE, #Censored
       background = FALSE,
       atrisk.cex = 1,
       # minAtrisk = 0, #applying a minimun at risk cutoff journal requirment
       atrisk.interspace = 1.2,
       confint = TRUE, #Confidence interval
       automar = TRUE,  #search and find the right number at risk
       xlim = c(0,150), #Range of Months printed on the graph
       atrisk.at = seq(0, 150, 12), # seqeucance of where to put at risk numbers
       logrank = TRUE) # show log-rank p-value
}
### Usage
# Replace time, cens and group with the correct column names in data
kpobj <- prodlim(Hist(time,cens) ~ group, data, grid = NULL)
aKP(kpobj)
# Overall follow-up by reversed kaplan meier and survival probablities
# Use 1 as group to get kpobj
km0 <- prodlim(Hist(time, cens) ~ 1, data, grid = NULL) 
# Generate qurtile summaries and CI
qkm0 <- quantile(km0)
# Generate overall kp plot
aKP(km0)

# Area Under the curve for the linear predictor i.e risk scores
##
# Define a function
AUCplot <- function(lp, t, x) {
res <- with(x,                        
  survivalROC(Stime = time,
              status = event,
              marker = get(lp),
              predict.time = t,
              # KM method without smoothing
              method       = "KM"))       
## Plot Area under the curve
with(res, plot(TP ~ FP, type = "l", 
               main = sprintf("t = %.0f, AUC = %.2f", t, AUC)))
          abline(a = 0, b = 1, lty = 2)
  res
}  
# Usage 
# Set matrix layout as needed
layout(matrix(1:15, byrow = T, ncol = 5))
# Plot time:AUCs - adjust sequence as needed
AUCobj <- lapply(1:10 * 12, function(t) {
  AUCplot(lp = "lp", t, x)
})


## See Univariate.R for single variable surival analysis
