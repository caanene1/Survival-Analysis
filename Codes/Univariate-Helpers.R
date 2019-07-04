# Required packages
library(survival)
# Univariate survival analysis
# First and second column must be time and status respectively
UniVV <- function(dat) { 
# Rename columns
colnames(dat)[1:2] <- c("time", "status")
# Set variable names
covar <- c(names(dat[-c(1,2)]))

# Set the formular for adding variables
forma <- sapply(covar,
                function(x) as.formula(paste('Surv(time, status) ~ ', x)))

# Run the model
umods <- lapply(forma, function(x){coxph(x, dat)})

# Extract data 
unres <- lapply(umods,
                  function(x){ 
                   x <- summary(x)
                   p.value <- signif(x$logtest["pvalue"], digits = 3)
                   log.test <- signif(x$logtest["test"],  digits = 2)
                   # You can switch to wald as below
                   #p.value <- signif(x$wald["pvalue"], digits = 2)
                   #wald.test <- signif(x$wald["test"], digits = 2)
                   beta <- signif(x$coef[1], digits = 2);
                   HR <- signif(x$coef[2], digits = 2);
                   HR.CI.I <- signif(x$conf.int[,"lower .95"], 2)
                   HR.CI.U <- signif(x$conf.int[,"upper .95"], 2)
                   HR <- paste0(HR, " (", 
                                HR.CI.I, "-", HR.CI.U, ")")
                   res <- c(beta, HR, log.test, p.value)
                   names(res) <- c("beta", "HR (95% CI)", "log.test", 
                                 "p.value")
                   return(res)
                   #return(exp(cbind(coef(x),confint(x))))
})

# Final Output
res <- t(as.data.frame(unres, check.names = FALSE))
return(res)
}

# Usage 
Res <- UniVV(foo)