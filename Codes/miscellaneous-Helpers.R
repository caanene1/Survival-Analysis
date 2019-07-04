# F-Test to compare nested models
anova(mcoxfull, mcoxnest, test = "LRT")
# Models can be of cox or normal regression.
# one model must contain full variables and the other less