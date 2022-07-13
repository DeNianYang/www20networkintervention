#To get standardized regression coefficients,
#standardize all variables (IVs and DV),
#then re-run regression model. Standardize by
#(IV - mean(IV))/sd(IV) #scale 
standardize <- function(array){
    array = (array - mean(array, na.rm = TRUE))/ sd(array, na.rm = TRUE)
    return (array)
}

center <- function(array){
    array = (array - mean(array, na.rm = TRUE))
    return (array)
}


#-------------------------------------------------------------------------------------------- 
library(lmerTest)
library(psycho)

#table <- read.table("C:\\Users\\Cynthia\\Desktop\\intervention_code\\user study\\user_study_3.csv", header=TRUE, sep=",")
table <- read.table("user_study_3_0124_5.csv", header=TRUE, sep=",")
#table <- read.table("user_study_4_2.csv", header=TRUE, sep=",")
#Symiin @@@
table$Intervention = factor(table$Intervention,
                            labels=c("Control","Intervention"))
table$Week = table$Week - 1
#I (Week = 0, 2, 4, 6, 8)
#C (Week = 0, 1, 2, 3)
#table = table[table$ID >= 57,] #Keeping only weeks <= 2 so I group's effect does not level out
#table = table[table$Week >= 2,]
#Random effect in intercept only
fit <- lmer(Q2~Intervention*Week+(1|ID) , data=table)
anova(fit)
psycho::analyze(fit)
psycho::get_means(fit)
summary(fit)

fit <- lmer(Q3~Intervention*Week+(1|ID) , data=table)
anova(fit)
psycho::analyze(fit)
psycho::get_means(fit)
summary(fit)
#SMC: No need to follow-up with contrast as Intervention effect not significant
#Also, with only two groups, it is easy to make interpretations without
#using contrasts

#results <- get_contrasts(fit, "Intervention")
#print(results)
#results <- psycho::get_means(fit)
#print(results)

# Keep random effects of intercept and Week in your model
fit2 <- lmer(Q3~Intervention*Week+(1+Week|ID), data=table)
anova(fit2)
psycho::analyze(fit2)
psycho::get_means(fit2)
summary(fit2)
# Predicted Q2 for C = 12.37 -1.15*Week
# Predicted Q2 for I = 12.37 -3.14*Intervention 
#                     (-1.15 + 1.38)*Week
fit3 <- lmer(Q4~Intervention*Week+(1+Week|ID), data=table)
summary(fit3)
anova(fit3)
psycho::analyze(fit3)
psycho::get_means(fit3)
summary(fit3)

Week = c(0,1,2)
Control = 12.37 -1.15*Week
Interv = 12.37 -3.14*1 -1.15*Week + 1.38*Week

plot(Week, Control,type="l",ylim=c(9,12.5))
lines(Week, Interv, col="red",lty=2)
legend("topright",c("Control","Intervention"),
       col=c(1,"red"),lty=c(1,2),bty="n")

fit4 <- lmer(Q3~Intervention*Week+(1+Week|ID), data=table)
summary(fit4)
anova(fit4)
psycho::analyze(fit4)
psycho::get_contrasts(fit4)

fit5 <- lmer(Q2~Intervention*Week+(1+Week|ID), data=table)
summary(fit5)
anova(fit5)
psycho::analyze(fit5)
psycho::get_contrasts(fit5)
