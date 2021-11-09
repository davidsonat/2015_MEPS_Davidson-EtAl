setwd("G:/Acquisitions/School/Manuscript/New Stuff")
data <- read.table(file = "distfactor.txt", header = TRUE) 
#dist.txt for mod1, distfactor.txt for mod2-4, andydieoff.txt for 5-7, andyrecol.txt for 8, andysurvey.txt for 9-11

#Note that I changed coding of NC (control) to 'AC' so it appears as intercept in the model to make comparisons easier.

#Just making sure plot and treatment are treated as factors
treatment <- as.factor(data$treat)
plot <- as.factor(data$plot)
treatall <- as.factor(data$treatall)

#Just for the variables analysed with the quadrats (i.e., all except heights), I am treating disance as a factor.
distance <- as.factor(data$distance)


#Using nmle (not lme4) because it give P-values straight-up
library(nlme)

#Note that model formula is tiny bit different for nmle vs. lme4. I found correct formula based on pred. div. paper#

#Effect of distance on height, accounting for plot. Intercept is no-crab control at distance zero
#... and other treatments (WC and BC) shown relative to the intercept. Distance term in model summary
#... is the effect of distance in the controls, which is quite rightly not significant. However, what is 
#...important is that the interactions (distance:treatmentBC/WC) are significant, which directly tests our
#... hypothesis - effect of distance is much stronger (and actually present) for the crab treatments.
#... Remaining issues: need to explore presence of autocorrelation w distance, and plot model predictions

mod1 <- lme (height ~ log(distance+1)*treatment, random = ~1|plot, data = data)
summary(mod1)
anova(mod1)
plot(mod1)

matrix<- read.table(file ="comp.txt", header = TRUE)

as.matrix(matrix)

ContMat <-as.matrix(matrix)

comp<- glht(mod1, linfct = ContMat)

summary(comp)


#Looks like soem dodgy auto-correlation going on. We will need to add a term to account for this is the model
#.. Rebecca, over to you. 

#density within quadrats

mod2 <-lme (density ~ distance*treatment, random = ~1|plot, data = data)
summary(mod2)
anova(mod2)
plot(mod2)

#As prev. analysis showed, no effect on density (they don't shift sideways). But, need to check - and account for - 
#... any patterns in the residuals. I think they might be less likely in these models, because we are fitting a seperate 
#...parameter for each distance. 

mod3 <-lme (footdown ~ distance*treatment, random = ~1|plot, data = data)
summary(mod3)
anova(mod3)

comp<- glht(mod3, linfct = ContMat)

summary(comp)



#As seen before, there is no interaction. 

mod4 <-lme (rads ~ distance*treatment, random = ~1|plot, data = data)
summary(mod4)
anova(mod4)

comp<- glht(mod4, linfct = ContMat)

summary(comp)

#Model output is quite intuitive.






#For multiple comparisons.
#Simplify by making a single factor design. Make new column in data (davidson2) 'treatall'

#First, we need to read in the matrix of comparisons from the txt file. 

matrix<- read.table(file ="Andycomps.txt", header = TRUE)

#Then we need to turn this data frame into a matrix

as.matrix(matrix)
ContMat <-as.matrix(matrix)

#Then re-run rads model with treatall
mod4b <-lme (rads ~ treatall, random = ~1|plot, data = data)
summary(mod4b)
library(MASS)
library(multcomp)
comprads <- glht(mod4b, linfct = ContMat)
summary(comprads, test = adjusted ("fdr"))

#Shows and effect of both species (individually) on rads within first two distances only.
#.. corrected for multiple comparisons using 'false discovery rate' procedure.

#Re-run footdown model, as above
mod3b <-lme (footdown ~ treatall, random = ~1|plot, data = data)
summary(mod3b)
compfootdown <- glht(mod3b, linfct = ContMat)
summary(compfootdown, test = adjusted ("fdr"))

#This show that, strictly speaking, the effect of crabs on 'footdown' disappears after the first ssection.








#Now for the die-off. All plots unique, so no need for plot as random effect. 

mod5 <-lm (height ~ treatment, data = data)
summary(mod5)

#Can simply do Tukey tests for this
compheightdie <- glht(mod5, linfct = mcp(treatment="Tukey"))
summary(compheightdie)

mod6 <-lm (rads ~ treatment, data = data)
summary(mod6)
#Can simply do Tukey tests for this
compradsdie <- glht(mod6, linfct = mcp(treatment="Tukey"))
summary(compradsdie)

#For propotion recovery after snail removal, we need to use a binomial model. 

recover <- data$start-data$change

y <- cbind(data$start,data$final)

mod8 <- glm(y ~ treatment, family = binomial, data = data)
summary(mod8)
drop1(mod8, test="Chi")







#surveydata

#rads

mod9 <- lm(rades ~ treat, data = data)
summary(mod9)
anova(mod9)
plot(mod9)

#footdown (it's a count, so use poisson)
mod10 <- glm(footdown ~ treat, family = "poisson", data = data)
summary(mod10)
anova(mod10)
plot(mod10)
AIC(mod10)
drop1(mod10, test="Chisq")

#height
mod11 <- lm(height ~ treat, data = data)
summary(mod11)
anova(mod11)
plot(mod9)




