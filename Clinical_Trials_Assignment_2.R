# Load the package to read in the data
library(readxl)

# Read in the data
data <- read_excel("C:/Users/mihal/OneDrive/Case study Greece dataset_20220303.xlsx")


##============================
## Part 1a)
##============================

# Descriptive Statistics
head(data)
# View the first 5 values of the data
tail(data)
# View the last 5 values of the data
str(data)
# 3000 obs, 5 variables
dim(data)
# Check that the data were read in correctly
summary(data)
# Useful information regarding our data set
which(is.na(data))
# There are no missing values

# Creating our variables
subject <- data$id
trt <- factor(data$trt) # 0 refers to SoC, 1 refers to Drug X
y0 <- data$y0 # Baseline value
time <- data$time # 1 refers to Week 4, 2 refers to Week 8, 3 refers to Week 12
change <- data$change # Target variable




# Descriptive Statistics and Visualizations
with(data, interaction.plot(time,trt,change,type='o',pch=c(1,16)))
# We observe that as time goes by patients receiving SoC do NOT progress while patients receiving Drug X do progress!
# In time 1(4th Week) the difference in change between patients receiving SoC and Drug X is not great 
# In time 2(8th Week) the difference in change between patients receiving SoC and Drug X is considerably high
# In time 3(12th Week) the difference in change between patients receiving SoC and Drug X is huge!

ftime <- as.factor(time)

with(data,tapply(change,list(trt,ftime),mean))
# We observe that patients receving Soc have almost the same (mean) change at each time period (indicating minor to no progress)
# However patients receiving Drug X treatment seem to have (mean) change considerably higher at each time period, thus indicating that the drug X is more efficient!

library(lattice)
bwplot(change~ftime|trt)
# We observe that as regards the boxplots for the first time period (which refer to Week 4) there doesn't seem to be a great difference. Some outliers (on the patients receving the SoC) noted.
# We observe that as regards the boxplots for the second time period (which refers to Week 8) they seem a bit different.
# Patients receiving Drug X seem to score higher, thus having a better quality of life, in comparison to patients receiving SoC. The variability seems to be the same between the 2 treatments.
# we also observe that regarding the boxplots for the third time period (which refers to Week 12) there seems to be an overwhelming difference between the 2 treatments.
# Patients receiving Drug X seem to have a considerably higher change, thus indicating that they have a much better quality of life in comparison to patients receiving SoC.

xyplot(change~ftime | trt)
# Same conclusions as above


library(nlme)
# More descriptive statistics and visualizations

# dat <- groupedData(change~ftime|subject,outer=~as.factor(trt),data=data)
# plot(dat)
# plot(dat,outer=T)


dat2 <- groupedData(change~time|subject,outer=~as.factor(trt),data=data)
plot(dat2[1:27,]) # Profile plot per subject
plot(dat2[1:27,],outer=T) # Profile plot per drug
# Profile plots are quite informative since we can have indications regarding the heterogeneity of each subject, the general trend of the variance!
# Useful plot for tracking purposes

head(data,10)



# Data manipulation (long to wide format)
data21 <- data[data[,4]==1,5]
data21

data22 <- data[data[,4]==2,5]
data22

data23 <- data[data[,4]==3,5]
data23

treat <- data[data[,4]==1,2]
treat

vector_trt <- treat$trt
length(vector_trt)
vector_trt <- as.factor(vector_trt)

subject2 <- data[data[,4]==1,1]
vec_subject <- subject2$id
length(vec_subject)


y0 <- data[data[,4]==1,3]
y0 <- y0$y0
length(y0)


data2 <- cbind(data21,data22,data23,vector_trt,vec_subject,y0)
colnames(data2) <- c('Change in Week 4','Change in Week 8','Change in Week 12','trt','subject','y0')
head(data2)

dim(data2)




# Summary measures analysis
mean_4_8_12 <- (data2[,1]+data2[,2]+data2[,3])/3 # Create a summary measure


mod1 <- glm(mean_4_8_12~vector_trt,gaussian) # Post analysis
summary(mod1)
# We observe that the effect of the factor treatment is statistically significant!

plot(data2$y0,mean_4_8_12,pch=0+as.numeric(vector_trt),col=c('blue','red'))
# We observe that the scores in Weeks 4,8,12 (mean scores) are highly correlated with the baseline scores 

abline(lm(mean_4_8_12~data2$y0,subset=(vector_trt==0)),lty=2,col='blue')

abline(lm(mean_4_8_12~data2$y0,subset=(vector_trt==1)),lty=2,col='red')
# We observe that the slopes are parallel!
# That means that the interaction term (with the baseline value) is expected to be statistically insignificant!
# Let us see:


model1 <- lm(mean_4_8_12~vector_trt*y0) # Ancova analysis (GLM with gaussian family would be the same)
summary(model1)
# We observe that baseline and the interaction term are not statistically significant

model2 <- lm(mean_4_8_12~vector_trt) # Model without the interaction term and the baseline value
summary(model2)
# We observe that there's a difference between the 2 treatments! The estimated difference is 4.22 (statistically significant)


model3 <- lm(mean_4_8_12~vector_trt,offset=y0) # Another simple analysis (change analysis)
summary(model3)
# Once again the treatment factor is statistically significant



# Different summary measure (Slopes)
library(nlme)
id <- data$id
sepcoef <- lmList(change~time|id,data) # Fit a linear model on time for each subject
coef.emp <- coef(sepcoef) # All coefficients per subject
drug <- data$trt
subj <- data$id
data3 <- cbind(coef.emp,subj,drug)
head(data3)


boxplot(data3[,2]~data3[,4],xlab = 'Drug', ylab = 'Individual Slopes')
# We observe that the median of the slopes of the patients receiving Drug X (1 refers to Drug X) is higher than the median of the slopes of the patients receiving SoC treatment
# Variability is approximately the same between the 2 treatment groups
# There are some outliers observed in both treatment groups


# We can check that also by fitting a glm as follows:
model3 <- lm(time~as.factor(drug),data=data3)
summary(model3)
# We observe that the estimated difference between patients receiving Drug X and those receiving SoC treatment, is 1.14 (statistically significant!)
# That is to mean that patients receiving Drug X have higher mean slopes!
# We can also infer that the progress over time is NOT the same for the 2 treatment groups!

# We also observe that: 
var(coef(sepcoef)[1])
var(coef(sepcoef)[2])





# More visualizations
# Let us visualize the impact of different treatment groups on change variable across all time points
boxplot(change~trt, col=rainbow(2), las=1, main='Boxplots of different Treatment groups across all time points', ylab = 'Change', xlab='Treatment')
legend("topleft", c('SoC','Drug X'), lwd=4, col=c('red', 'cyan'))
# A boxplot is a standardized way of displaying the distribution of data based on a five number summary 
# (minimum, first quartile (Q1), median, third quartile (Q3), and maximum). It can tell you about your outliers 
# and what their values are. It can also tell you if your data is symmetrical, how tightly your data is grouped, and 
# if and how your data is skewed.
# We notice that the data are equally dispersed above and below the median.
# We observe that there are outliers in both groups!
# However, in treatment arm  1 (which refers to patients receiving Drug X) we notice there are outliers only in the higher extreme, which is positive as these values refer to better quality of life!
# We observe that there is no visible difference between the 2 groups of variable treatment as regards their impact on change!
# Moreover, we observe that variability seems to be approximately the same between the 2 groups.
# Plus, note that the scores of patients receiving Drug X seem to be higher in the 0-100 scale of the QoL scores in comparison to patients receiving SoC treatment!







# Create a new data frame, one for each treatment
new_data_drug_x <- data[data$trt==1,]
table(new_data_drug_x$trt)
# Confirmation that we selected the patients receiving Drug X

new_data_soc <- data[data$trt==0,]
table(new_data_soc$trt)
# Confirmation that we selected the patients receiving SoC



# Let us visualize the impact of different time points on change variable for patients receiving treatment Drug X
boxplot(new_data_drug_x$change~new_data_drug_x$time, col=rainbow(3), las=1, main='Boxplots of different Time Points for patients receiving Drug X' ,ylab = 'Change', xlab='Time Points')
legend("topleft", c('Week 4','Week 8','Week 12'), lwd=4, col=c('red','green','blue'))
# It is clear that time points 1 and 3 differ. 
# As time goes by, the variability of change increases.
# The majority of patients receiving Drug X at week 12 seem to have positive change scores thus indicating a better quality of life





# Let us visualize the impact of different time points on change variable for patients receiving treatment SoC
boxplot(new_data_soc$change~new_data_soc$time, col=rainbow(3), las=1, main='Boxplots of different Time Points for patients receiving SoC' ,ylab = 'Change', xlab='Time Points')
legend("bottomleft", c('Week 4','Week 8','Week 12'), lwd=4, col=c('red','green','blue'))
# It is clear that time points do not differ in this case 
# As time goes by, the variability of change increases.
# In this case, we observe that the majority of patients receiving SoC do not have significantly higher change values
# hence they do not have a better quality of life, which is the overall goal
# Therefore, this is another proof that Drug X is more effective than SoC overall







##============================
## Part 1b)
##============================
head(data)

# Fitting marginal models
m0.gls <- gls(change~trt,weights=NULL,method="ML",data=data) # Independent, homoscedastic data
summary(m0.gls) # AIC = 17019.9
# Same as linear model or glm with gaussian family
AIC_MARGINAL_MODEL_1 <- extractAIC(m0.gls)[2]



m1.gls <- gls(change~trt*ftime + y0,correlation=corCompSymm(form=~1|subject),method="ML",data=data) # Compound symmetry correlation matrix
summary(m1.gls) # R = 42.87% , AIC = 15705.87
getVarCov(m1.gls)
AIC_MARGINAL_MODEL_2 <- extractAIC(m1.gls)[2]


m2.gls <- gls(change~trt*ftime + y0,correlation=corAR1(form=~1|subject),method="ML",data=data) # Autoregressive of order 1
summary(m2.gls) # R = 58% , AIC = 15480.07
getVarCov(m2.gls)
AIC_MARGINAL_MODEL_3 <- extractAIC(m2.gls)[2]


m2.gls.heter <- gls(change~trt*ftime + y0,correlation=corAR1(form=~1|subject),weights=varIdent(form=~1|ftime),method="ML",data=data)
summary(m2.gls.heter) # R = 67.6% , AIC = 14213.04
getVarCov(m2.gls.heter)
AIC_MARGINAL_MODEL_4 <- extractAIC(m2.gls.heter)[2]



m3.gls <- gls(change~trt*ftime + y0,correlation=corSymm(form=~1|subject),method="ML",data=data) # Unstructured Cov matrix
summary(m3.gls) # AIC = 14884.43
getVarCov(m3.gls)
AIC_MARGINAL_MODEL_5 <- extractAIC(m3.gls)[2]



m3.gls.heter <- gls(change~trt*ftime + y0,correlation=corSymm(form=~1|subject),weights=varIdent(form=~1|ftime),method="ML",data=data) # Unstructured Cov matrix, heteroscedastic
summary(m3.gls.heter) # AIC = 14006.02
getVarCov(m3.gls.heter)
AIC_MARGINAL_MODEL_6 <- extractAIC(m3.gls.heter)[2]



m4.gls <- gls(change~trt*ftime + y0,correlation=corARMA(0.8,form=~1|subject,q=1),method="ML",data=data) # MA(1) (banded) correlation matrix, homoscedastic errors
summary(m4.gls) # R = 70% , AIC = 15568.97 
getVarCov(m4.gls)
AIC_MARGINAL_MODEL_7 <- extractAIC(m4.gls)[2]




m5.gls <- gls(change~trt*ftime + y0,correlation=corARMA(c(0.8,0.8),form=~1|subject,q=2),method="ML",data=data) # MA(2) (banded) correlation matrix, homoscedastic errors
summary(m5.gls) # AIC = 15467.21
getVarCov(m5.gls)
AIC_MARGINAL_MODEL_8 <- extractAIC(m5.gls)[2]




m6.gls <- gls(change~trt*ftime + y0,correlation=corARMA(c(0.8,0.8),form=~1|subject,p=1,q=1),method='ML',data=data) # ARMA(1,1)
summary(m6.gls) # AIC = 15467.21
getVarCov(m6.gls)
AIC_MARGINAL_MODEL_9 <- extractAIC(m6.gls)[2]





m6.gls.heter <- gls(change~trt*ftime + y0,correlation=corARMA(c(0.8,0.8),form=~1|subject,p=1,q=1),weights=varIdent(form=~1|ftime),method='ML',data=data) # ARMA(1,1), heteroscedastic
summary(m6.gls.heter) # AIC = 14150.88
getVarCov(m6.gls.heter)
AIC_MARGINAL_MODEL_10 <- extractAIC(m6.gls.heter)[2]



m7.gls.heter <- gls(change~trt*ftime + y0,correlation=corARMA(c(0.2,0.2,0.5,0.5),form=~1|subject,p=2,q=2),weights=varIdent(form=~1|ftime),method='ML',data=data) # ARMA(2,2), heteroscedastic
summary(m7.gls.heter) # AIC = 14150.88
getVarCov(m7.gls.heter)
AIC_MARGINAL_MODEL_11 <- extractAIC(m7.gls.heter)[2]




m8.gls <- gls(change~trt*ftime + y0,correlation=corExp(form=~1|subject),method='ML',data=data)
summary(m8.gls) # AIC = 15480.07
getVarCov(m8.gls)
AIC_MARGINAL_MODEL_12 <- extractAIC(m8.gls)[2]

# We observe that the model with the lowest AIC value is the Unrestricted Cov matrix with heteroscedastic errors!




# Create a matrix with the AIC of the marginal models run above
mat <- matrix(c(AIC_MARGINAL_MODEL_1,AIC_MARGINAL_MODEL_2,AIC_MARGINAL_MODEL_3,AIC_MARGINAL_MODEL_4,AIC_MARGINAL_MODEL_5,
AIC_MARGINAL_MODEL_6,AIC_MARGINAL_MODEL_7,AIC_MARGINAL_MODEL_8,
AIC_MARGINAL_MODEL_9,AIC_MARGINAL_MODEL_10,AIC_MARGINAL_MODEL_11,AIC_MARGINAL_MODEL_12),nrow=12,ncol=1,byrow=T)
mat

rownames(mat) <- c('Independent + Homoscedastic','Compound Symmetry + Homoscedastic','AR(1) + Homoscedastic','AR(1) + Heteroscedastic',
'Unstructured + Homoscedastic','Unstructured + Heteroscedastic','MA(1) + Homoscedastic','MA(2) + Homoscedastic','ARMA(1,1) + Homoscedastic',
'ARMA(1,1) + Heteroscedastic','ARMA(2,2) + Heteroscedastic','Exponential + Homoscedastic')

colnames(mat) <- 'AIC'
mat




# Multiple comparisons
anova(m1.gls,m2.gls,m2.gls.heter,m3.gls,m3.gls.heter,m4.gls,m5.gls,m6.gls,m6.gls.heter,m7.gls.heter,m8.gls) # Only for the nested models

intervals(m3.gls.heter) # 95% CI's of the coeffs of the best model based on AIC

plot(m3.gls.heter,resid(.,type="p")~time|trt) # pearson residual plot vs time for each treatment
# The heights of the bars seem to be different, indicating that there's heteroscedasticity

head(predict(m3.gls.heter))

qqnorm(m3.gls.heter,~resid(.,type="n"))  # resid type="response", "pearson", "normalized"











# Fitting random effects (or subject specific models)
m1.lme <- lme(change~trt*ftime + y0,random=~1|subject,method="ML",data=data) # one random intercept (for each subject)
summary(m1.lme) # AIC = 15705.87
AIC_RE_MODEL_1 <- extractAIC(m1.lme)[2]



m2.lme <- lme(change~trt*ftime + y0,random=~1|trt,method="ML",data=data) # one random intercept (for each treatment group)
summary(m2.lme) # AIC = 16206.62
AIC_RE_MODEL_2 <- extractAIC(m2.lme)[2]



m3.lme <- lme(change~trt*ftime + y0,random=~trt|subject,method="ML",data=data) # one random intercept (for each treatment group)
summary(m3.lme) # AIC = 15709.73
AIC_RE_MODEL_3 <- extractAIC(m3.lme)[2]




m4.lme <- lme(change~trt*ftime + y0,random=~time|subject,method="ML",data=data) # one random intercept (for each subject) and one random slope
summary(m4.lme) # AIC = 14930.64
# Note that stdDev(time) = 1.972, which is not close to zero, thus indicating that random slope is meaningful!
# Default is the Unrestricted Cov structure 
AIC_RE_MODEL_4 <- extractAIC(m4.lme)[2]




m5.lme <- lme(change~trt*ftime + y0,random=list(subject=pdDiag(~1+time)),method="ML",data=data) # independent and heteroscedastic
summary(m5.lme) # AIC = 15047.3
AIC_RE_MODEL_5 <- extractAIC(m5.lme)[2]



m6.lme <- lme(change~trt*ftime + y0,random=list(subject=pdIdent(~time)),method="ML",data=data) # independent and homoscedastic
summary(m6.lme) # AIC = 15088.33
AIC_RE_MODEL_6 <- extractAIC(m6.lme)[2]




m7.lme <-ortho.lme1.2<-lme(change~trt*ftime + y0,random=~1|subject,weights=varIdent(form=~1|trt),method="ML",data=data) # one random intercept, two residual error variances one for each treatment group
summary(m7.lme) # AIC = 15706.05
AIC_RE_MODEL_7 <- extractAIC(m7.lme)[2]




m8.lme.cor <- lme(change~trt*ftime + y0,random=~1|subject,correlation=corCompSymm(value=0.3,form=~1|subject),method="ML",data=data) # compound symmetry plus a random intercept (for each subject)
summary(m8.lme.cor) # AIC = 15707.87
AIC_RE_MODEL_8 <- extractAIC(m8.lme.cor)[2]




m9.lme.cor <- lme(change~trt*ftime + y0,random=~1|subject,correlation=corSymm(form=~1|subject),method="ML",data=data) # Unstuctured Cov matrix plus a random intercept (for each subject)
summary(m9.lme.cor) # AIC = 14886.43
AIC_RE_MODEL_9 <- extractAIC(m9.lme.cor)[2]




m10.lme.cor.heter <- lme(change~trt*ftime + y0,random=~1|subject,correlation=corSymm(form=~1|subject),
weights=varIdent(form=~1|trt), method="ML",data=data) # Unstuctured Cov matrix plus a random intercept (for each subject) and two residual error variances (one for each treatment)
summary(m10.lme.cor.heter) # AIC = 14885.31
# From the 10 models constructed using lme command, this one has the lowest AIC value
AIC_RE_MODEL_10 <- extractAIC(m10.lme.cor.heter)[2]




# Create a matrix with the AIC of the random effects models run above
mat2 <- matrix(c(AIC_RE_MODEL_1,AIC_RE_MODEL_2,AIC_RE_MODEL_3,AIC_RE_MODEL_4,AIC_RE_MODEL_5,
                 AIC_RE_MODEL_6,AIC_RE_MODEL_7,AIC_RE_MODEL_8,
                 AIC_RE_MODEL_9,AIC_RE_MODEL_10),nrow=10,ncol=1,byrow=T)
mat2


rownames(mat2) <- c('Random Intercept (for each subject)','Random Intercept (for each treatment)',
'Random Intercept + Random Slope for each subject (different for each treatment arm)',
'Random Intercept + Random Slope for each subject (different for each time point)','Covmat Random Effects, Diagonal',
'Covmat Random Effects, Identity',
'Random Intercept for each subject + Two Residual Error Variances (one for each treatment group)',
'Compound Symmetry Correlation Matrix + Random Intercept (for each subject)',
'Unstuctured Cov Matrix + Random Intercept (for each subject)',
'Unstuctured Cov Matrix + Random Intercept (for each subject) + Two Residual Error Variances (one for each treatment)')


colnames(mat2) <- 'AIC'
mat2




# More output based on lme
VarCorr(m10.lme.cor.heter)  # estimated variance (correlations) of random effects
tmp0 <- summary(m10.lme.cor.heter)
names(tmp0) # provides useful functions
head(random.effects(m10.lme.cor.heter))
summary(random.effects(m10.lme.cor.heter))
boxplot(random.effects(m10.lme.cor.heter))




##============================
## Part 2)
##============================

# Loading the library to be used for multiple comparisons
library(emmeans)

summary(m3.gls.heter) # The best model based on AIC (m3.gls.heter)
# We observe that ftime2 and ftime3 are not statistically significant but the interaction terms are, so we will not remove any components of the fixed parts of the model

intervals(m3.gls.heter) # 95% CI's of the models coefficients



emmeans(m3.gls.heter,pairwise~trt|ftime,adjust="tukey") 
# Multiple comparisons at different time points for each treatment
# We observe that mean change for patients receiving SoC treatment is 2.1 at the first time point (which refers to Week 4), while mean change for patients receiving Drug X is 3.04
# We observe that mean change for patients receiving SoC treatment is 2.19 at the second time point (which refers to Week 8), while mean change for patients receiving Drug X is 6.12
# We observe that mean change for patients receiving SoC treatment is 2.23 at the third time point (which refers to Week 12), while mean change for patients receiving Drug X is 10.02
# Note that we do trust the point estimates and the standard errors but not the p_values when using emmeans (p_values can get quite conservative!)







##============================
## Part 3
##============================

# Based on AIC, best model is the m3.gls.heter
# It's the marginal model that has unstructured covariance matrix with heteroscedastic errors!
summary(m3.gls.heter)

# Interpretations: (Πρακτικά αρκούσαν τα interpretations από το emmeans!)
# Intercept = 2.04 : Η μέση μέτρηση του change των ασθενών που λαμβάνουν SoC στην χρονική στιγμή 0 (στην baseline!)
# trt = 0.94 : Η μέση διαφορά του change μεταξύ ασθενών που λαμβάνουν Drug X και SoC στην 1η χρονική στιγμή (και είναι στατιστικά σημαντική!)
# ftime2 = 0.08 : H μέση διαφορά του change μεταξύ των ασθενών που λαμβάνουν SoC στην 1η και στην 2η χρονική στιγμή (και είναι στατιστικά ασήμαντη)
# ftime3 = 0.12 :H μέση διαφορά του change μεταξύ των ασθενών που λαμβάνουν SoC στην 1η και στην 3η χρονική στιγμή (και είναι στατιστικά ασήμαντη)
# y0 = 0.001 : Ο ρυθμός αλλαγής της μέτρησης της μέσης τιμής του change ως προς το χρόνο για τους ασθενείς που λαμβάνουν SoC (το επίπεδο αναφοράς) και είναι στατιστικά ασήμαντος όρος, η προσθήκη αυτού του όρου στο μοντέλο κάνει ευκολότερη την ερμηνεία των υπολοίπων!
# trt:ftime2 = 2.99 : Η διαφορά των ρυθμών αλλαγής των ασθενών που λαμβάνουν Drug X-SoC, δηλαδή οι ασθενείς που λαμβάνουν Drug X έχουν μεγαλύτερη διαφορά μεταξύ των χρονικών στιγμών 1 και 2! (και είναι στατιστικά σημαντικός όρος) 
# trt:ftime3 = 6.85 : Η διαφορά των ρυθμών αλλαγής των ασθενών που λαμβάνουν Drug X-SoC, δηλαδή οι ασθενείς που λαμβάνουν Drug X έχουν μεγαλύτερη διαφορά μεταξύ των χρονικών στιγμών 1 και 3! (και είναι στατιστικά σημαντικός όρος) 

# Ο πίνακας Correlation μετά τις εκτιμήσεις των coefficients, αναφέρεται στις συσχετίσεις μεταξύ των εκτιμώμενων παραμέτρων του (fixed part) μοντέλου



# Για τον αδόμητο πίνακα διακύμανσης-συνδιακύμανσης του μοντέλου έχουμε ότι: 
getVarCov(m3.gls.heter)
# Στην διαγώνιο του πίνακα βρίσκονται οι διακυμάνσεις των Yi για την κάθε χρονική στιγμή, i=1,2,3
# Τα μη διαγώνια στοιχεία του πίνακα είναι οι συνδιακυμάνσεις, δηλαδή τα Cov(Yij,Yij')
# 6 parameters to be estimated in this covariance matrix (3 parameters for the correlation estimates, 2 parameters for the proportions of the parameter variance estimates, 1 parameter for the residual standard error)
# Var(jk)=(σ^2)*θ(jk)
# Π.χ. για την ερμηνεία, έστω το: Cov(Yi1,Yi2) = 5.02 => Cor(Yi1,Yi2) = 5.02/(sqrt(4.0435)*sqrt(9.6585)) = 5.02/6.249332 = 0.8032859 > 0
# Αυτό λοιπόν σημαίνει ότι εφόσον έχουμε υψηλή τιμή την 1η χρονική στιγμή θα έχουμε υψηλή τιμή και την 2η χρονική στιγμή!







##============================
## Part 4a)
##============================

head(data)

# Let's create a  data frame that provides the data for time point 1 (which refers to week 4)
df_week_4 <- data[data$time==1,]
str(df_week_4)
table(df_week_4$time)
# Confirmation that we got the right time point

# Creating the variables for the first time point
id_w4 <- df_week_4$id
trt_w4 <- df_week_4$trt
table(df_week_4$trt)
# Initially, at week 4 there are 500 patients that were treated with SoC and 500 patients that were treated with Drug X

y0_w4 <- df_week_4$y0
time_w4 <- df_week_4$time

progress_w4 <- df_week_4$change
# Naming 'progress' the variable 'change', as we will transform it to a binary one:
# 0 refers to no improvement of the patients scores, 1 refers to improvement of the patients scores! 
# We were given that a change from baseline of 5 (or more) points indicates an improvement in the GHS/QoL scale (scores)



# Transforming the elements of variable 'progress_w4'
progress_w4[!progress_w4>=5] <- 0
progress_w4[progress_w4>=5] <- 1


head(progress_w4)



##============================
## Part 4b)
##============================

# Creating a new data frame that has the variable 'progress_w4' now
new_df_week_4 <- data.frame(id_w4,time_w4,trt_w4,y0_w4,progress_w4)
head(new_df_week_4,20)
tail(new_df_week_4,20)

table(new_df_week_4$progress_w4)
# Not quite optimistic results at a first sight: We observe at week 4 the patients that made progress (that is to mean their scores improved by 5 or more points) were only 115, while the patients that made no progress were 885




# Creating a new dataframe with the patients receiving SoC at Week 4
new_df_week_4_SoC <- new_df_week_4[new_df_week_4$trt_w4==0,]
head(new_df_week_4_SoC)
table(new_df_week_4_SoC$trt_w4) # Check that we got the patients receiving SoC only




# Visualizing the progress of the patients receiving SoC at Week 4
barplot(table(new_df_week_4_SoC$progress_w4), xlab = 'Progress', ylab = 'Number of Patients', 
        main ='The progress of patients receiving SoC at Week 4', col = c('blue','red'), las=1)
legend("topright", c('No Improvement','Improvement'), lwd=4, col=c('blue', 'red'))
# The vast majority of patients receiving SoC at week 4 hasn't made progress!





# Creating a new dataframe with the patients receiving Drug X at Week 4
new_df_week_4_DrugX <- new_df_week_4[new_df_week_4$trt_w4==1,]
head(new_df_week_4_DrugX)
table(new_df_week_4_DrugX$trt_w4) # Check that we got the patients receiving Drug X only




# Visualizing the progress of the patients receiving Drug X at Week 4
barplot(table(new_df_week_4_DrugX$progress_w4), xlab = 'Progress', ylab = 'Number of Patients', 
        main ='The progress of patients receiving Drug X at Week 4', col = c('blue','red'), las=1)
legend("topright", c('No Improvement','Improvement'), lwd=4, col=c('blue', 'red'))
# We observe the majority of patients receiving Drug X at week 4 hasn't made progress either!








ftime_w4 <- as.factor(time_w4) # Convert to factor

with(new_df_week_4,tapply(progress_w4,list(trt_w4,ftime_w4),mean))
# We observe that patients receiving Drug X seem to perform better as they have about twice the mean change scores vs patients receiving SoC




##============================
## Part 4c)
##============================



# Let's build a logistic regression model since the response is binary
target_w4 <- as.factor(progress_w4)
ftrt_w4 <- as.factor(trt_w4)

logistic_reg.1_w4 <- glm(target_w4~ftrt_w4 + y0_w4,binomial)
summary(logistic_reg.1_w4)

coef(logistic_reg.1_w4)
exp(coef(logistic_reg.1_w4))


# Building the optimal model since the baseline was statistically insignificant
# logistic_reg.2 <- glm(target~ftrt_w4,binomial)
# summary(logistic_reg.2)
# coef(logistic_reg.2)
# exp(coef(logistic_reg.2))



# Interpretation: (Στο μοντέλο logistic_reg.1_w4)
# Το exp(β1) (εφόσον ο γραμμικός προβλεπτής είναι σε logit scale) είναι ίσο με 2.21(=1+1.21), άρα οι ασθενείς που λαμβάνουν Drug X είναι 121% πιο πιθανό να έχουν progress σε σχέση με τους ασθενείς που λαμβάνουν SoC!








##============================
## Part 4d)
##============================


# Let's create a data frame that provides the data for time point 2 (which refers to week 8)
df_week_8 <- data[data$time==2,]
str(df_week_8)
table(df_week_8$time)
# Confirmation that we got the right time point

# Creating the variables for the second time point
id_w8 <- df_week_8$id
trt_w8 <- df_week_8$trt
table(df_week_8$trt)
# At week 8 there are 500 patients that were treated with SoC and 500 patients that were treated with Drug X

y0_w8 <- df_week_8$y0
time_w8 <- df_week_8$time

progress_w8 <- df_week_8$change
# Naming 'progress' the variable 'change', as we will transform it to a binary one:
# 0 refers to no improvement of the patients scores, 1 refers to improvement of the patients scores! 
# We were given that a change from baseline of 5 (or more) points indicates an improvement in the GHS/QoL scale (scores)



# Transforming the elements of variable 'progress_w8'
progress_w8[!progress_w8>=5] <- 0
progress_w8[progress_w8>=5] <- 1


head(progress_w8)



# Creating a new data frame that has the variable 'progress_w8' now
new_df_week_8 <- data.frame(id_w8,time_w8,trt_w8,y0_w8,progress_w8)
head(new_df_week_8,20)
tail(new_df_week_8,20)

table(new_df_week_8$progress_w8)
# At week 8 we observe that the patients that made progress (that is to mean their scores improved by 5 or more points) were 417, while the patients that made no progress were 583




barplot(table(new_df_week_8$progress_w8), xlab = 'Progress', ylab = 'Number of Patients', 
        main ='The progress of patients at Week 8', col = c('blue','red'), las=1)
legend("topright", c('No Improvement','Improvement'), lwd=4, col=c('blue', 'red'))
# The results now are more balanced in comparison to Week 4



ftime_w8 <- as.factor(time_w8) # Convert to factor

with(new_df_week_8,tapply(progress_w8,list(trt_w8,ftime_w8),mean))
# We observe that patients receiving Drug X seem to perform better as they have about the triple mean change scores vs patients receiving SoC



# Let's build a logistic regression model since the response is binary
target_w8 <- as.factor(progress_w8)
ftrt_w8 <- as.factor(trt_w8)

logistic_reg.1_w8 <- glm(target_w8~ftrt_w8 + y0_w8,binomial)
summary(logistic_reg.1_w8)

coef(logistic_reg.1_w8)
exp(coef(logistic_reg.1_w8))


# Interpretation: (Στο μοντέλο logistic_reg.1_w8)
# Το exp(β1) (εφόσον ο γραμμικός προβλεπτής είναι σε logit scale) είναι ίσο με 7.23(=1+6.23), άρα οι ασθενείς που λαμβάνουν Drug X είναι 623% πιο πιθανό να έχουν progress σε σχέση με τους ασθενείς που λαμβάνουν SoC!








# Let's create a  data frame that provides the data for time point 3 (which refers to week 12)
df_week_12 <- data[data$time==3,]
str(df_week_12)
table(df_week_12$time)
# Confirmation that we got the right time point

# Creating the variables for the third time point
id_w12 <- df_week_12$id
trt_w12 <- df_week_12$trt
table(df_week_12$trt)
# At week 12 there are 500 patients that were treated with SoC and 500 patients that were treated with Drug X

y0_w12 <- df_week_12$y0
time_w12 <- df_week_12$time

progress_w12 <- df_week_12$change
# Naming 'progress' the variable 'change', as we will transform it to a binary one:
# 0 refers to no improvement of the patients scores, 1 refers to improvement of the patients scores! 
# We were given that a change from baseline of 5 (or more) points indicates an improvement in the GHS/QoL scale (scores)



# Transforming the elements of variable 'progress_w12'
progress_w12[!progress_w12>=5] <- 0
progress_w12[progress_w12>=5] <- 1


head(progress_w12)



# Creating a new data frame that has the variable 'progress_w12' now
new_df_week_12 <- data.frame(id_w12,time_w12,trt_w12,y0_w12,progress_w12)
head(new_df_week_12,20)
tail(new_df_week_12,20)

table(new_df_week_12$progress_w12)
# At week 12 we observe that the patients that made progress (that is to mean their scores improved by 5 or more points) were 417, while the patients that made no progress were 583




barplot(table(new_df_week_12$progress_w12), xlab = 'Progress', ylab = 'Number of Patients', 
        main ='The progress of patients at Week 12', col = c('blue','red'), las=1)
legend("topright", c('No Improvement','Improvement'), lwd=4, col=c('blue', 'red'))
# The results now are definitely more encouraging



ftime_w12 <- as.factor(time_w12) # Convert to factor

with(new_df_week_12,tapply(progress_w12,list(trt_w12,ftime_w12),mean))
# We observe that patients receiving Drug X seem to perform better as they have about the triple mean change scores vs patients receiving SoC



# Let's build a logistic regression model since the response is binary
target_w12 <- as.factor(progress_w12)
ftrt_w12 <- as.factor(trt_w12)

logistic_reg.1_w12 <- glm(target_w12~ftrt_w12 + y0_w12,binomial)
summary(logistic_reg.1_w12)

coef(logistic_reg.1_w12)
exp(coef(logistic_reg.1_w12))


# Interpretation: (Στο μοντέλο logistic_reg.1_w12)
# Το exp(β1) (εφόσον ο γραμμικός προβλεπτής είναι σε logit scale) είναι ίσο με 12.97(=1+11.97), άρα οι ασθενείς που λαμβάνουν Drug X είναι 1197% πιο πιθανό να έχουν progress σε σχέση με τους ασθενείς που λαμβάνουν SoC!










# Creating a new dataframe with the patients receiving SoC at Week 8
new_df_week_8_SoC <- new_df_week_8[new_df_week_8$trt_w8==0,]
head(new_df_week_8_SoC)
table(new_df_week_8_SoC$trt_w8) # Check that we got the patients receiving SoC only




# Visualizing the progress of the patients receiving SoC at Week 8
barplot(table(new_df_week_8_SoC$progress_w8), xlab = 'Progress', ylab = 'Number of Patients', 
        main ='The progress of patients receiving SoC at Week 8', col = c('blue','red'), las=1)
legend("topright", c('No Improvement','Improvement'), lwd=4, col=c('blue', 'red'))
# We observe that the majority of patients receiving SoC at Week 8 made no progress





# Creating a new dataframe with the patients receiving Drug X at Week 8
new_df_week_8_DrugX <- new_df_week_8[new_df_week_8$trt_w8==1,]
head(new_df_week_8_DrugX)
table(new_df_week_8_DrugX$trt_w8) # Check that we got the patients receiving Drug X only




# Visualizing the progress of the patients receiving Drug X at Week 8
barplot(table(new_df_week_8_DrugX$progress_w8), xlab = 'Progress', ylab = 'Number of Patients', 
        main ='The progress of patients receiving Drug X at Week 8', col = c('blue','red'), las=1)
legend("topleft", c('No Improvement','Improvement'), lwd=4, col=c('blue', 'red'))
# We observe a noticeable difference in comparison to week 8 where there wasn't a considerable difference between the patients that made progress and those who didn't
# The majority of patients receiving Drug X has made progress!








# Creating a new dataframe with the patients receiving SoC at Week 12
new_df_week_12_SoC <- new_df_week_12[new_df_week_12$trt_w12==0,]
head(new_df_week_12_SoC)
table(new_df_week_12_SoC$trt_w12) # Check that we got the patients receiving SoC only




# Visualizing the progress of the patients receiving SoC at Week 12
barplot(table(new_df_week_12_SoC$progress_w12), xlab = 'Progress', ylab = 'Number of Patients', 
        main ='The progress of patients receiving SoC at Week 12', col = c('blue','red'), las=1)
legend("topright", c('No Improvement','Improvement'), lwd=4, col=c('blue', 'red'))
# We observe that the majority of patients receiving SoC at Week 12 made no progress 





# Creating a new dataframe with the patients receiving Drug X at Week 12
new_df_week_12_DrugX <- new_df_week_12[new_df_week_12$trt_w12==1,]
head(new_df_week_12_DrugX)
table(new_df_week_12_DrugX$trt_w12) # Check that we got the patients receiving Drug X only




# Visualizing the progress of the patients receiving Drug X at Week 12
barplot(table(new_df_week_12_DrugX$progress_w12), xlab = 'Progress', ylab = 'Number of Patients', 
        main ='The progress of patients receiving Drug X at Week 12', col = c('blue','red'), las=1)
legend("topleft", c('No Improvement','Improvement'), lwd=4, col=c('blue', 'red'))
# We observe a noticeable difference in comparison to week 12 where there wasn't a considerable difference between the patients that made progress and those who didn't
# The vast majority of patients receiving Drug X has made progress, the results now are overwhelming! Drug X seems clearly superior to SoC!







# GEE Approach
library(geepack)
head(data)
str(data)

overall_progress <- c(progress_w4,progress_w8,progress_w12)

# Descriptive statistics
with(data,tapply(overall_progress,list(trt,ftime),mean))
with(data,tapply(overall_progress,list(trt,ftime),var))

# Fitting GEE models
gee1 <- geeglm(overall_progress~trt*ftime + y0,binomial,id=subject,corstr="exchangeable",std.err="san.se",data=data1)
summary(gee1)


gee2 <- geeglm(overall_progress~trt*ftime + y0,binomial,id=subject,corstr="ar1",data=data1)
summary(gee2)


gee3 <- geeglm(overall_progress~trt*time + y0,binomial,id=subject,corstr="unstructured",data=data1)
summary(gee3)

# We observe that the exchangeable is preffered over the unstructured


gee4 <- geeglm(overall_progress~trt + y0,binomial,id=subject,corstr="exchangeable",std.err="san.se",data=data1)
summary(gee4)
exp(coef(gee4))


# Residuals plot
plot(fitted(gee4),resid(gee1,type="pearson"))






















