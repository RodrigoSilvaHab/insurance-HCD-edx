##########################################################
# Loading of Required Libraries
##########################################################

# Note: this process could take a couple of minutes
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")

library(caret)
library(lubridate)
library(readxl)
library(tidyverse) # includes ggplot2, tidyr, dplyr and others

##########################################################
# Loading Cleaned Data
##########################################################
# Sets the working Directory to the path where files are located
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Reads the data from XL File
HCData <- read_excel("Insurance.xlsx", sheet="DataV1")

# The first row (i.e. row 0) is the header, it contains appropriate variable names
# The second row is a dummy data row will all the fields with the correct
#   data type and non-empty values, it allows correct type identification 
#   by R, and it it  deleted after loading data
HCData <- HCData[-1,]

# Convert MedConditions from char to factors
as.factor(HCData$Cond) -> MedConditions
HCData <- bind_cols(HCData, MedConditions)
HCData <- HCData[, -which(names(HCData) == "Cond")]
colnames(HCData)[42] <- "MedCond"
rm(MedConditions)

# Convert EventState from char to factors
as.factor(HCData$EvState) -> States
HCData <- bind_cols(HCData, States)
HCData <- HCData[, -which(names(HCData) == "EvState")]
colnames(HCData)[42] <- "EvState"
rm(States)

# Generate Test and Train Data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = HCData$IDEv, times = 1, p = 0.1, list = FALSE)

HC_Train <- HCData[-test_index,]
HC_Test <- HCData[test_index,]

# PointPlot
HC_Train %>%
  ggplot(aes(x=as.factor(MedCond), y=IAPaid/1000000)) +
  geom_point(alpha = 0.05, size = 4) +
  labs(x="", y="", title="Paid Amount (M COP) Vs. Medical Condition") +
  theme(axis.text.x=element_text (angle=90))

# BoxPlot
HC_Train %>%
  ggplot(aes(x=factor(MedCond), y=IAPaid/1000000), fill=factor(MedCondition)) +
  geom_boxplot() +
  labs(x="", y="", title="Paid Amount (M COP) Vs. Medical Condition") +
  theme(axis.text.x=element_text (angle=90))

# ALL Numerical Columns, columns that contain numbers (no dates), including IndexedAmountPaid (IAPaid)
NumCols <- c("AInsured", "ADed", "AInvTot", "ATAud", "ATObj", "IAInv", "IAPret", "IAObj", "IAGlos", "IAAud", "IAAccObj", "IARaisObj", "IAPaid")

# High Cost Numerical Value, including IndexedAmountPaid (IAPaid)
HCNum_Train <- HCData[colnames(HC_Train) %in% NumCols]
HCNum_Test <- HCData[colnames(HC_Test) %in% NumCols]

cor(HCNum_Train)

# Numerical Columns, columns that contain numbers (no dates), excluding IndexedAmountPaid (IAPaid)
# IAGlos and IAAccObj are non significant
Model1 <- lm(IAPaid ~ AInsured + ADed + ATAud + ATObj + IAInv + IAPret + IAObj + IAAud + IARaisObj, data = HCNum_Train)

# Residuals Plot (in COP Million)
Model1 %>%
  ggplot(aes(x=.fitted/1000000, y=.resid/1000000)) +
  geom_point(alpha = 0.05, size = 4) +
  geom_hline(yintercept = 0) +
  labs(title = "Residual (Millions) Vs. Fitted IAPaid (Millions) Plot", x="Fitted IAPaid", y = "Residuals")

Model1 %>%
  ggplot(aes(x=.fitted/1000000, y=.resid/1000000)) +
  geom_point(alpha = 0.05, size = 4) +
  coord_cartesian(ylim=c(-50, 50)) + 
  geom_hline(yintercept = 0) +
  labs(title = "Residual (Millions) Vs. Fitted IAPaid (Millions) Plot", x="Fitted IAPaid", y = "Residuals Under 50 M")

summary(Model1)

# Extract residuals in COP Millions
residuals <- residuals(Model1)/1e6

summary(residuals)

# Create a dataframe with residuals
residual_df <- data.frame(Residuals = residuals)

residual_df %>%
  ggplot(aes(x = Residuals)) +
  geom_histogram(binwidth = 0.1) +
  coord_cartesian(xlim=c(-5, 5))  +
  labs(title = "Histogram of Residuals", x = "Residuals (Millions) Under 5M", y = "Count")

qqnorm(Model1$residuals)
qqline(Model1$residuals)

# IAAud and IARaisObj have low coefficients
Model2 <- lm(IAPaid ~ AInsured + ADed + ATAud + ATObj + IAInv + IAPret + IAObj, data = HCNum_Train)

# Not a better model, cannot reject null hypothesis
anova(Model1, Model2)


HCNum_Test %>% 
  mutate(IAPaid_hat = predict(Model1, HCNum_Test)) %>%
  mutate(Error = IAPaid_hat - IAPaid) %>%
  ggplot(aes(x=IAPaid_hat/1000000, y=Error/1000000)) +
    geom_point(alpha = 0.05, size = 4) +
    coord_cartesian(ylim=c(-75, 75)) +
    labs(title = "Residual (Millions) Vs. Predicted IAPaid (Millions) Plot for Test Set", x="Fitted IAPaid", y = "Residuals")

# Use predict to get predictions and intervals
predictions <- predict(Model1, newdata = HCNum_Test, interval = "prediction")

# Create a dataframe with HCNum_Test, fitted Value and Intervals
results <- cbind(HCNum_Test, predictions)

results %>%
  ggplot(aes(IAPaid/1e6, fit/1e6)) +
  geom_point(alpha = 0.05, size = 4) +
  coord_cartesian(xlim=c(0, 75), ylim=c(0, 75)) +
  stat_smooth(method = lm) +
  labs(title = "Predicted Vs. Test IAPaid Plot (in COP Millions)", x="Test IAPaid", y = "Predited IAPaid")
