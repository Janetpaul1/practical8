states<-as.data.frame(state.x77)
states
str(states)
View(states)
colnames(states)[colnames(states)=="Life Exp"]<-"Life_Exp"
colnames(states)[colnames(states)=="HS Grad"]<-"HS_Grad"
View(states)
windows(20,10)
pairs(states)
#install.packages("psych")
library(psych)
pairs.panels(states,
             smooth = FALSE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = FALSE,    # If TRUE, draws ellipses
             method = "pearson",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)     
model <- lm(Murder ~ 
              Population + 
              Income + 
              Illiteracy + 
              Life_Exp + 
              HS_Grad + 
              Frost+
              Area,
            data = states)
model
summary(model)
windows(20,10)
par(mfrow=c(4,2))


scatter.smooth(x=states$Population,
               y=states$Murder,
               main = "Scatter Plot of population vs. Murder",
               xlab = "Population",
               ylab = "murder")
scatter.smooth(x=states$Income,
               y=states$Murder,
               main = "Scatter Plot of life expectancy vs. Murder",
               xlab="Income",
               ylab="Murder")

scatter.smooth(x=states$Illiteracy,
               y=states$Murder,
               main = "Scatter Plot of illeteracy vs. Murder",
               xlab="Illeteracy",
               ylab = "Murder")
scatter.smooth(x=states$Life_Exp,
               y=states$Murder,
               xlab="Life_Exp",
               ylab="Murder",
               main = "Scatter Plot of life expectancy vs. Murder")
scatter.smooth(x=states$HS_Grad,
               y=states$Murder,
               main = "Scatter Plot of HS grad vs. Murder",
               xlab = "HS_Grad",
               ylab="Murder")
scatter.smooth(x=states$Frost,
               y=states$Murder,
               main = "Scatter Plot of Frost vs. Murder",
               xlab = "Frost",
               ylab ="murder")
scatter.smooth(x=states$Area,
               y=states$Murder,
               main = "Scatter Plot of Area vs. Murder",
               xlab = "Area",
               ylab = "Murder")

#cor(states$Murder,states$Population)
correlation_matrix<-cor(states)
windows(20,16)
corPlot(correlation_matrix)
