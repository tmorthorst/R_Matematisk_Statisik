#opgave 56

#Vi bruger stadig datasæt med atleter fra ol2022, hvor vægten er responsvariablen og højden er forklaringsvariablen. 
#0 Fit den lineære regressionsmodel for datasættet hvor du fjerner de to tungeste atleter.

library(resampledata3)
View(Olympics2012)
plot(Olympics2012$Height, Olympics2012$Weight)

which(Olympics2012$Weight > 250)
Olympics2012_no_outliers <- Olympics2012[-c(9,32),]
plot(Olympics2012_no_outliers$Height, Olympics2012_no_outliers$Weight)
View(Olympics2012_no_outliers)
regr_olympics_no_outliers <- lm(Weight ~ Height, data = Olympics2012_no_outliers)
regr_olympics_no_outliers$coefficients

# 1. Hvad er den prædikterede vægt for en atlet, som er 69.4 inches høj?

regr_olympics_no_outliers$coefficients[1] + regr_olympics_no_outliers$coefficients[2] * 69.4

# 149,16

#2. Find et 95% konfidensinterval for den forventede vægt af en atlet, som er 69.4
#inches høj. Hvad siger intervallet om vægten?
new_obs <- data.frame(Height = 69.4)
predict(regr_olympics_no_outliers, newdata = new_obs, interval = "confidence", level = 0.95)


#3. 3. Find et 95% prædiktionsinterval for vægten af ovenst˚aende atlet, som er
#69.4 inches høj. Hvordan ser intervallet ud i forhold til konfidensintervalet
#fra delspm. 2? Forklar hvorfor.

predict(regr_olympics_no_outliers, newdata = new_obs, interval = 'prediction', level = 0.95)


#44. Find nu både 95% konfidensintervaller og 95% prædiktionintervaller for vægten
#af atleter med højde p˚a hhv. 54, 60 og 66 inches. Hvad kan siges om den
#relative størrelse af disse intervaller, n˚ar der varieres p˚a den forklarende variabel?
#Forklar dette.

obs_54 <- data.frame(Height = 54)
obs_60 <- data.frame(Height = 60)
obs_66 <- data.frame(Height = 66)

konf_54 <- predict(regr_olympics_no_outliers, newdata = obs_54, interval = "confidence", level = 0.95)
konf_60 <- predict(regr_olympics_no_outliers, newdata = obs_60, interval = "confidence", level = 0.95)
konf_66 <- predict(regr_olympics_no_outliers, newdata = obs_66, interval = "confidence", level = 0.95)

pred_54 <- predict(regr_olympics_no_outliers, newdata = obs_54, interval = "prediction", level = 0.95)
pred_60 <- predict(regr_olympics_no_outliers, newdata = obs_60, interval = "prediction", level = 0.95)
pred_66 <- predict(regr_olympics_no_outliers, newdata = obs_66, interval = "prediction", level = 0.95)

data.frame(konf_54, pred_54)
data.frame(konf_60, pred_60)
data.frame(konf_66, pred_66)
