#1---
ufc = read.csv("ufc.csv")
str(ufc)
# a. names of tallest/ fattest trees
ufc_sort_by_height = ufc[order(ufc$height.m,decreasing = TRUE),]
head(ufc_sort_by_height)
ufc_sort_by_diameter = ufc[order(ufc$dbh.cm,decreasing = TRUE),]
head(ufc_sort_by_diameter)

#b
tapply(ufc$dbh.cm, ufc$species, mean)

#c
tapply(ufc$dbh.cm, ufc$species, quantile, prob = c(0.75))
sort(tapply(ufc$dbh.cm, ufc$species, quantile, prob = c(0.75)))

#d
ufc[,'slenderness'] = ufc$height.m/ufc$dbh.cm
(slenderness_by_species <- tapply(ufc$slenderness, ufc$species, quantile, prob = 0.5))
sort(slenderness_by_species)

#e
# need to identify species with largest average diameter
(ave_diameter_by_species <- tapply(ufc$dbh.cm, ufc$species, mean))
which.max(ave_diameter_by_species)
all_DF_data = ufc[ufc$species== "DF",]
all_DF_data[all_DF_data$height.m== max(all_DF_data$height.m),]


#2---
mammals = read.csv('sleep.csv')

#a
View(mammals)
par(mfrow = c(2,2))
hist(mammals$BodyWt)
hist(mammals$BrainWt)
plot(mammals$BodyWt, mammals$BrainWt)
model = lm(BrainWt~BodyWt,data=mammals)
pred = mammals$BodyWt*model$coefficients[2]+model$coefficients[1]
lines(mammals$BodyWt,pred)
summary(model)
plot(model)

#b
mammals$BodyWt <- log(mammals$BodyWt)
mammals$BrainWt <- log(mammals$BrainWt)
plot(mammals$BodyWt, mammals$BrainWt)
model2 = lm(BrainWt~BodyWt,data=mammals)
pred = mammals$BodyWt*model2$coefficients[2]+model2$coefficients[1]
lines(mammals$BodyWt,pred)
summary(model2)
plot(model2)

#c
model2$coefficients
model2$residuals
deviance(model2)
deviance(model2)/model$df.residual

#d
pred = predict(model2,data.frame(BodyWt=log(50)),interval = "confidence", level =0.95)
pred
exp(pred)

#f
null = lm(BrainWt~1,data=mammals)
anova(null,model2)
