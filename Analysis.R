# Environment setting
rm(list=ls())
library(psych)
library(lavaan)

# Loading Conceptual span data
data <- read.csv("CSitems.csv")

### FACTOR ANALYSIS ###

data_CFA <- data[,4:13]

model <- "F=~  item1 + item2 + item3 + item4 + item5 + item6 + item7 + item8 + item9 + item10
"

fit <- cfa(model, data=data_CFA, std.lv=TRUE)

summary(fit, fit.mea=TRUE)

### Capacity Estimation ###

mean(apply(data_CFA, 1, mean), na.rm=T)*3*5/8

# Loading full dataset

data <- read.csv("puntizeta.csv", na.string="NS")
data <- data[!data$soggetto == "", 1:46]
socioana <- read.csv("socioana.csv")

data <- merge(data, socioana, by.x="soggetto", by.y="Codice") # merging socio-biographical characteristics
colnames(data)[4] <- "ReadingComprehension"
colnames(data)[28] <- "ConceptualSpan"
colnames(data)[25] <- "DirectDigitSpan"
colnames(data)[26] <- "InverseDigitSpan"
colnames(data)[12] <- "ReadingSpeed"
colnames(data)[13] <- "ReadingErrors"

### Linear model ###

fit <- lm(ReadingComprehension ~ DirectDigitSpan + InverseDigitSpan + ReadingSpeed + ReadingErrors + ConceptualSpan, data=data)
summary(fit)


