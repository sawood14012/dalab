library(ISLR)
library(MASS)

df = Default
df$student = as.numeric(df$student)
df$student = as.factor(df$student)

index = sample(1:nrow(df), round(nrow(df) * 0.6))
df_train = df[index, ]
df_test = df[-index, ]

model = qda(default ~ student + balance + income, df_train)
summary(model)

pred = predict(model, df_test)
df_test$pred = pred$class
df_test

table(df_test$pred, df_test$default)