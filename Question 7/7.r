library(ISLR)
library(MASS)
df = Default

index = sample(x = 1:nrow(df), size = round(nrow(df) * 0.6))
df_train = df[index, ]

df_test = df[-index, ]

model = lda(default ~ student + balance + income, data = df_train)
summary(model)

pred = predict(model, df_test)
df_test$pred = pred$class
df_test

table(df_test$pred, df_test$default)