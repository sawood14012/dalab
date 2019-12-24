library(ISLR)
df = Default
df$student = ifelse(df$student == "Yes", 1, 0)
df$student = as.factor(df$student)
df$default = ifelse(df$default == "Yes", 1, 0)
df$default = as.factor(df$default)

index = sample(x = 1:nrow(df), size = round(nrow(df) * 0.6))
df_train = df[index, ]
df_test = df[-index, ]

df_test
model <-
  glm(default ~ student + balance + income,
      family = "binomial",
      data = df_train)
summary(model)

pred = predict(model, df_test)
df_test$pred = pred
df_test$pred = ifelse(df_test$pred > 0.5, "Y", "N")
df_test$default = ifelse(df_test$default == 0, "N", "Y")
df_test
table(df_test$pred, df_test$default)