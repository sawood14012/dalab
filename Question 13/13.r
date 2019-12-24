df = read.csv("data/13.csv")

model = lm(m3 ~ m1 + m2, data = df)
summary(model)
df$pred_builtin = predict(model, data = df)
df

x1m = mean(df$m1)
x2m = mean(df$m2)
ym = mean(df$m3)

num1 = sum((df$m1 - x1m) * (df$m3 - ym))
den1 = sum((df$m1 - x1m) ^ 2)
b1 =  num1 / den1

num2 = sum((df$m2 - x2m) * (df$m3 - ym))
den2 = sum((df$m2 - x2m) ^ 2)
b2 =  num2 / den2

b0 = ym - b1 * x1m - b2 * x2m

df$pred = b0 + b1 * df$m1 + b2 * df$m2
df

rss = sum((df$m3 - df$pred) ^ 2)
rss
tss = sum((df$m3 - ym) ^ 2)
tss
se = 1 - (rss / tss)
se
rse = rss / (nrow(df) - 2)
rse