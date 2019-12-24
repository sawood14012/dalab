df = read.csv("data/5.csv")

model = lm(Sales ~ TV + Radio, data = df)
summary(model)
df$pred_builtin = predict(model, data = df)
df

x1m = mean(df$TV)
x2m = mean(df$Radio)
ym = mean(df$Sales)

num1 = sum((df$TV - x1m) * (df$Sales - ym))
den1 = sum((df$TV - x1m) ^ 2)
b1 =  num1 / den1

num2 = sum((df$Radio - x2m) * (df$Sales - ym))
den2 = sum((df$Radio - x2m) ^ 2)
b2 =  num2 / den2

b0 = ym - b1 * x1m - b2 * x2m

df$pred = b0 + b1 * df$TV + b2 * df$Radio
df

rss = sum((df$Sales - df$pred) ^ 2)
rss
tss = sum((df$Sales - ym) ^ 2)
tss
se = 1 - (rss / tss)
se
rse = rss / (nrow(df) - 2)
rse