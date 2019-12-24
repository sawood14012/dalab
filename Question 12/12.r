df = read.csv("data/12.csv")

model = lm(m2 ~ m1, data = df)
summary(model)
df$pred_builtin = predict(model, data = df)
df
plot(df$m1, df$m2)
lines(df$m1, df$pred_builtin, col = "red")

xm = mean(df$m1)
ym = mean(df$m2)
num = sum((df$m1 - xm) * (df$m2 - ym))
den = sum((df$m1 - xm) ^ 2)
b1 = num / den
b0 = ym - b1 * xm
df$pred = b0 + b1 * df$m1
df
plot(df$m1, df$m2)
lines(df$m1, df$pred, col = "red")

rss = sum((df$m2 - df$pred) ^ 2)
rss
tss = sum((df$m2 - ym) ^ 2)
tss
se = 1 - (rss / tss)
se
rse = rss / (nrow(df) - 2)
rse