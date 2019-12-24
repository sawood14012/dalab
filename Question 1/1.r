df = read.csv("data/1.csv")

model = lm(publications ~ experience, data = df)
summary(model)
df$pred_builtin = predict(model, data = df)
df
plot(df$experience, df$publications)
lines(df$experience, df$pred_builtin, col = "red")

xm = mean(df$experience)
ym = mean(df$publications)
num = sum((df$experience - xm) * (df$publications - ym))
den = sum((df$experience - xm) ^ 2)
b1 = num / den
b0 = ym - b1 * xm
df$pred = b0 + b1 * df$experience
df
plot(df$experience, df$publications)
lines(df$experience, df$pred, col = "red")

rss = sum((df$publications - df$pred) ^ 2)
rss
tss = sum((df$publications - ym) ^ 2)
tss
se = 1 - (rss / tss)
se
rse = rss / (nrow(df) - 2)
rse