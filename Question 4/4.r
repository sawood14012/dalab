df = read.csv("data/4.csv")

model = lm(sales ~ budget, data = df)
summary(model)
df$pred_builtin = predict(model, data = df)
df

plot(df$budget, df$sales)
lines(df$budget, df$pred_builtin, col = "red")

xm = mean(df$budget)
ym = mean(df$sales)

num = sum((df$budget - xm) * (df$sales - ym))
den = sum((df$budget - xm) ^ 2)
b1 = num / den

b0 = ym - b1 * xm

df$pred = b0 + b1 * df$budget
df

plot(df$budget, df$sales)
lines(df$budget, df$pred, col = "red")

rss = sum((df$publications - df$pred) ^ 2)
rss
tss = sum((df$publications - ym) ^ 2)
tss
se = 1 - (rss / tss)
se
rse = rss / (nrow(df) - 2)
rse