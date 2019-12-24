df = read.csv("data/2.csv")

model = lm(publications ~ experience + training, data = df)
summary(model)
df$pred_builtin = predict(model, data = df)
df

x1m = mean(df$experience)
x2m = mean(df$training)
ym = mean(df$publications)

num1 = sum((df$experience - x1m) * (df$publications - ym))
den1 = sum((df$experience - x1m) ^ 2)
b1 =  num1 / den1

num2 = sum((df$training - x2m) * (df$publications - ym))
den2 = sum((df$training - x2m) ^ 2)
b2 =  num2 / den2

b0 = ym - b1 * x1m - b2 * x2m

df$pred = b0 + b1 * df$experience + b2 * df$training
df

rss = sum((df$publications - df$pred) ^ 2)
rss
tss = sum((df$publications - ym) ^ 2)
tss
se = 1 - (rss / tss)
se
rse = rss / (nrow(df) - 2)
rse