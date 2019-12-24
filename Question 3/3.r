df = read.csv("data/3.csv")

calcDist = function(x1, x2, y1, y2) {
  x = sqrt((x1 - x2) ^ 2 + (y1 - y2) ^ 2)
  return(x)
}
knn = function(age, load, n) {
  dist = c()
  for (i in 1:nrow(df)) {
    temp = calcDist(df[i, 1], age, df[i, 2], load)
    dist[i] = temp
  }
  df$dist = dist
  order_df = df[order(df$dist), ]
  print(order_df)
  countn = 0
  county = 0
  for (i in 1:n) {
    if (order_df$defaulter[i] == "N") {
      countn = countn + 1
    } else {
      county = county + 1
    }
  }
  if (countn >= county) {
    print("Not Defaulter")
  } else {
    print("Defaulter")
  }
}
age = 50
loan = 500000
neighbours = 3
knn(age, loan, neighbours)