df = read.csv("data/11.csv")

calcDist = function(x1, x2, y1, y2) {
  x = sqrt((x1 - x2) ^ 2 + (y1 - y2) ^ 2)
  return(x)
}
knn = function(m1, m2, n) {
  dist = c()
  for (i in 1:nrow(df)) {
    temp = calcDist(df[i, 1], m1, df[i, 2], m2)
    dist[i] = temp
  }
  df$dist = dist
  order_df = df[order(df$dist), ]
  print(order_df)
  countsplus = 0
  counts = 0
  counta = 0
  countb = 0
  countc = 0
  countd = 0
  counte = 0
  countf = 0
  for (i in 1:n) {
    if (order_df[i, 3] == "S+") {
      countsplus = countsplus + 1
    } else if (order_df[i, 3] == "S") {
      counts = counts + 1
    } else if (order_df[i, 3] == "A") {
      counta = counta + 1
    } else if (order_df[i, 3] == "B") {
      countb = countb + 1
    } else if (order_df[i, 3] == "C") {
      countc = countc + 1
    } else if (order_df[i, 3] == "D") {
      countd = countd + 1
    } else if (order_df[i, 3] == "E") {
      counte = counte + 1
    } else {
      countf = countf + 1
    }
  }
  if (countsplus >= counts) {
    print("S+")
  } else if (counts >= counta) {
    print("S")
  } else if (counta >= countb) {
    print("A")
  } else if (countb >= countc) {
    print("B")
  } else if (countc >= countd) {
    print("C")
  } else if (countd >= counte) {
    print("D")
  } else if (counte >= countf) {
    print("E")
  } else {
    print("F")
  }
}

m1 = 80
m2 = 85
neighbours = 3
knn(m1, m2, neighbours)