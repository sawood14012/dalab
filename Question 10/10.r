x = c(1:10)
hash = function(val) {
  return ((6 * val + 1) %% 23)
}
tobit = function(val) {
  val = intToBits(val)
  val = paste(as.integer(val), collapse = "")
  return(val)
}
max = 0
count_max = function(val) {
  count = 0
  val = strsplit(val, split = "")
  for (i in 1:length(val[[1]])) {
    if (val[[1]][i] != 1) {
      count = count + 1
    } else {
      return(count)
    }
  }
  return(0)
}
hash_values = c()
bit_values = c()
count_values = c()
for (i in x) {
  hash_values[i] = hash(i)
  bit_values[i] = tobit(hash_values[i])
  count_values[i] = count_max(bit_values[i])
}
final = data.frame(hash = hash_values, bits = bit_values, count = count_values)
final
num = 2 ^ max(count_values)
num
