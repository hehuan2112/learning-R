library(meta)
library(pwr)

data <- read.csv('./Chills-10.csv')

# calculate RD
rst_meta <- metabin(
  Et,
  Nt,
  Ec,
  Nc,
  data = data,
  studlab = study,
  comb.random = TRUE,
  comb.fixed = FALSE,
  sm = "RR",
  method = "MH",
  method.tau = "DL",
  hakn = FALSE
)

forest(rst_meta)

total_Et <- sum(rst_meta$event.e)
total_Nt <- sum(rst_meta$n.e)
total_Ec <- sum(rst_meta$event.c)
total_Nc <- sum(rst_meta$n.c)
p1 <- total_Et / total_Nt
p2 <- total_Ec / total_Nc
h <- 2*asin(sqrt(p1)) - 2*asin(sqrt(p2))

# calculate OIS
#pwr.2p.test(h = , 
#            sig.level=, 
#            power =, 
#            alternative="two.sided", "less", or "greater")

#•	h=effect size (0.25)
#•	sig.level= significance level (0.05)
#•	power=power of test (0.80)
#•	alternative= two tailed

pwr.2p.test(
  h=h,         # RR
  power=0.8,      # beta 0.20
  sig.level=0.05, # alpha 0.05
  alternative="two.sided"
)

pub_bias <- metabias(rst_meta, method.bias = "linreg")

p <- 0.02
##If the p-value <0.05
##then
rst_tf <- trimfill(rst_meta)

summary(rst_tf)

original <- 1.39
adjusted <- 1.63


original <- 0.58
adjusted <- 0.90

adjusted - original