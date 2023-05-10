library(meta)

data <- read.csv('./IO-raw-ET.csv')

rst_meta <- metabin(Et,
                      Nt,
                      Ec,
                      Nc,
                      data = data,
                      studlab = study,
                      comb.random = TRUE,
                      comb.fixed = FALSE,
                      sm = "OR",
                      method = "MH",
                      method.tau = "DL",
                      hakn = FALSE)

rst_meta$I2 * 100

rst_bias = metabias(rst_meta, method.bias = "linreg")
print(rst_bias)