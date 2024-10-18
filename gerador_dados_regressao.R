# gerador de dados
ra <- 186777
b0 <- runif(1, -2, 2); b1 <- runif(1, -2, 2)
bB <- 2; bC <- 3
n <- 25
x <- rpois(n, lambda = 4) + runif(n, -3, 3)
grupo <- sample(LETTERS[1:3], size = n, replace = TRUE)
y <- rnorm(n, mean = b0 + b1*x + bB*(grupo=="B") + bC*(grupo=="C"), sd = 2) 
df <- data.frame(x = x, grupo = grupo, y = y,
                   momento_registro = lubridate::now()) 

readr::write_csv(df, file = "dados_regressao.csv")
                                                                                               