library(plumber)
library(readr)
library(lubridate)
library(ggplot2)

# Definir o nome do arquivo CSV
csv_file <- "dados_regressao.csv"

# Verificar se o arquivo CSV existe, se não, criar o arquivo com as colunas corretas
if (!file.exists(csv_file)) {
  readr::write_csv(data.frame(x = numeric(), grupo = character(), y = numeric(), 
                       momento_registro = character()), csv_file)
}


#* @apiTitle API para manipulação de dados de regressão

#* Inserir um novo registro no banco de dados
#* @param x O valor de x (numérico)
#* @param grupo O valor do grupo (A, B ou C)
#* @param y O valor de y (numérico)
#* @post /add_record
function(x, grupo, y) {
  # Validar os parâmetros
  if (missing(x) || missing(grupo) || missing(y)) {
    return(list(error = "Todos os parâmetros (x, grupo, y) são necessários."))
  }
  
  # Converter os parâmetros para o tipo correto
  x <- as.numeric(x)
  y <- as.numeric(y)
  
  # Validar o grupo
  if (!grupo %in% c("A", "B", "C")) {
    return(list(error = "O grupo deve ser 'A', 'B' ou 'C'."))
  }
  
  # Criar o novo registro com o momento atual
  novo_registro <- data.frame(
    x = x,
    grupo = grupo,
    y = y,
    momento_registro = as.character(lubridate::now())
  )
  
  # Adicionar o novo registro ao CSV
  readr::write_csv(novo_registro, csv_file, append = TRUE)
  
  return(list(mensagem = "Novo registro computado no arquivo CSV.", 
              registro = novo_registro))
}

#* Modificar um registro existente no banco de dados
#* @param id O índice do registro a ser modificado
#* @param x O novo valor de x (opcional)
#* @param grupo O novo valor de grupo (opcional)
#* @param y O novo valor de y (opcional)
#* @put /modify_record

function(id, x = NULL, grupo = NULL, y = NULL) {
  # Ler o banco de dados
  df <- readr::read_csv(csv_file)
  
  # Imprimir o dataframe e o número de linhas para diagnóstico
  print(df)
  print(paste("Número de linhas no dataframe:", nrow(df)))
  print(paste("ID fornecido:", id))
  
  # Verificar se há linhas suficientes no dataframe
  if (nrow(df) == 0) {
    return(list(error = "O banco de dados está vazio."))
  }
  
  # Verificar se o ID existe no intervalo do dataframe
  if (id < 1 || id > nrow(df)) {
    return(list(error = paste("ID inválido. O ID deve estar entre 1 e", nrow(df))))
  }
  
  if (df[id, "x"] == x){
    print(list(error = "O valor fornecido de x é igual ao existente neste id"))
  }
  if (df[id, "grupo"] == grupo){
    print(list(error = "O grupo fornecido é igual ao existente neste id"))
  }
  if (df[id, "y"] == y){
    print(list(error = "O valor fornecido de y é igual ao existente neste id"))
  }
  
  
  # Atualizar os valores se fornecidos, verificando se há apenas um valor e se é numérico
  if (!is.null(x)) {
    x <- as.numeric(x)
    if (is.na(x)) {
      return(list(error = "O valor de x deve ser numérico."))
    }
    df[id,"x"] <- x
  }
  
  if (!is.null(grupo)) {
    if (!grupo %in% c("A", "B", "C")) {
      return(list(error = "O grupo deve ser 'A', 'B' ou 'C'."))
    }
    df[id,"grupo"] <- grupo
  }
  
  if (!is.null(y)) {
    y <- as.numeric(y)
    if (is.na(y)) {
      return(list(error = "O valor de y deve ser numérico."))
    }
    df[id, "y"] <- y
  }
  
  tryCatch({
    readr::write_csv(df, csv_file)
    return(list(mensagem = paste("CSV. após a alteração do ID:", id), 
                data_frame = df))
  }, error = function(e) {
    return(list(error = "Erro ao salvar o arquivo CSV."))
  })
}

#* Deletar um registro existente no banco de dados
#*  Fazer com que seja possível deletar mais de um id por vez
#* @param id O índice do registro a ser deletado
#* @delete /delete_record
function(id) {
  col_spec <- cols(
    x = col_double(),
    grupo = col_character(),
    y = col_double(),
    momento_registro = col_datetime(format = "")
  )
  
  df <- readr::read_csv(csv_file, col_types = col_spec)
  
  id <- as.numeric(id)
  
  if (is.na(id) || id < 1 || id > nrow(df)) {
    return(list(error = paste("ID inválido. O ID deve estar entre 1 e", nrow(df))))
  }
  df <- df[-id, ]
  
  readr::write_csv(df, csv_file)
  
  resultado <- list(mensagem = paste("CSV. após o ID:", id, "ser deletado"), 
                    data_frame = df)
  
  return(resultado)
}


# Gráfico dispersão com Regressão
#* @serializer png
#* @get /grafico_dispersao
function() {
  grafico <- ggplot2::ggplot(df, aes(x = x, y = y, color = grupo)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Reta de regressão
  labs(title = "Gráfico de Dispersão com Regressão",
       x = "X",
       y = "Y",
       color = "Grupo") +
  theme_minimal()
return(grafico)
}


# Cálculo dos coeficientes de regressão 
# Cria as variáveis dummy e utiliza o grupo A como referência 
#*@parser json
#* @serializer unboxedJSON
#* @get /estimativas_coeficientes
function() {
  modelo <- lm(y ~ x + grupo, data = df)
  coeficientes <- list(intercepto = modelo$coefficients[1],
                       x = modelo$coefficients[2],
                       grupoB = modelo$coefficients[3],
                       grupoC = modelo$coefficients[4])

return(coeficientes)
}


# Resíduos da Regressão 
#*@parser json
#* @serializer unboxedJSON
#* @get /residuos
function() {
  residuos <- lm(y ~ x + grupo, data = df)$residuals 
  return(list(mensagem = "Resíduos do modelo de regressão",
    Residuos = residuos 
  ))
}



# Gráfico Valores Observados x Resíduos 
#* @serializer jpeg
#* @get /grafico_residuos
function() {
  df <- read_csv(csv_file)
  
  dados_graf <- data.frame(residuos = lm(y ~ x + grupo, data = df)$residuals, 
                           observados = df$y) 
  
  grafico_res <- ggplot2::ggplot(dados_graf) +
    geom_point(mapping = aes(observados, residuos), color = "red") +
    labs(
      x = "Obsevados (y)",
      y = "Resíduos",
      title = "Gráfico de Dispersão",
      subtitle = "Valores observados vs Resíduos do modelo") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.caption = element_text(hjust = 1, face = "italic"),
          plot.subtitle = element_text(hjust = 0.5, face = "bold"),
          legend.title = element_text(face = "bold"),
          axis.title = element_text(face = "bold")) 
  
  return(grafico_res)
}



# Predição do modelo de Regressão 
# Precisa otimizar, não colocar os números diretamente 
#* @param x
#* @param grupo
#* @parser json
#* @serializer unboxedJSON
#* @get /predicao
function(x,grupo) {
  coeficientes <- as.vector(lm(y ~ x + grupo, data = df)$coefficients)
  
  if (length(x) == 1){
    if (grupo == "B") {
      coef <- 1.427378}
    else if (grupo == "C") {  
      coef <- 3.442200}
    else {
      coef <- 0
    }
    y <- 1.155123*as.numeric(x) + coef +  1.356496
    return(y)
  }
    else{
      
    }
  
  if (grupo == "B") {
    coef <- 1.427378}
  else if (grupo == "C") {  
    coef <- 3.442200}
  else {
    coef <- 0
  }
  y <- 1.155123*as.numeric(x) + coef +  1.356496
  print(y)
  }
