library(plumber)
library(readr)
library(lubridate)

# Definir o nome do arquivo CSV
csv_file <- "dados_regressao.csv"

# Verificar se o arquivo CSV existe, se não, criar o arquivo com as colunas corretas
if (!file.exists(csv_file)) {
  write_csv(data.frame(x = numeric(), grupo = character(), y = numeric(), momento_registro = character()), csv_file)
}

#* @apiTitle API para manipulação de dados de regressão

#* Echo back the input
#* @param msg The message to echo
#* @get /echo
function(msg=""){
  list(msg = paste0("The message is: '", msg, "'"))
}

#* Plot a histogram
#* @serializer png
#* @get /plot
function(){
  rand <- rnorm(100)
  hist(rand)
}

#* Return the sum of two numbers
#* @param a The first number to add
#* @param b The second number to add
#* @post /sum
function(a, b){
  as.numeric(a) + as.numeric(b)
}

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
  write_csv(novo_registro, csv_file, append = TRUE)
  
  return(list(success = TRUE, registro = novo_registro))
}

#* Modificar um registro existente no banco de dados
#* @param id O índice do registro a ser modificado
#* @param x O novo valor de x (opcional)
#* @param grupo O novo valor de grupo (opcional)
#* @param y O novo valor de y (opcional)
#* @put /modify_record

function(id, x = NULL, grupo = NULL, y = NULL) {
  # Ler o banco de dados
  df <- read_csv(csv_file)
  
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
  
  # Atualizar os valores se fornecidos, verificando se há apenas um valor e se é numérico
  if (!is.null(x)) {
    x <- as.numeric(x)
    if (is.na(x)) {
      return(list(error = "O valor de x deve ser numérico."))
    }
    df$x[id] <- x
  }
  
  if (!is.null(grupo)) {
    if (!grupo %in% c("A", "B", "C")) {
      return(list(error = "O grupo deve ser 'A', 'B' ou 'C'."))
    }
    df$grupo[id] <- grupo
  }
  
  if (!is.null(y)) {
    y <- as.numeric(y)
    if (is.na(y)) {
      return(list(error = "O valor de y deve ser numérico."))
    }
    df$y[id] <- y
  }
  
  # Salvar o arquivo atualizado
  write_csv(df, csv_file)
  
  return(list(success = TRUE, registro_atualizado = df[id, ]))
}

#* Deletar um registro existente no banco de dados
#* @param id O índice do registro a ser deletado
#* @delete /delete_record
function(id) {
  # Ler o banco de dados
  df <- read_csv(csv_file)
  
  # Verificar se o ID existe
  if (id < 1 || id > nrow(df)) {
    return(list(error = "ID inválido."))
  }
  
  # Deletar o registro
  df <- df[-id, ]
  
  # Salvar o arquivo atualizado
  write_csv(df, csv_file)
  
  return(list(success = TRUE, message = paste("Registro", id, "deletado.")))
}
