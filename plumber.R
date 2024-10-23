library(plumber)
library(readr)
library(lubridate)
library(ggplot2)

# Definir o nome do arquivo CSV
csv_file <- "dados_regressao.csv"

col_spec <- cols(
  x = col_double(),
  grupo = col_character(),
  y = col_double(),
  momento_registro = col_datetime(format = "")
)

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
  df <- readr::read_csv(csv_file, col_types = col_spec)
  
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
                ID_alterado= df[id,]))
  }, error = function(e) {
    return(list(error = "Erro ao salvar o arquivo CSV."))
  })
}

#* Deletar um registro existente no banco de dados
#*  Fazer com que seja possível deletar mais de um id por vez
#* @param id O índice do registro a ser deletado
#* @delete /delete_record
function(id) {
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


#* Gráfico dispersão com a reta da regressão
#* @serializer png
#* @get /grafico_dispersao
function() {
  df <- readr::read_csv(csv_file, col_types = col_spec)
  
  grafico <- ggplot2::ggplot(df, aes(x = x, y = y, color = grupo)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Reta de regressão
  labs(title = "Gráfico de Dispersão com Regressão",
       x = "X",
       y = "Y",
       color = "Grupo") +
  theme_minimal()
print(grafico)
}


#* Cálculo dos coeficientes da regressão 
# Cria as variáveis dummy e utiliza o grupo A como referência 
#*@parser json
#* @serializer unboxedJSON
#* @get /estimativas_coeficientes
function() {
  df <- readr::read_csv(csv_file, col_types = col_spec)
  
  df$grupo <- as.factor(df$grupo)
  
  modelo <- lm(y ~ x + grupo, data = df)
  coeficientes <- list(intercepto = modelo$coefficients[1],
                       x = modelo$coefficients[2],
                       grupoB = modelo$coefficients[3],
                       grupoC = modelo$coefficients[4])

return(coeficientes)
}


#* Resíduos da regressão 
#*@parser json
#* @serializer unboxedJSON
#* @get /residuos
function() {
  readr::read_csv(csv_file, col_types = col_spec)
  
  residuos <- lm(y ~ x + grupo, data = df)$residuals 
  
  return(list(mensagem = "Resíduos do modelo de regressão",
    Residuos = residuos 
  ))
}



#* QQplot dos resíduos 
#* @serializer png
#* @get /grafico_residuos
function() {
  df <- readr::read_csv(csv_file, col_types = col_spec)
  
  modelo <- lm(y ~ x + grupo, data = df)
  
  grafico_res <- qqplot(y,modelo$residuals)
  
  print(grafico_res)
}



#* Predição do modelo de Regressão 
# Precisa otimizar, não colocar os números diretamente 
#* @param x Valor numérico
#* @param grupo Grupos A, B e C 
#* @parser json
#* @serializer unboxedJSON
#* @get /predicao
function(x, grupo) {
  df <- readr::read_csv(csv_file, col_types = col_spec)
  
  if (!is.null(x)) {
    if (!is.numeric(x)) {
      return(list(error = "O valor atribuído a x deve ser numérico"))
    }
  }
  
  if (!is.null(grupo)) {
    if (!grupo %in% c("A", "B", "C")) {
      return(list(error = "O grupo deve ser 'A', 'B' ou 'C'."))
    }
  }
  
  
  x <- as.numeric(x)
  
  modelo <- lm(y ~ x + grupo, data = df)
  
  predicao_df <- data.frame(x = as.vector(x), 
                            grupo = as.vector(grupo))
  
  
  predicao_valores <- predict(modelo, predicao_df)
  
  return(predicao_valores)
  }
