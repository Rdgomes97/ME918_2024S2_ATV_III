
# Documentação da API para Modelo de Regressão

## Visão Geral

Esta API permite a interação com um banco de dados de um modelo de regressão, possibilitando a adição, modificação e exclusão de registros, além de gerar inferências e realizar predições. A API foi construída utilizando o pacote `plumber` no R, e o banco de dados é armazenado em um arquivo CSV.

## Endpoints da API

### 1. Adicionar um novo registro

**Rota**: `/add_record`  
**Método**: POST  
**Descrição**: Adiciona um novo registro ao conjunto de dados.  
**Parâmetros**:
- `x`: Um valor numérico para a covariável.
- `grupo`: Uma variável categórica (A, B ou C).
- `y`: Um valor numérico para a variável resposta.

**Exemplo de Requisição**:

```bash
curl -X POST "http://localhost:8000/add_record?x=3.5&grupo=A&y=2.1"
```

### 2. Modificar um registro existente

**Rota**: `/modify_record`  
**Método**: PUT  
**Descrição**: Modifica um registro existente no banco de dados.  
**Parâmetros**:
- `id`: O índice do registro a ser modificado.
- `x`: (Opcional) O novo valor de `x`.
- `grupo`: (Opcional) O novo valor de `grupo` (A, B ou C).
- `y`: (Opcional) O novo valor de `y`.

**Exemplo de Requisição**:

```bash
curl -X PUT "http://localhost:8000/modify_record?id=1&x=4.0&grupo=B&y=3.2"
```

### 3. Deletar um registro existente

**Rota**: `/delete_record`  
**Método**: DELETE  
**Descrição**: Deleta um registro existente no banco de dados.  
**Parâmetros**:
- `id`: O índice do registro a ser deletado.

**Exemplo de Requisição**:

```bash
curl -X DELETE "http://localhost:8000/delete_record?id=1"
```

### 4. Obter gráfico de dispersão com reta de regressão

**Rota**: `/grafico_dispersao`  
**Método**: GET  
**Descrição**: Retorna um gráfico de dispersão dos dados com a reta de regressão ajustada.

**Exemplo de Requisição**:

```bash
curl "http://localhost:8000/grafico_dispersao" --output grafico.png
```

### 5. Estimativas dos coeficientes de regressão

**Rota**: `/estimativas_coeficientes`  
**Método**: GET  
**Descrição**: Retorna as estimativas dos coeficientes do modelo de regressão.

**Exemplo de Requisição**:

```bash
curl "http://localhost:8000/estimativas_coeficientes"
```

### 6. Obter resíduos do modelo de regressão

**Rota**: `/residuos`  
**Método**: GET  
**Descrição**: Retorna os resíduos do modelo de regressão.

**Exemplo de Requisição**:

```bash
curl "http://localhost:8000/residuos"
```

### 7. Predições do modelo de regressão

**Rota**: `/predicao`  
**Método**: GET  
**Descrição**: Retorna a predição para os valores fornecidos de `x` e `grupo`.

**Parâmetros**:
- `x`: Um valor numérico para a covariável.
- `grupo`: Um grupo categórico (A, B ou C).

**Exemplo de Requisição**:

```bash
curl "http://localhost:8000/predicao?x=5.0&grupo=B"
```
