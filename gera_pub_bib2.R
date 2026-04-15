# --- Carregar bibliotecas necessárias ---
library(biblio)
library(glue)

# --- Configurações ---
bib_file_path <- "INTERAS_20260415_ate202604.bib"
output_dir <- "publicacoes/artigos"

# --- Lógica do Script ---

# 1. Criar diretório de saída
if (!dir.exists(output_dir)) {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  cat("Diretório '", output_dir, "' criado com sucesso.\n")
} else {
  unlink(glue(output_dir,"/*.qmd"))
  cat("Arquivos .qmd antigos removidos\n")
}

# 2. Ler arquivo .bib
cat("Lendo o arquivo de bibliografia:", bib_file_path, "\n")
tryCatch({
  pubs_df <- read_bib(bib_file_path)
}, error = function(e) {
  stop("Erro ao ler o arquivo .bib. Detalhes: ", e$message)
})

names(pubs_df) <- toupper(names(pubs_df))

cat("Arquivo lido com sucesso. Total de", nrow(pubs_df), "publicações encontradas.\n")
cat("Iniciando o processamento...\n\n")

# 3. Iterar sobre cada publicação
for (i in 1:nrow(pubs_df)) {
  
  pub <- pubs_df[i, ]
  filename_key <- pub$BIBTEXKEY
  
  if (is.na(filename_key) || nchar(filename_key) == 0) {
    cat("AVISO: Publicação na linha", i, "sem BIBTEXKEY. Pulando.\n")
    next
  }
  
  output_qmd_path <- file.path(output_dir, paste0(filename_key, ".qmd"))
  
  if (file.exists(output_qmd_path)) {
    cat("O arquivo '", basename(output_qmd_path), "' já existe. Pulando.\n")
    next 
  }
  
  # --- Função de Limpeza e Conversão de LaTeX para HTML ---
  sanitize_for_yaml <- function(text) {
    if (is.na(text) || nchar(text) == 0) return("")
    
    # Etapa 1: Conversões específicas de LaTeX para HTML
    text <- gsub("\\$\\^\\{(?:\\\\textrm\\{)?(.*?)\\}?\\}\\$", "<sup>\\1</sup>", text, perl = TRUE)
    text <- gsub("\\$\\_\\{(?:\\\\textrm\\{)?(.*?)\\}?\\}\\$", "<sub>\\1</sub>", text, perl = TRUE)
    
    # Etapa 2: Tratamento de comandos LaTeX e caracteres de escape comuns
    text <- gsub("\\\\textless\\s*\\{?\\}?", "<", text)
    text <- gsub("\\\\textgreater\\s*\\{?\\}?", ">", text)
    text <- gsub("\\\\&", "&", text)
    text <- gsub("\\\\%", "%", text)
    
    # Etapa 3: Limpeza geral de caracteres BibTeX remanescentes
    text <- gsub("\\{|\\}", "", text)      # Remove chaves órfãs (Resolve o problema do {McGrail})
    text <- gsub("\\\\", "", text)         # Remove barras órfãs
    text <- gsub("\"", "'", text)          # Padroniza aspas para não quebrar o YAML
    text <- gsub("\\s+", " ", trimws(text)) # Normaliza espaços e quebras de linha múltiplas
    
    return(text)
  }
  
  # --- Preparação dos Metadados ---
  
  title <- sanitize_for_yaml(pub$TITLE)
  abstract <- ifelse("ABSTRACT" %in% names(pub) && !is.na(pub$ABSTRACT), sanitize_for_yaml(pub$ABSTRACT), "Resumo não disponível.")
  
  # AQUI ESTÁ A CORREÇÃO PRINCIPAL: Aplicando a sanitização nos autores
  raw_author <- ifelse("AUTHOR" %in% names(pub) && !is.na(pub$AUTHOR), pub$AUTHOR, "Autor Desconhecido")
  authors_list <- unlist(strsplit(raw_author, " and "))
  authors_list <- sapply(authors_list, sanitize_for_yaml, USE.NAMES = FALSE) # Limpa as chaves e sujeiras de cada autor
  authors_yaml <- paste0("  - name: ", authors_list, collapse = "\n")
  
  date_formatted <- NA
  if ("DATE" %in% names(pub) && !is.na(pub$DATE)) {
    date_field <- pub$DATE
    if (grepl("^\\d{4}-\\d{2}-\\d{2}$", date_field)) date_formatted <- date_field
    else if (grepl("^\\d{4}-\\d{2}$", date_field)) date_formatted <- paste0(date_field, "-01")
    else if (grepl("^\\d{4}$", date_field)) date_formatted <- paste0(date_field, "-01-01")
  }
  if (is.na(date_formatted)) {
    date_formatted <- ifelse("YEAR" %in% names(pub) && !is.na(pub$YEAR), paste0(pub$YEAR, "-01-01"), as.character(Sys.Date()))
  }
  
  bibtype <- ifelse("BIBTYPE" %in% names(pub) && !is.na(pub$BIBTYPE), pub$BIBTYPE, "misc")
  
  iniciohtml <- "{=html}"
  
  # --- Construção Dinâmica do Bloco 'citation' ---
  
  citation_parts <- list()
  
  container_title <- sanitize_for_yaml(ifelse("JOURNALTITLE" %in% names(pub) && !is.na(pub$JOURNALTITLE), pub$JOURNALTITLE, ""))
  if(nchar(container_title) > 0) {
    citation_parts$container <- glue('  container-title: "{container_title}"')
  }
  
  volume <- ifelse("VOLUME" %in% names(pub) && !is.na(pub$VOLUME), sanitize_for_yaml(pub$VOLUME), "")
  if(nchar(volume) > 0) {
    citation_parts$volume <- glue('  volume: {volume}')
  }
  
  issue <- ifelse("NUMBER" %in% names(pub) && !is.na(pub$NUMBER), sanitize_for_yaml(pub$NUMBER), "")
  if(nchar(issue) > 0) {
    citation_parts$issue <- glue('  issue: {issue}')
  }
  
  doi <- ifelse("DOI" %in% names(pub) && !is.na(pub$DOI), sanitize_for_yaml(pub$DOI), "")
  if(nchar(doi) > 0) {
    citation_parts$doi <- glue('  doi: "{doi}"')
  }
  
  citation_yaml <- ""
  if (length(citation_parts) > 0) {
    citation_content <- paste(unlist(citation_parts), collapse = "\n")
    citation_yaml <- paste0("citation: \n", citation_content)
  }
  
  # --- Template YAML Final ---
  
  qmd_content <- glue::glue('---
title: "{title}"
date: {date_formatted}
author:
{authors_yaml}
abstract: > 
  {abstract}
categories: [{bibtype}]
{citation_yaml}
---

```{iniciohtml}
<nav aria-label="breadcrumb">
  <ol class="breadcrumb">
    <li class="breadcrumb-item"><a href="../../index.html">Home</a></li>
    <li class="breadcrumb-item"><a href="../index.html">Publicações</a></li>
    <li class="breadcrumb-item active" aria-current="page">{title}</li>
  </ol>
</nav>
')
  
  # 4. Escrever o conteúdo no arquivo .qmd
  con <- file(output_qmd_path, "w", encoding = "UTF-8")
  writeLines(qmd_content, con, useBytes = TRUE)
  close(con)
  
  cat("Arquivo '", basename(output_qmd_path), "' criado com sucesso.\n")
}

cat("\nProcessamento concluído.\n")
