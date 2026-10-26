# STREAMGUARD | Analise exploratoria e KPIs | Dados sinteticos

if (!requireNamespace('ggplot2', quietly = TRUE))
  stop('Instale ggplot2 primeiro.')
library(ggplot2)
options(scipen = 8)
pasta <- if (file.exists('metricas_detalhadas.csv'))
  '.'
else
  'streamguard_analise'
ler <- function(nome)
  read.csv(file.path(pasta, nome),
           stringsAsFactors = FALSE,
           check.names = FALSE)
d <- ler('metricas_detalhadas.csv')
j <- ler('resumo_metricas.csv')
p <- ler('resumo_processos.csv')
exigir <- function(df, colunas, nome)
  if (!all(colunas %in% names(df)))
    stop(paste('Colunas ausentes:', nome))
exigir(d,
       c('servidor_id', 'timestamp', 'tipo', 'valor', 'unidade'),
       'detalhes')
exigir(
  j,
  c(
    'servidor_id',
    'inicio_periodo',
    'tipo',
    'unidade',
    'quantidade_amostras',
    'media_amostral',
    'minimo',
    'maximo'
  ),
  'resumo metricas'
)
exigir(
  p,
  c(
    'servidor_id',
    'inicio_periodo',
    'pid',
    'nome',
    'quantidade_amostras',
    'cpu_media_amostral',
    'cpu_maxima',
    'ram_media_bytes',
    'ram_maxima_bytes'
  ),
  'processos'
)
fuso <- 'America/Sao_Paulo'
parse_data <- function(x)
  as.POSIXct(x, format = '%Y-%m-%d %H:%M:%S', tz = fuso)
d$timestamp <- parse_data(d$timestamp)
j$inicio_periodo <- parse_data(j$inicio_periodo)
p$inicio_periodo <- parse_data(p$inicio_periodo)
stopifnot(
  !anyNA(d),
  !anyNA(j),
  !anyNA(p),
  all(is.finite(d$valor)),!anyDuplicated(d[c('servidor_id', 'timestamp', 'tipo')]),!anyDuplicated(j[c('servidor_id', 'inicio_periodo', 'tipo', 'unidade')]),!anyDuplicated(p[c('servidor_id', 'inicio_periodo', 'pid', 'nome')])
)
unidades <- c(
  USO_CPU = '%',
  USO_RAM = '%',
  USO_DISCO = '%',
  RAM_DISPONIVEL = 'bytes',
  TAXA_ENVIO_REDE = 'bytes/s',
  TAXA_RECEBIMENTO_REDE = 'bytes/s',
  TAXA_LEITURA_DISCO = 'bytes/s',
  TAXA_ESCRITA_DISCO = 'bytes/s',
  CARGA_CPU_1M = 'load',
  CARGA_CPU_5M = 'load',
  CARGA_CPU_15M = 'load'
)
stopifnot(
  all(d$tipo %in% names(unidades)),
  all(d$unidade == unidades[d$tipo]),
  
  all(d$valor[d$unidade == '%'] >= 0 &
        d$valor[d$unidade == '%'] <= 100),
  all(d$valor[d$unidade != '%'] >= 0),
  all(j$quantidade_amostras > 0),
  all(p$quantidade_amostras > 0)
)
d$servidor_id <- factor(d$servidor_id)
j$servidor_id <- factor(j$servidor_id)
p$servidor_id <- factor(p$servidor_id)
d$tipo <- factor(d$tipo)
d$unidade <- factor(d$unidade)
cat('\n=== VISAO GERAL ===\n')
str(d)
summary(d)
print(table(d$servidor_id, d$tipo))

cat('Registros:', nrow(d), nrow(j), nrow(p), '\n')
print(range(d$timestamp))
# Estatisticas por servidor e metrica (dados individuais, nao medias de janelas)
moda_exata <- function(x) {
  f <- table(x)
  if (max(f) <= 1)
    return(NA_real_)
  as.numeric(names(f)[which.max(f)])
}
calc <- function(x)
  data.frame(
    n = length(x),
    media = mean(x),
    mediana = median(x),
    moda = moda_exata(x),
    minimo = min(x),
    maximo = max(x),
    amplitude = diff(range(x)),
    variancia = var(x),
    dp = sd(x),
    q1 = unname(quantile(x, .25)),
    q3 = unname(quantile(x, .75)),
    p90 = unname(quantile(x, .9)),
    p95 = unname(quantile(x, .95)),
    iqr = IQR(x)
  )
partes <- split(d, list(d$servidor_id, d$tipo, d$unidade), drop = TRUE)
estatisticas <- do.call(rbind, lapply(partes, function(z)
  cbind(
    servidor_id = as.character(z$servidor_id[1]),
    tipo = as.character(z$tipo[1]),
    unidade = as.character(z$unidade[1]),
    calc(z$valor)
  )))
row.names(estatisticas) <- NULL
print(estatisticas)
write.csv(estatisticas, 'estatisticas_streamguard.csv', row.names = FALSE)
# KPI: percentual de AMOSTRAS acima da referencia; nao e duracao de indisponibilidade
limites <- c(USO_CPU = 90,
             USO_RAM = 90,
             USO_DISCO = 95)
kp <- subset(d, as.character(tipo) %in% names(limites))
kp$limite <- limites[as.character(kp$tipo)]
kp$excedeu <- kp$valor >= kp$limite
kpis <- aggregate(
  as.integer(kp$excedeu),
  by = list(servidor_id = kp$servidor_id, tipo = kp$tipo),
  FUN = function(x)
    c(
      amostras = length(x),
      excedencias = sum(x),
      percentual = 100 * mean(x)
    )
)
kpis <- data.frame(kpis[1:2], kpis$x)
print(kpis)
write.csv(kpis, 'kpis_streamguard.csv', row.names = FALSE)
# Graficos 
for (t in c('USO_CPU', 'USO_RAM', 'USO_DISCO')) {
  x <- droplevels(subset(d, tipo == t))
  lim <- limites[t]
  print(
    ggplot(x, aes(timestamp, valor, color = servidor_id)) + geom_line(linewidth =
                                                                        .35) +
      geom_hline(
        yintercept = lim,
        linetype = 'dashed',
        color = 'firebrick'
      ) +
      labs(
        title = paste('Serie temporal:', t),
        x = 'Horario',
        y = 'Utilizacao (%)',
        color = 'Servidor'
      ) + theme_minimal()
  )
  print(
    ggplot(x, aes(valor, fill = servidor_id)) + geom_histogram(
      bins = 35,
      alpha = .6,
      position = 'identity'
    ) +
      labs(
        title = paste('Distribuicao:', t),
        x = 'Utilizacao (%)',
        y = 'Frequencia',
        fill = 'Servidor'
      ) + theme_minimal()
  )
  print(
    ggplot(x, aes(servidor_id, valor, fill = servidor_id)) + geom_boxplot() +
      labs(
        title = paste('Dispersao:', t),
        x = 'Servidor',
        y = 'Utilizacao (%)'
      ) + theme_minimal()
  )
}
# Rede em Mbit/s (conversao de bytes/s)
rede <- droplevels(subset(d, tipo %in% c(
  'TAXA_ENVIO_REDE', 'TAXA_RECEBIMENTO_REDE'
)))
rede$mbps <- rede$valor * 8 / 1e6
print(
  ggplot(rede, aes(timestamp, mbps, color = servidor_id)) + geom_line(linewidth =
                                                                        .3) +
    facet_wrap( ~ tipo, scales = 'free_y') + labs(
      title = 'Trafego de rede',
      x = 'Horario',
      y = 'Mbit/s',
      color = 'Servidor'
    ) + theme_minimal()
)
# Janelas de 5 minutos: medias e maximos (nao substituir detalhes por agregados)
jan <- subset(j, tipo %in% c('USO_CPU', 'USO_RAM', 'USO_DISCO'))
print(
  ggplot(jan, aes(inicio_periodo, media_amostral, color = servidor_id)) +
    geom_line() +
    facet_wrap( ~ tipo, ncol = 1) + labs(
      title = 'Medias por janela de 5 minutos',
      x = 'Inicio da janela',
      y = 'Media (%)',
      color = 'Servidor'
    ) + theme_minimal()
)
# Fases do gerador: contexto conhecido, nao coluna original; recuperacao comparada ao critico
jan$minutos <- as.numeric(difftime(jan$inicio_periodo, min(jan$inicio_periodo), units =
                                     'mins'))
jan$fase <- c('normal', 'elevada', 'critica', 'recuperacao')[(floor(jan$minutos /
                                                                      5) %% 4) + 1]
fases <- aggregate(
  jan$media_amostral,
  list(
    servidor_id = jan$servidor_id,
    tipo = jan$tipo,
    fase = jan$fase
  ),
  mean
)
names(fases)[4] <- 'media_das_janelas'
print(fases)
# Processos: media ponderada por quantidade de amostras
proc <- aggregate(seq_len(nrow(p)), list(servidor_id = p$servidor_id, nome =
                                           p$nome), function(idx) {
                                             z <- p[idx, ]
                                             c(
                                               cpu_media = weighted.mean(z$cpu_media_amostral, z$quantidade_amostras),
                                               ram_media_GiB = weighted.mean(z$ram_media_bytes, z$quantidade_amostras) /
                                                 1024^3,
                                               cpu_pico = max(z$cpu_maxima)
                                             )
                                           })
proc <- data.frame(proc[1:2], proc$x)
print(proc)
print(
  ggplot(proc, aes(
    reorder(nome, cpu_media), cpu_media, fill = servidor_id
  )) + geom_col(position = 'dodge') + coord_flip() +
    labs(
      title = 'CPU media dos processos (convencao multicore)',
      x = 'Processo',
      y = 'CPU media de processo (%)',
      fill = 'Servidor'
    ) + theme_minimal()
)
# Correlacoes: somente amostras no mesmo instante e mesmo servidor
selecionadas <- c('USO_CPU',
                  'USO_RAM',
                  'USO_DISCO',
                  'TAXA_ENVIO_REDE',
                  'TAXA_RECEBIMENTO_REDE')
correlacoes <- list()
modelos <- list()
for (s in levels(d$servidor_id)) {
  z <- subset(d, servidor_id == s & tipo %in% selecionadas)
  wide <- reshape(z[c('timestamp', 'tipo', 'valor')],
                  idvar = 'timestamp',
                  timevar = 'tipo',
                  direction = 'wide')
  names(wide) <- sub('^valor\\.', '', names(wide))
  vars <- intersect(selecionadas, names(wide))
  mat <- matrix(NA_real_, length(vars), length(vars), dimnames = list(vars, vars))
  pares <- mat
  for (a in vars)
    for (b in vars) {
      ok <- complete.cases(wide[c(a, b)])
      pares[a, b] <- sum(ok)
      if (sum(ok) >= 3 &&
          sd(wide[[a]][ok]) > 0 &&
          sd(wide[[b]][ok]) > 0)
        mat[a, b] <- cor(wide[[a]][ok], wide[[b]][ok], method = 'pearson')
    }
  cat('\nServidor', s, '- Pearson:\n')
  print(round(mat, 3))
  cat('Pares validos:\n')
  print(pares)
  correlacoes[[s]] <- list(correlacao = mat, pares = pares)
  xy <- wide[complete.cases(wide[c('USO_CPU', 'USO_RAM')]), c('USO_CPU', 'USO_RAM')]
  print(
    ggplot(xy, aes(USO_CPU, USO_RAM)) + geom_point(alpha = .2) + geom_smooth(method =
                                                                               'lm', se = TRUE) +
      labs(
        title = paste('CPU x RAM - Servidor', s),
        x = 'CPU (%)',
        y = 'RAM (%)'
      ) + theme_minimal()
  )
  modelo <- lm(USO_RAM ~ USO_CPU, data = xy)
  modelos[[s]] <- modelo
  cat('Regressao servidor', s, '\n')
  print(summary(modelo))
  print(coef(modelo))
  cat('R2:', summary(modelo)$r.squared, '\n')
  print(data.frame(
    observado = xy$USO_RAM,
    previsto = fitted(modelo),
    residuo = residuals(modelo)
  )[1:5, ])
  print(
    ggplot(
      data.frame(previsto = fitted(modelo), residuo = residuals(modelo)),
      aes(previsto, residuo)
    ) + geom_point(alpha = .2) + geom_hline(yintercept = 0, linetype = 'dashed') +
      labs(
        title = paste('Residuos - Servidor', s),
        x = 'RAM prevista (%)',
        y = 'Residuo (p.p.)'
      ) + theme_minimal()
  )
  novo <- data.frame(USO_CPU = median(xy$USO_CPU))
  print(predict(
    modelo,
    newdata = novo,
    interval = 'prediction',
    level = .95
  ))
}
# Amostragem aleatoria simples
cpu <- subset(d, tipo == 'USO_CPU' &
                servidor_id == levels(d$servidor_id)[1])$valor
cat('Media populacao finita CPU:', mean(cpu), '\n')
for (n in c(30, 100, 300))
  cat('Amostra', n, 'media', mean(sample(cpu, n, replace = FALSE)), '\n')
# Distribuicoes: diagnostico normal e proporcoes observadas
for (s in levels(d$servidor_id)) {
  x <- subset(d, tipo == 'USO_CPU' & servidor_id == s)$valor
  cat(
    'CPU servidor',
    s,
    'media',
    mean(x),
    'dp',
    sd(x),
    'assimetria aproximada',
    mean((x - mean(x))^3) / sd(x)^3,
    '\n'
  )
  print(
    ggplot(data.frame(x = x), aes(x)) + geom_histogram(aes(y = after_stat(density)), bins =
                                                         35) +
      stat_function(
        fun = dnorm,
        args = list(mean = mean(x), sd = sd(x)),
        color = 'red'
      ) +
      labs(
        title = paste('Normal apenas como referencia visual -', s),
        x = 'CPU (%)',
        y = 'Densidade'
      ) + theme_minimal()
  )
}
cat(
  '\nBinomial: proporcoes de excedencia sao DESCRITIVAS; amostras temporais nao independentes.\n'
)
cat(
  'Poisson/exponencial: NAO ajustadas, pois ciclos periodicos nao sustentam taxa homogenea e independencia.\n'
)
cat(
  'LIMITACOES: dados sinteticos; nao ha FPS, travamentos ou falhas medidas. Correlacao e R2 nao provam causalidade.\n'
)
