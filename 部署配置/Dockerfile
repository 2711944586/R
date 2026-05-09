# =============================================================================
# Dockerfile for Global Health Spending 20x Project
# - Base: rocker/shiny-verse 4.5 (含 tidyverse + Shiny + 系统字体)
# - 内置: Quarto, leaflet/sf 系统依赖, 仪表盘运行环境
# - 运行: docker run -p 3838:3838 ghs-dashboard
# =============================================================================
FROM rocker/shiny-verse:4.5.0

LABEL maintainer="Zhuang Song <20241334>"
LABEL description="Global Health Spending dashboard + Quarto narrative book"

# ---- 1. 系统依赖 -----------------------------------------------------------
RUN apt-get update && apt-get install -y --no-install-recommends \
      libxml2-dev libcurl4-openssl-dev libssl-dev \
      libudunits2-dev libgdal-dev libgeos-dev libproj-dev \
      libfontconfig1-dev libfreetype6-dev \
      libharfbuzz-dev libfribidi-dev \
      libpng-dev libtiff5-dev libjpeg-dev \
      libcairo2-dev libxt-dev \
      pandoc git curl wget gnupg unzip \
      fonts-noto-cjk fonts-liberation \
      locales \
    && locale-gen en_US.UTF-8 zh_CN.UTF-8 \
    && rm -rf /var/lib/apt/lists/*

ENV LANG en_US.UTF-8
ENV LC_ALL en_US.UTF-8

# ---- 2. Quarto -------------------------------------------------------------
ARG QUARTO_VERSION=1.5.57
RUN curl -fsSL -o /tmp/quarto.deb \
      "https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VERSION}/quarto-${QUARTO_VERSION}-linux-amd64.deb" \
    && apt-get update \
    && apt-get install -y /tmp/quarto.deb \
    && rm -f /tmp/quarto.deb \
    && rm -rf /var/lib/apt/lists/*

# ---- 3. R 包（按 安装依赖.R 的清单） ------------------------------------------
COPY 安装依赖.R /tmp/安装依赖.R
RUN Rscript -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); \
                 source('/tmp/安装依赖.R')"

# ---- 4. 项目代码 -----------------------------------------------------------
WORKDIR /srv/ghs
COPY . /srv/ghs

# ---- 5. 预热缓存（可选；失败不阻塞构建） -----------------------------------
RUN Rscript -e "tryCatch({ \
  setwd('/srv/ghs'); \
  for (f in list.files('程序', pattern='\\\\.R$', full.names=TRUE)) source(f, encoding='UTF-8'); \
  ghs <- load_ghs(); \
  master <- enrich_master(build_master_wide(ghs), with_wdi = FALSE); \
  dir.create('派生数据/处理结果', recursive=TRUE, showWarnings=FALSE); \
  saveRDS(master, '派生数据/处理结果/master_enriched.rds'); \
  cat('cache built rows=', nrow(master), '\\n') \
}, error = function(e) cat('cache build skipped:', conditionMessage(e), '\\n'))"

# ---- 6. 运行 ---------------------------------------------------------------
EXPOSE 3838
CMD ["R", "-e", "shiny::runApp('/srv/ghs/仪表盘', host='0.0.0.0', port=3838)"]
