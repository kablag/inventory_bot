FROM rocker/r-ver:4.5.2

RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential pkg-config cmake \
    libcurl4-openssl-dev libssl-dev libxml2-dev \
    libsqlite3-dev libicu-dev libmagick++-dev \
    && rm -rf /var/lib/apt/lists/*

# Fail the image build if any required package cannot be loaded.
RUN Rscript -e 'pkgs <- c("httr2", "jsonlite", "DBI", "RSQLite", "base64enc", "magick"); install.packages(pkgs, repos="https://mirror.truenetwork.ru/CRAN/", Ncpus=2); missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly=TRUE)]; if (length(missing)) stop("Missing R packages: ", paste(missing, collapse=", ")); cat("All R dependencies installed successfully\\n")'

WORKDIR /app
COPY bot.R /app/bot.R
CMD ["Rscript", "/app/bot.R"]
