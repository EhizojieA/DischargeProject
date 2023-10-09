FROM rocker/shiny-verse:4.3.0

ARG GITHUB_PAT
ENV GITHUB_PAT=$GITHUB_PAT

RUN rm -rf /srv/shiny-server/* && \
    touch /srv/shiny-server/.Renviron && \
    chown shiny:shiny /srv/shiny-server/.Renviron

COPY --chown=shiny:shiny src/ /srv/shiny-server/
COPY entrypoint.sh /

# use packagemanager.rstudio.com to determine necessary non-R system prerequisites to install 
# If the above tool doesn't have any SystemRequirements listed, use
#   maketools::package_sysdeps("package_name")
# on a linux system with the required R package already installed
RUN apt-get update && apt-get install -y --no-install-recommends \ 
    libstdc++6 \
    libxml2 \
    zlib1g \
    libc6 \
    libgdal30 \
    libproj22 \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# remotes::install_deps relies on the contents of src/DESCRIPTION
# and it must be copied to the container before running this command
RUN Rscript -e "install.packages(c('remotes'), repos='https://cran.us.r-project.org')" && \
    Rscript -e "remotes::install_deps('/srv/shiny-server/')"

USER shiny:shiny
EXPOSE 3838
ENTRYPOINT [ "/entrypoint.sh" ]
