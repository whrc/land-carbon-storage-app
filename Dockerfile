# get shiny server plus geospatial software, see:
# https://github.com/rocker-org/rocker-versioned2/wiki/geospatial_6345e63d82d0
FROM rocker/geospatial:4.4.3

# install gcloud (and shiny server)
# https://cloud.google.com/sdk/docs/install#installation_instructions
RUN apt-get update && apt-get install -y apt-transport-https ca-certificates gnupg \
	&& echo "deb [signed-by=/usr/share/keyrings/cloud.google.gpg] https://packages.cloud.google.com/apt cloud-sdk main" | tee -a /etc/apt/sources.list.d/google-cloud-sdk.list \
	&& curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | gpg --dearmor -o /usr/share/keyrings/cloud.google.gpg \
	&& apt-get update && apt-get install -y google-cloud-cli \
	&& apt-get update && apt-get install -y shiny-server

# copy gcloud service account credentials into image, activate gcloud account, and set gcp project
COPY gcloud-key.json /opt/.config/gcloud/application_default_credentials.json
RUN gcloud auth activate-service-account --key-file=/opt/.config/gcloud/application_default_credentials.json \
	&& gcloud config set project --quiet woodwell-biomass

# install miniforge conda
RUN wget -O Miniforge3.sh "https://github.com/conda-forge/miniforge/releases/latest/download/Miniforge3-$(uname)-$(uname -m).sh" \
	&& bash Miniforge3.sh -b -p /opt/conda \
	&& rm Miniforge3.sh

# set environment variables for conda
ENV PATH="/opt/conda/bin:$PATH"
SHELL ["/bin/bash", "--login", "-c"]

# create conda environment and install earthengine and its dependencies
RUN conda create -y --name rgee_py \
	&& conda run -n rgee_py conda install -c conda-forge -y numpy earthengine-api jsbeautifier regex openssl=3.3.0

# ensure conda environment is activated in the container
ENV CONDA_DEFAULT_ENV=rgee_py
ENV PATH="/opt/conda/envs/rgee_py/bin:$PATH"

# install R packages
RUN R -e "install.packages('shiny', version = '1.9.1', repos = 'https://cloud.r-project.org')" \
	&& R -e "install.packages('shinyjs', version = '2.1.0', repos = 'https://cloud.r-project.org')" \
	&& R -e "install.packages('shinyWidgets', version = '0.8.7', repos = 'https://cloud.r-project.org')" \
	&& R -e "install.packages('shinybusy', version = '0.3.3', repos = 'https://cloud.r-project.org')" \
	&& R -e "install.packages('shinyBS', version = '0.61.1', repos = 'https://cloud.r-project.org')" \
	&& R -e "install.packages('phosphoricons', version = '0.2.1', repos = 'https://cloud.r-project.org')" \
	&& R -e "install.packages('DT', version = '0.33', repos = 'https://cloud.r-project.org')" \
	&& R -e "install.packages('viridis', version = '0.6.5', repos = 'https://cloud.r-project.org')" \
	&& R -e "install.packages('geojson', version = '0.3.5', repos = 'https://cloud.r-project.org')" \
	&& R -e "install.packages('geojsonio', version = '0.11.3', repos = 'https://cloud.r-project.org')" \
	&& R -e "install.packages('reticulate', version = '1.41.0.1', repos = 'https://cloud.r-project.org')" \
	&& R -e "install.packages('future', version = '1.34.0', repos = 'https://cloud.r-project.org')" \
	&& R -e "install.packages('googleCloudStorageR', version = '0.7.0', repos = 'https://cloud.r-project.org')" \
	&& R -e "remotes::install_github(repo = 'r-spatial/rgee@e177ad5100669771772d454709dee96a13ca9b37')" \
	&& R -e "devtools::install_github(repo = 'walkerke/mapboxer', ref = 'gljs-v2')" \
	&& rm -rf /tmp/downloaded_packages/ /tmp/*.rds

# copy shiny server configuration files into the Docker image
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
COPY app /srv/shiny-server/
COPY shiny-server.sh /usr/bin/shiny-server.sh

# set shiny server file permissions
RUN mkdir -p /var/lib/shiny-server/bookmarks \
    && chown -R shiny:shiny /var/lib/shiny-server \
    && chmod +x /usr/bin/shiny-server.sh

# expose port for shiny server
EXPOSE 8080

# set user
USER shiny

CMD ["/usr/bin/shiny-server"]
