FROM mambaorg/micromamba:1.4.2-bullseye

SHELL ["/bin/bash", "-o", "pipefail", "-c"]
COPY environment.yml /tmp/environment.yml

RUN micromamba create -n geopy311 -f /tmp/environment.yml --yes && \
    micromamba clean --all --yes
RUN micromamba shell init -s bash -p /opt/conda

RUN touch /home/mambauser/.bashrc
RUN echo 'micromamba activate geopy311' >> /home/mambauser/.bashrc

USER root
RUN apt-get update -y
RUN apt install git -y
RUN apt-get install -y --no-install-recommends \
    gcc \
    g++ \
    make \
    build-essential \
    zlib1g-dev \
    libpython3-dev \
    libssl-dev \
    libffi-dev \
    libsqlite3-dev \
    libgdal-dev

ARG CACHEBUST=1

RUN git clone https://github.com/springinnovate/ecoshard.git /usr/local/ecoshard && \
    cd /usr/local/ecoshard && \
    micromamba run -n geopy311 pip install . --no-build-isolation && \
    git log -1 --format="%h on %ci" > /usr/local/ecoshard.gitversion

# This shows the timedate of the ecoshard and inspring repos
RUN echo 'if [ -f "/usr/local/ecoshard.gitversion" ]; then' >> /home/mambauser/.bashrc && \
    echo '  echo "ecoshard: commit on $(cat /usr/local/ecoshard.gitversion)"' >> /home/mambauser/.bashrc && \
    echo 'fi' >> /home/mambauser/.bashrc

USER mambauser
WORKDIR /workspace
CMD ["/bin/bash"]
