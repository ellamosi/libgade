FROM ubuntu:24.04

ARG ALIRE_VERSION=2.1.0
ARG GNATFORMAT_VERSION=25.0.0

ENV DEBIAN_FRONTEND=noninteractive
ENV PATH="/opt/alire/bin:/root/.alire/bin:${PATH}"

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        ca-certificates \
        curl \
        g++ \
        gcc \
        git \
        gnat \
        gprbuild \
        jq \
        lcov \
        python3 \
        python3-pip \
        unzip \
    && rm -rf /var/lib/apt/lists/*

RUN set -eux; \
    release_json="$(curl -fsSL "https://api.github.com/repos/alire-project/alire/releases/tags/v${ALIRE_VERSION}")"; \
    asset_url="$(printf '%s' "${release_json}" | jq -r '.assets[] | select(.name | test("linux.*zip$")) | .browser_download_url' | head -n1)"; \
    test -n "${asset_url}"; \
    curl -fsSL "${asset_url}" -o /tmp/alr.zip; \
    unzip -q /tmp/alr.zip -d /opt/alire; \
    rm -f /tmp/alr.zip; \
    alr --version

RUN alr -n install "gnatformat=${GNATFORMAT_VERSION}"

