FROM ubuntu:24.04

ARG ALIRE_VERSION=2.1.0
ARG GNATFORMAT_VERSION=25.0.0
ARG TARGETARCH

ENV DEBIAN_FRONTEND=noninteractive
ENV PATH="/opt/alire/bin:/root/.alire/bin:${PATH}"

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        ca-certificates \
        curl \
        g++ \
        gcc \
        git \
        jq \
        lcov \
        libgmp-dev \
        python3 \
        python3-pip \
        unzip \
    && rm -rf /var/lib/apt/lists/*

RUN set -eux; \
    case "${TARGETARCH}" in \
      amd64) alire_arch='x86_64' ;; \
      arm64) alire_arch='aarch64' ;; \
      *) echo "unsupported TARGETARCH: ${TARGETARCH}" >&2; exit 1 ;; \
    esac; \
    release_json="$(curl -fsSL "https://api.github.com/repos/alire-project/alire/releases/tags/v${ALIRE_VERSION}")"; \
    asset_url="$(printf '%s' "${release_json}" | jq -r --arg arch "${alire_arch}" '.assets[] | select(.name | test("linux") and test($arch) and test("\\.zip$")) | .browser_download_url' | head -n1)"; \
    test -n "${asset_url}"; \
    curl -fsSL "${asset_url}" -o /tmp/alr.zip; \
    unzip -q /tmp/alr.zip -d /opt/alire; \
    chmod +x /opt/alire/bin/alr; \
    rm -f /tmp/alr.zip; \
    alr --version

RUN alr -n install gnat_native gprbuild \
    && alr -n install "gnatformat=${GNATFORMAT_VERSION}"
