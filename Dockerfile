ARG BASE_IMAGE=ubuntu:24.04
FROM ${BASE_IMAGE} AS dev-base

ARG USERNAME=nvel-dev
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
    ca-certificates \
    gfortran \
    python3 \
    python3-venv \
    && rm -rf /var/lib/apt/lists/* \
    && if id -u ubuntu >/dev/null 2>&1; then \
        usermod -l "${USERNAME}" ubuntu \
        && groupmod -n "${USERNAME}" ubuntu \
        && usermod -d "/home/${USERNAME}" -m "${USERNAME}"; \
    fi

COPY --from=ghcr.io/astral-sh/uv:0.7 /uv /uvx /bin/

FROM dev-base AS dev
WORKDIR /workspaces/VolumeLibrary
