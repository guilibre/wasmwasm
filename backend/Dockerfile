FROM debian:bookworm-slim AS build

RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    ca-certificates \
    cmake \
    git \
    libasio-dev \
    libssl-dev \
    pkg-config \
    zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /src

COPY . .
RUN cmake -B /build --preset release
RUN cmake --build /build -- -j$(nproc)

FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y --no-install-recommends \
    libgmp10 ca-certificates unixodbc libaio1 \
    && rm -rf /var/lib/apt/lists/*

COPY vendor/instantclient /opt/oracle/instantclient
COPY odbcinst.ini /etc/odbcinst.ini
ENV LD_LIBRARY_PATH=/opt/oracle/instantclient

COPY --from=build /build/backwasm /usr/local/bin/backwasm

EXPOSE 8080

CMD ["backwasm"]
