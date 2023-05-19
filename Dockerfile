# tor container to run OF with tor 1:1 in single container

FROM erlang:26-alpine as build

# Set working directory
RUN mkdir /buildroot
WORKDIR /buildroot

# Copy our Erlang test application
COPY rebar.config .
COPY src/ src/

# And build the release
RUN rebar3 as prod release

FROM alpine:3.16

LABEL maintainer="valtteri@shufflingbytes.com"

RUN apk add --no-cache \
    bash \
    tor \
    socat \
    curl \
    openssl \
    ncurses-libs \
    libstdc++

# Set working directory
RUN mkdir /onionfermenter
WORKDIR /onionfermenter

RUN chown tor:nogroup /onionfermenter

COPY deploy/torrc /etc/tor/torrc
COPY deploy/run.sh run.sh

# Install the released application
COPY --from=build /buildroot/_build/prod/rel/onionfermenter /onionfermenter

USER tor

VOLUME /var/lib/tor/hidden_service

CMD [ "./run.sh" ]
