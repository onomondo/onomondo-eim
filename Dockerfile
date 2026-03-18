FROM 066263928951.dkr.ecr.eu-central-1.amazonaws.com/ono-erlang:1.2.0-28 AS builder

USER root
RUN apk update && apk add --no-cache build-base

USER nonroot

COPY . .

RUN rebar3 compile
RUN rebar3 release

FROM 066263928951.dkr.ecr.eu-central-1.amazonaws.com/ono-erlang:1.2.0-28

COPY --from=builder /usr/src/app/_build/default/rel/onomondo_eim_release /usr/src/app

EXPOSE 8000 8080

CMD ["./bin/onomondo_eim_release", "foreground"]
