FROM ocaml/opam:ubuntu-24.04-ocaml-5.3

LABEL org.opencontainers.image.title="ripe CI"
LABEL org.opencontainers.image.description="Pre-built OCaml/opam environment for CI"

USER root

ENV OPAMROOT=/home/opam/.opam
ENV OPAMROOTISOK=1

RUN apt-get update -y && apt-get install -y nodejs python3 && rm -rf /var/lib/apt/lists/*

WORKDIR /tmp/deps
COPY ripe.opam ripe.opam
COPY ripe.opam.locked ripe.opam.locked
RUN git config --global --add safe.directory '*' \
  && opam update -y && opam install . --deps-only --locked --yes

WORKDIR /
RUN rm -rf /tmp/deps
