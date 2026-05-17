FROM ocaml/opam:ubuntu-24.04-ocaml-5.3

USER root

ENV OPAMROOT=/home/opam/.opam
ENV OPAMROOTISOK=1

RUN apt-get update -y && apt-get install -y nodejs && rm -rf /var/lib/apt/lists/*

WORKDIR /tmp/deps
COPY compiler/ripe.opam ripe.opam
COPY compiler/ripe.opam.locked ripe.opam.locked
RUN opam install . --deps-only --locked --yes

WORKDIR /
RUN rm -rf /tmp/deps
