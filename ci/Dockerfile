FROM ocaml/opam:ubuntu-24.04-ocaml-5.5

LABEL org.opencontainers.image.title="ripe CI"
LABEL org.opencontainers.image.description="Pre-built OCaml/opam environment for CI"

USER root

ENV OPAMROOT=/home/opam/.opam
ENV OPAMROOTISOK=1

RUN apt-get update -y && apt-get install -y nodejs python3 && rm -rf /var/lib/apt/lists/*

RUN ln -f /usr/bin/opam-2.6 /usr/bin/opam && opam init --reinit -ni

WORKDIR /tmp/deps
COPY ripe.opam ripe.opam
COPY ripe.opam.locked ripe.opam.locked
RUN opam repository set-url default https://opam.ocaml.org \
  && opam update -y && opam install . --deps-only --locked --with-test --with-dev-setup --yes

WORKDIR /
RUN rm -rf /tmp/deps
