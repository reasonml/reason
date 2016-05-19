FROM ocaml/opam:debian
RUN sudo -u opam sh -c "opam depext -u merlin utop"
RUN opam remote add main https://opam.ocaml.org && \
    opam pin add -y merlin https://github.com/the-lambda-church/merlin.git#reason-0.0.1 && \
    opam pin add -y merlin_extend https://github.com/let-def/merlin-extend.git#reason-0.0.1 && \
    opam pin add -y reason https://github.com/facebook/reason.git#0.0.6
