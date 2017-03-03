This is the source code of bspack, the necessary parts copied over from https://github.com/bloomberg/bucklescript/tree/master/jscomp/bin.

bspack itself is bundled using bspack; if you want to make a contribution to it, please head over to BuckleScript! We're only copying over the bundled bspack.ml, and the other parts needed to compile the final `bspack` binary (we can't check in the binary instead, as it's not platform-independent).

You don't have to manually build this; `../reason_bspack.sh` takes care of it.

In the future, we should probably have bspack exposed as a freely installable utility,
