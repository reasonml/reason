# Dockerfile for Reason

A Docker file for Reason development.

## Build

* Ensure that you have docker [installed](https://docs.docker.com/engine/installation/).
* Build the image: `docker build -t reason .`

All set!

## Running

### Reason interactive top-level

Start the Reason interactive top-level with:

    $ docker run -it reason rtop

### Build native apps

Assuming that you have a reason project directory called `hello_reason` with a
single file `hello.re`:

```
$ cat hello.re
print_string "Hello, Reason!\n"
```

To build the `hello` native app:

    $ cd hello_reason
    $ docker run -it -v `pwd`:/src reason
    $ cd /src
    $ rebuild hello.native
    $ ./hello.native
    Hello, Reason!

You can further edit your source file from the host machine and rebuild the
native app from your docker container. The build artifacts are found in your
host machines `hello_reason` directory.
