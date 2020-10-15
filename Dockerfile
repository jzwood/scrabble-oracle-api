FROM fpco/stack-build:lts-16.17 as scotty-build

RUN mkdir /opt/build
COPY . /opt/build
RUN cd /opt/build && stack build --copy-bins --resolver lts-16.17 --system-ghc    # uses GHC available on the path (ie already installed in container)

# make sure base ubuntu image is the same as fpco base image $ lsb_release -a
FROM ubuntu:18.04
RUN mkdir -p /opt/scotty-app
ARG BINARY_PATH
WORKDIR /opt/scotty-app
RUN apt-get update && apt-get install -y ca-certificates libgmp-dev

 #NOTICE THIS LINE
COPY --from=scotty-build /opt/build/.stack-work/install/x86_64-linux/9e573233521c0d3891de0a40a50043e4b941f9265713503415c2de96fd01409f/8.8.4/bin/ .
PORT=3000
EXPOSE $PORT
ENTRYPOINT ["./scrabble-scotty-app-exe"]
