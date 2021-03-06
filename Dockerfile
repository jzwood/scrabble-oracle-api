FROM fpco/stack-build:lts-16.17 as scotty-build

RUN mkdir /opt/build
COPY . /opt/build
WORKDIR /opt/build
RUN stack build --copy-bins --resolver lts-16.17 --system-ghc    # uses GHC available on the path (ie already installed in container)

# make sure base ubuntu image is the same as fpco base image $ lsb_release -a
FROM ubuntu:18.04
RUN mkdir -p /opt/scotty-app
WORKDIR /opt/scotty-app
RUN apt-get update && apt-get install -y ca-certificates libgmp-dev

# Copy exe
COPY --from=scotty-build /opt/build/.stack-work/install/x86_64-linux/9e573233521c0d3891de0a40a50043e4b941f9265713503415c2de96fd01409f/8.8.4/bin/ .
# Copy dictionary
COPY --from=scotty-build /opt/build/dicts/scrabble_words.txt ./dicts/scrabble_words.txt
# Copy shared-lib that postgres-simple needs
COPY --from=scotty-build /usr/lib/x86_64-linux-gnu/ /usr/lib/x86_64-linux-gnu/
COPY --from=scotty-build /lib/x86_64-linux-gnu/ /lib/x86_64-linux-gnu/

EXPOSE $PORT
ENTRYPOINT ["./scrabble-scotty-app-exe"]
