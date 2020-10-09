FROM fpco/stack-build:lts-16.17 as scotty-build

RUN mkdir /opt/build
COPY . /opt/build
RUN cd /opt/build && stack build --resolver lts-16.17 --system-ghc    # uses GHC available on the path (ie already installed in container)

FROM ubuntu:16.04
RUN mkdir -p /opt/scotty-app
ARG BINARY_PATH
WORKDIR /opt/scotty-app
RUN apt-get update && apt-get install -y ca-certificates libgmp-dev

 #NOTICE THIS LINE
COPY --from=scotty-build /opt/build/.stack-work/install/x86_64-linux/1e8ec7032505a1e485163fd76ac50024dbf7574e77fe77754d88390c9890eede/8.8.4/bin/ .
#EXPOSE 3000
#ENTRYPOINT ["/opt/scotty-app/scrabble-scotty-app-exe"]
