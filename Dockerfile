FROM haskell:7.10

# Adapted from https://github.com/thoughtbot/docker-heroku-haskell-stack/blob/master/Dockerfile

RUN mkdir -p /opt/server
WORKDIR /opt/server

# Setup GHC and the like
RUN stack setup

# Copy over configuration for building the app
ONBUILD COPY stack.yaml .
ONBUILD COPY *.cabal .

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
ONBUILD RUN stack build --dependencies-only -j2

# Add and Install Application Code
ONBUILD COPY . /opt/server
ONBUILD RUN stack --local-bin-path=. install -j2

# Clean up
ONBUILD RUN rm -rf /opt/server/.stack-work

CMD ["snap-example"]
