FROM gitpod/workspace-full

ENV tz Asia/Tokyo

RUN brew install scala sbt
