FROM gitpod/workspace-full

RUN brew install scala sbt tzdata

ENV TZ=Asia/Tokyo


