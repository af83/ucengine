FROM debian:stable
MAINTAINER Laurent Arnoud <laurent@spkdev.net>

ENV DEBIAN_FRONTEND noninteractive
ENV DEBIAN_PRIORITY critical
ENV DEBCONF_NOWARNINGS yes

RUN groupadd -r ucengine && useradd --home-dir /data -r -g ucengine ucengine

RUN apt-get update && apt-get -y upgrade \
    && apt-get install -y openssl erlang erlang-yaws --no-install-recommends

ENV ERL_LIBS /usr/lib/yaws/

RUN mkdir /code
WORKDIR /code
COPY . /code
RUN chown -R ucengine:ucengine /code

RUN mkdir /data && chown ucengine:ucengine /data
VOLUME /data

RUN buildDeps='make git'; \
    set -x \
    && apt-get update && apt-get install -y $buildDeps --no-install-recommends \
    && su ucengine -c "make rel" \
    && apt-get purge -y $buildDeps \
    && apt-get autoremove -y \
    && apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

WORKDIR /code/rel/ucengine

USER ucengine

EXPOSE 5280

CMD ["./bin/ucengine", "run"]
