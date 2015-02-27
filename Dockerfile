FROM debian:stable

MAINTAINER François de Metz <francois@stormz.me>

RUN apt-get update
RUN apt-get install -y erlang erlang-yaws make git
