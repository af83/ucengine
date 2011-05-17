# Unified Collaboration Engine

U.C.Engine (Unified Collaboration Engine) includes:

* a publish/subscribe server with persistence,
* a Javascript frontend client with a library of several realtime collaborative widgets (chat, file sharing...)
*  several backend clients that provide additional features to your application (video streaming, file converters...)

U.C.Engine allows you to build real time applications like collaboration based services, live meetings, games or anything that fits well in an event driven philosophy.

U.C.Engine is a free software project written in erlang (server) and javascript (front client). It exposes a ReSTful HTTP API with JSON as its primary exchange format.

## Build and launch

### Dependencies

* [Erlang R14A](http://erlang.org/)
* [yaws](http://yaws.hyber.org/)

See how to install dependencies in the [installation page](http://docs.ucengine.org/install.html).

### Build

    $ git clone git://github.com/AF83/ucengine.git
    $> cd ucengine
    $> make rel

If you have some problem when compiling, please see the [installation page](http://docs.ucengine.org/install.html).

### Starting U.C.Engine

    $> cd rel/ucengine
    $> bin/ucengine start

## More docs

[docs.ucengine.org](http://docs.ucengine.org/)

## License

Copyright 2011, af83

The default license for all files is AGPLv3 except priv/www under MIT/GPLv2.
