# Chomp Game

[![](https://img.shields.io/badge/rebar3-3.15.1-brown)](https://www.rebar3.org/)
[![](https://img.shields.io/badge/jinterface-1.6.1-yellow)](https://mvnrepository.com/artifact/org.erlang.otp/jinterface)
[![](https://img.shields.io/badge/JavaFX_Graphics-15.0.1-green)](https://mvnrepository.com/artifact/org.openjfx/javafx-graphics)

Project showcasing usage of jinterface - library made for connecting erlang application with java. Rules of the game can be found [here](https://en.wikipedia.org/wiki/Chomp).

## [Chomp Api](./chompApi/src)
Erlang gen_server application.

### Run server
```
cd chompApi 
rebar3 shell --sname apiNode --setcookie erljava
```

## [ChompClient](./chompClient/src/main/java)
Java application responsible for communication with erlang server and visualization of game.

## Showcase

![](res/2021-05-06-14-04-08.png)

![](res/2021-05-06-14-05-35.png)

![](res/2021-05-06-14-05-59.png)
