_list:
    @just --list

start:
    elm-live --host 0.0.0.0 --dir public/ src/Main.elm -- --output=public/index.js
