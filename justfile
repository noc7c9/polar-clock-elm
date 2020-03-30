BUILD_FLAGS := "src/Main.elm --output=public/index.js"

_list:
    @just --list

start:
    elm-live --host 0.0.0.0 --dir public/ -- {{BUILD_FLAGS}}

build:
    elm make {{BUILD_FLAGS}}

deploy-gh-pages: build
    git worktree add gh-pages gh-pages
    rm -rf gh-pages/*
    cp -r public/* gh-pages/
    cd gh-pages/
    git commit -m "Deploy: $(git log '--format=format:%H' master -1)"
    git push origin gh-pages
    cd -
    git worktree remove gh-pages
