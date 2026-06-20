
# show this
default:
    @just --list

# gleam build
test: 
    gleam test

birdie:
    gleam run -m birdie

# run an integration test
# make sure to `start-dev-server` first
integration-test: 
    gleam run -m integration/dev

# start the dev server
start-dev-server:
    mailpit &

# stop the dev server
stop-dev-server:
    pkill mailpit 


