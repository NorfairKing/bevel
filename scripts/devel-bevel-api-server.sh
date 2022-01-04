#!/usr/bin/env bash

stack install bevel-api-server \
  --file-watch \
  --exec='./scripts/restart-bevel-api-server.sh'
