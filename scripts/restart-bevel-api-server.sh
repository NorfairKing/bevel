#!/usr/bin/env bash


killall bevel-api-server || true

bevel-api-server &
