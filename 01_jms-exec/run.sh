#!/bin/sh


cabal install --overwrite-policy=always

/home/zaku/.cabal/bin/jms-exec -y ./jms-exec.yaml


