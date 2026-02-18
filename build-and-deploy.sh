#!/bin/bash

set -e
set -x

./build-static.sh

ssh droplet "systemctl stop datastar-activity-feed.service datastar-heap-live-fibs.service datastar-heap-live-map.service datastar-heap-simple-list.service datastar-hello-world-channel.service datastar-hello-world.service"

scp examples/*html droplet:/opt/datastar-hs/examples/

scp static-build/* droplet:/opt/datastar-hs/

ssh droplet "systemctl restart datastar-activity-feed.service datastar-heap-live-fibs.service datastar-heap-live-map.service datastar-heap-simple-list.service datastar-hello-world-channel.service datastar-hello-world.service"

