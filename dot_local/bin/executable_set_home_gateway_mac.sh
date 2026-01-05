#!/bin/bash

CURRENT_GATEWAY_MAC=`arp fritz.box | grep fritz.box | awk '{print $3;}'`

echo "$CURRENT_GATEWAY_MAC" > ~/.home_gateway_mac