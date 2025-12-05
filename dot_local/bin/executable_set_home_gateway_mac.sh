#!/bin/bash

CURRENT_GATEWAY_MAC=arp -a|grep -e "^_gateway "|awk '{print $4;}'

echo "$CURRENT_GATEWAY_MAC" > ~/.home_gateway_mac