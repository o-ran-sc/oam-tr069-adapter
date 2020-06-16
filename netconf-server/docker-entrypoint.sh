#!/bin/sh
## Warning!!! Please do not add any java arguments at the end of command. if need to add, addit before.

java -Xdebug -Xnoagent -Xrunjdwp:transport=dt_socket,address=9876,server=y,suspend=n -jar ./lib/netconf-server-1.0.0.jar --schemas-dir /opt/CSAdapter/schemas/ --debug true --starting-port ${NETCONFSRVS_START_PORT} 

