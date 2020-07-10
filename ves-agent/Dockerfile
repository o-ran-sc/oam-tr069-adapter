# ============LICENSE_START========================================================================
# ONAP : tr-069-adapter
# =================================================================================================
# Copyright (C) 2020 CommScope Inc Intellectual Property.
# =================================================================================================
# This tr-069-adapter software file is distributed by CommScope Inc
# under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# This file is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
# ===============LICENSE_END=======================================================================

FROM openjdk:8-alpine

ARG JAR

ENV VES_REST_PORT=8383
ENV CONFIG_NAME_SPACE=default
ENV CONFIG_MAP_NAME=tr069adapter-ves-notifier
ENV CONFIG_RELOAD_TIME=6000000
ENV faultvesCollectorHost=localhost
ENV faultvesCollectorPort=30235
ENV pnfregvesCollectorHost=localhost
ENV pnfregvesCollectorPort=30235
ENV vendorName=ORAN
ENV faultEventType=O_RAN_Alarms
ENV pnfRegEventType=O_RAN_pnfRegistration
ENV unitFamily=ORAN
ENV unitType=SmallCell
ENV faultEventSourcePrefix=SmallCell
ENV DB_SERVICE=tr069adapter-mariadb
ENV DB_NAME=dmsdb
ENV DB_USERNAME=root
ENV DB_PASSWORD=root
ENV MAPPER_REST_HOST=tr069adapter-netconfig-mapper
ENV MAPPER_REST_PORT=9999
ENV CONFIG_MAP_ENABLE=true

WORKDIR /opt/CSAdapter/

ADD target/${JAR} ./lib/ves-agent.jar
EXPOSE ${VES_REST_PORT}
ENTRYPOINT ["java", "-jar", "./lib/ves-agent.jar"]
