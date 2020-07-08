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

ENV SBI_REST_HOST=tr069adapter-acs
ENV SBI_REST_PORT=9977
ENV CONFIG_DB_REST_HOST=tr069adapter-acs-initialpnpdb
ENV CONFIG_DB_REST_PORT=9000
ENV MAPPER_REST_PORT=9999
ENV REQUEST_TIMEOUT=300
ENV preConfigureOnPNP=true
ENV ALARM_MO_REGEX=(.)*.FaultMgmt.ExpeditedEvent.(.)*
ENV NBI_REST_HOST=tr069adapter-netconf-server
ENV NBI_REST_PORT=8181
ENV VES_REST_HOST=tr069adapter-ves-notifier
ENV VES_REST_PORT=8383
ENV netConfServerPort=17830
ENV netConfServerIP=localhost
ENV DB_SERVICE=tr069adapter-mariadb
ENV DB_NAME=dmsdb
ENV DB_USERNAME=root
ENV DB_PASSWORD=root

WORKDIR /opt/CSAdapter/

ADD target/${JAR} ./lib/mapper.jar
ADD ./mapper-schema ./mapper-schema
ADD ./error-code-mapping.json ./error-code-mapping.json


ENTRYPOINT ["java", "-jar", "./lib/mapper.jar"]
