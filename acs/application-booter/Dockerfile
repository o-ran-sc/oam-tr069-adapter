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

ENV ACS_REST_PORT=9977
ENV ACS_ADDITIONAL_REST_PORT=9975
ENV ACS_SSL_ENABLED=false
ENV CERT_FILE=/logs

ENV DB_USERNAME=root
ENV DB_PASSWORD=root
ENV DB_SERVICE=tr069adapter-mariadb
ENV DB_NAME=dmsdb
ENV MAPPER_REST_PORT=9999
ENV FACTORY_REST_PORT=8089
ENV FACTORY_REST_HOST=tr069adapter-acs-factorydb
ENV MAPPER_REST_HOST=tr069adapter-netconfig-mapper


WORKDIR /opt/CSAdapter/

ADD target/${JAR} ./lib/application-booter.jar

EXPOSE ${ACS_REST_PORT} ${ACS_ADDITIONAL_REST_PORT}
ENTRYPOINT ["java", "-jar", "./lib/application-booter.jar"]
