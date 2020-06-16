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

ENV NETCONF_REST_PORT=8181
ENV MAPPER_REST_PORT=9999
ENV NETCONFSRVS_START_PORT=17830
ENV NETCONFSRVS_END_PORT=18030
ENV NUM_SERVERS=200
ENV MAPPER_REST_HOST=tr069adapter-netconfig-mapper
ENV DB_SERVICE=tr069adapter-mariadb
ENV DB_NAME=dmsdb
ENV DB_USERNAME=root
ENV DB_PASSWORD=root
ENV netConfServerIP=localhost

WORKDIR /opt/CSAdapter/

ADD ./schemas ./schemas

ADD ./docker-entrypoint.sh .

RUN dos2unix docker-entrypoint.sh

RUN chmod 777 docker-entrypoint.sh


ADD target/${JAR} ./lib/netconf-server-1.0.0.jar

EXPOSE ${VES_PORT}
ENTRYPOINT ["./docker-entrypoint.sh"]
