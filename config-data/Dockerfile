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

ENV DB_USERNAME=root
ENV DB_PASSWORD=root
ENV DB_SERVICE=tr069adapter-mariadb
ENV DB_NAME=dmsdb
ENV INITIALPNP_PORT=9000

WORKDIR /opt/CSAdapter/

ADD target/${JAR} ./lib/tr069adapter-acs-initialpnpdb.jar

EXPOSE ${INITIALPNP_PORT}
ENTRYPOINT ["java", "-jar", "./lib/tr069adapter-acs-initialpnpdb.jar"]

