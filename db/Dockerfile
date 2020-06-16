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

FROM mariadb:10.4.7 

WORKDIR /opt/CSAdapter/

ADD config/CSAdapter_DB_password.txt CSAdapter_DB_password.txt
ADD config/CSAdapter_DB_user.txt CSAdapter_DB_user.txt
ADD config/setup.sql /docker-entrypoint-initdb.d/
ADD config/parameter.cnf /etc/mysql/mariadb.conf.d/parameter.cnf
