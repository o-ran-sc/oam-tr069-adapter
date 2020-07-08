#!/bin/bash
# ============LICENSE_START========================================================================
# ONAP : tr-069-adapter
# =================================================================================================
# Copyright (C) 2020 CommScope Inc Intellectual Property.
# =================================================================================================
# This tr-069-adapter software file is distributed by CommScope Inc under the Apache License,
# Version 2.0 (the "License"); you may not use this file except in compliance with the License. You
# may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# This file is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
# either express or implied. See the License for the specific language governing permissions and
# limitations under the License.
# ===============LICENSE_END=======================================================================

# Run command:
# If SSL is enabled then execute : ./importConfig.sh SSL
# If SSL is not enabled then execute : ./importConfig.sh

PROTO="http"
[ "$1" = "SSL" ] && PROTO="https"

echo "Importing the Configuration..."
curl -k -F "files=@./testdata/0005B9423910.xml" $PROTO://localhost:1111/importConfig
echo -e "\n"

echo "Validating the Import..."
curl $PROTO://localhost:1111/getConfig/0005b9423910 -k
echo -e "\n"

