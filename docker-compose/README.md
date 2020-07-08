# O-RAN-SC docker-compose files

The tr069adapter-install.yaml and tr069adapter-install.env file will create an entire tr069-adapter system with one command:
docker-compose -f tr069adapter-install.yaml --env-file tr069adapter-install.env up -d

Howto:
cd tr069-adapter/
docker-compose -f tr069adapter-install.yaml --env-file tr069adapter-install.env up -d

The scripts in data/ will import the test data in the running system.

Run command:
If SSL is not enabled then execute
cd data/
./importConfig.sh 

If SSL is enabled then execute 
cd data/
./importConfig.sh SSL

Open link:
http://localhost:1111/getConfig/0005b9423910
or
https://localhost:1111/getConfig/0005b9423910

All the imported data is shown on the web page

## License

ONAP : tr-069-adapter
Copyright (C) 2020 CommScope Inc Intellectual Property.
This tr-069-adapter software file is distributed by CommScope Inc under the Apache License,
Version 2.0 (the "License"); you may not use this file except in compliance with the License. You
may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

This file is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
either express or implied. See the License for the specific language governing permissions and
limitations under the License.

