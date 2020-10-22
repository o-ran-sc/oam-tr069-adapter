.. This work is licensed under a Creative Commons Attribution 4.0 International License.
.. SPDX-License-Identifier: CC-BY-4.0
.. Copyright (C) 2020 CommScope Inc.

TR-069 Adapter Developer Guide
==============================

This document provides a quick start for developers of the O-RAN SC TR-069 Adapter

Prerequisites
-------------

1. Java development kit (JDK), version 8 or later
2. Maven dependency-management tool, version 3.4 or later


Clone and Update
----------------

After cloning the repository, update the repository to latest fetch head


Build Application
-----------------
TR-069 Adapter has 8 docker applications. 

++
db
++

The Maria DB container instance is used to support data persistence for the TR-069Adapter covering initial PnP Configuration Data, TR-069 Session Data and Device Connection Request URL information.

+++
acs
+++

The Auto Configuration Service (ACS) Module is used to communicate with the Device using the TR-069 protocol. it contains the core tr069 layer split into 3 modules 
	cpe: This module is responsible for sending http request/response to device.
	nbi: This module is responsible for interacting with mapper module
	requestprocessor: this module is responsible for processing the informs/nbi request from mapper and handing over to respective cpe/nbi modules

+++++++
factory
+++++++

This module supports Rest APIs to authenticate Device TR-069 sessions using basic authentication.
As of now it's dummy implementation, user can extend the code of FactoryDataController.java to support as per user requirements. For Example: If user wants to authenticate with DB, the extended class can implement DAO layer and verify in the database.

+++++++++++
config-data
+++++++++++

This module supports importing 3GPP Configuration XML (3GPP TS 32.594) for Device. The configurations from XML are used to provision as initial PnP parameters to devices as part of BootStrap. This module also supports REST APIs to import Config XML which contains device Initial PnP Configurations. The initial PnP Parameters are optional and could be used to emulate PnP using TR-069Adapter.

++++++
mapper
++++++

The main function of this module is to map the request between NETCONF server and ACS (conversion of NETCONF protocol specific message to TR-069 message and vice versa) and to forward notifications to the VES Notifier.

Mapper module will have a mapping file in CSV format which contains a mapping between TR069 MO name and its equivalent Yang MO name, MO type and equivalent TR069 data type.

$tr069-adapter/mapper/mapper-schema# tree

| base
|  ├── CSDMYangMONameMapping.txt
|  └── CustomYangModelMOmapping.txt

The mapping file holds TRMONAME,YANGMONAME,MOTYPE,TRDATATYPE. Sample content is as shown below:

TRMONAME,YANGMONAME,MOTYPE,TRDATATYPE
Device.,device,ScalarObject,object,urn:onf:otcc:wireless:yang:radio-access-196
Device.DeviceInfo.,device.device-info,ScalarObject,object
Device.DeviceInfo.DeviceCategory,device.device-info.device-category,Scalar-ReadOnly,string
Device.DeviceInfo.Manufacturer,device.device-info.manufacturer,Scalar-ReadOnly,string
Device.DeviceInfo.ManufacturerOUI,device.device-info.manufacturer-oui,Scalar-ReadOnly,string
Device.Services.FAPService.{i}.,device.services.fap-service.{i}.,TabularObject,object,urn:onf:otcc:wireless:yang:radio-access
Device.Services.FAPService.{i}.Alias,device.services.fap-service.{i}.alias,Tabular-ReadWrite,string
Device.Services.FAPService.{i}.DeviceType,device.services.fap-service.{i}.device-type,Tabular-ReadOnly,string

Note: To retrieve different namespace, provide namespace value as an additional argument in corresponding node in the mapping file. 

The mapping file location will have to be specified in 'profile-definition-mapping.json' as "csdm-mapping-dir-path" based on the software version. During the mapper startup it will load the mapping files defined 'profile-definition-mapping.json'.

profile-definition-mapping.json
.. jsonschema::

    {
        "profileDefinition":[{
			"profile-id":"base",
			"software-version":"*",
			"hardware-version":"*",
			"netconf-schema-dir-path":"base",
			"csdm-mapping-dir-path":"base"
		}]
    }
	
Mapper module exposes REST-API on the NBI to receive netconf rpc requests 
Mapper module also exposes REST-API on the SBI to receive acs notifications 

++++++++++++++
netconf-server
++++++++++++++

A NETCONF compliant server is used to communicate to SDN-R’s NetConf Client. It internally uses the Yang Model built based on TR-196 and TR-181. 

This module will create the netconf instances when it receives BOOTSTRAP from mapper module. While starting the the netconf instance, it will load the yang models from the path specified in "netconf-schema-dir-path" attribute in 'profile-definition-mapping.json' file based on software version. All common yang models will be placed "common" directory and version specific models under version based directory.

$ tr069-adapter/netconf-server/schemas# tree

|	├── base
|	│   ├── bbf-tr-196-2-0-3-full.yang
|	│   ├── tr-069-cwmp-notification.yang
|	│   └── tr-181-2-12-0-cwmp-full.yang
|	└── common
|		├── iana-hardware.yang
|		├── ietf-crypto-types.yang
|		├── ietf-hardware.yang
|		├── ietf-inet-types.yang
|		├── ietf-netconf-acm.yang
|		├── ietf-netconf-monitoring-extension.yang
|		├── ietf-netconf-monitoring.yang
|		├── ietf-yang-types.yang
|		├── o-ran-file-management.yang
|		├── o-ran-hardware.yang
|		├── o-ran-heartbeat-management.yang
|		├── o-ran-software-management.yang
|		└── tr069-operations.yang

+++++++++
ves-agent
+++++++++
Any notification from the device sent through TR-069Adapter and Mapper, is forwarded to VES by this component. Whenever fault inform is reported by tr-69 device, acs module will forward this to ves-agent module, it will convert tr09 faults fields to VES fault fields and sent to VES-Collector via ngnix.

+++++
nginx
+++++
This module is to offload the SSL communications between device and VES listener.

++++++++++++
How to build
++++++++++++
To build, run below commands

$mvn clean install -Dmaven.test.skip=false

It will generate the docker images in the respective module target directories. To deploy the adapter refer deployment guide.