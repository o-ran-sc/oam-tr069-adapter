.. This work is licensed under a Creative Commons Attribution 4.0 International License.
.. SPDX-License-Identifier: CC-BY-4.0
.. Copyright (C) 2020 CommScope Inc.

TR-069 Adapter User Guide
=========================

This document provides a quick start for using TR-069 Adapter.


TR-069 Device Connecting to TR-069 Adapter
------------------------------------------

Configure URL **http://<external_address_tr-069Adapter>/CPEMgmt/acs** to Device as Management Server URL. 

Importing a Initial PnP Configuration file
------------------------------------------
Initial PnP Configuration file supports the ability to configure the PnP parameters that TR-069Adapter must provision to the Device as part of a BootStrap Inform. The configuration XML (3GPP TS 32.594) file is used to define the initial PnP Configurations for Device. Before importing a configuration file to a server you must edit the file to enter the required configuration details.

To import a configuration file:

1.	Access the sample configuration file (samplexmls/macid.xml) from the distribution package.

2.	Edit the macid string in macid.xml file with the device’s macid value.

3.	Edit the file to add the device configuration details, in the same format as mentioned in the sample file.

4.	To import initial PnP configuration file using curl use following command 

    ``$curl -F 'files=@<completeFilePath>' http://<external_address>/importConfig``

    For example:
    ``$curl -F 'files=@/home/tr069admin/0005b9423910.xml' http://tr069.amazonaws.com/importConfig``

5.	You can check the imported configuration file details by entering the config service IP, external port, getConfig command, and MACID as shown in the below format in your browser:

    ``http://<config_service_IP>:<external_port>/<getConfig>/<MAC_ID>``  

    For example, ``http://tr069.amazonaws.com/getConfig/0005B9423910`` 


Mounting a Device in SDN-R
--------------------------

Device will get auto mounted in SDN-R once the BootStrap message is received from device and forwarded to VES Collector as VES PnfRegistration message by TR-069 Adapter. 
