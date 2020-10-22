.. This work is licensed under a Creative Commons Attribution 4.0 International License.
.. SPDX-License-Identifier: CC-BY-4.0
.. Copyright (C) 2020 CommScope Inc. 

TR-069 Adapter Deployment and Configuration
===========================================

This section lists the required steps to deploy and install the TR-069Adapter in Kubernetes environment.

Note: Before deploying and installing the TR-069Adapter, ensure the required third-party software such as Kubernetes, Docker, and Helm softwares are installed.

1. Push all TR-069Adapter Docker image files to the central repository.

2. Edit the values.yaml file to update the following parameters.

+------------------+----------------------+------------------------------------------------------------+
|**Parameter**     |**Default Value**     |**Description**                                             |
|                  |                      |                                                            |
+==================+======================+============================================================+
|mountPath	   |/dockerdata-nfs	  |Persisted Volume Mount point on the worker                  |
|                  |                      | node.                                                      |
+------------------+----------------------+------------------------------------------------------------+
|macID	           |0005B9AAAAA3          |Mac ID of the Device                                        |
|                  |                      |                                                            |
+------------------+----------------------+------------------------------------------------------------+
|dbRootPassword    |root	          |MariaDB root user password that needs to be                 |
|                  |                      |configured                                                  |
+------------------+----------------------+------------------------------------------------------------+
|preConfigOnPNP    |false                 |Set it to True if TR-069Adapter has to provision initial    |
|                  |                      |set of parameters as part of BootStrap.If this parameter    |
|                  |                      |is set to true, then TR-069Adapter shall provision the      |
|                  |                      |configurations as part of BootStrap and sets Admin          |
|                  |                      |Status to true. In case of boot, TR-069Adapter shall set    |
|                  |                      |Admin Status to true.Allowed values: true/false             |
|                  |                      |                                                            |
+------------------+----------------------+------------------------------------------------------------+
|repository	   |repo.com:5100         |Domain name and port to access the central repository where |
|                  |                      |TR-069Adapter docker images are uploaded.                   |
+------------------+----------------------+------------------------------------------------------------+
|pullSecret	   |dockerhub.com         |Secret key or password to login to central repository.      |
|                  |                      |                                                            |
+------------------+----------------------+------------------------------------------------------------+
|onapFaultVes      |10.211.5.27           |VES collector IP Address for sending device fault events.   |
|CollectorIP       |                      |                                                            |
+------------------+----------------------+------------------------------------------------------------+
|onapFaultVesCo    |30235                 |VES collector IP Port for sending device fault events.      |
|llectorPort       |                      |must be configured.                                         |
|                  |                      |                                                            |
+------------------+----------------------+------------------------------------------------------------+
|onapPnfRegVes     |10.211.5.27           |VES collector IP Address for sending pnfRegistration events.|
|CollectorIP       |                      |                                                            |
+------------------+----------------------+------------------------------------------------------------+
|onapPnfRegVes     |30235                 |VES collector IP Address for sending pnfRegistration events.| 
|CollectorPort     |                      |                                                            |
+------------------+----------------------+------------------------------------------------------------+
|type              |                      |Cluster IP or node port details to register                 |
|                  |                      |                                                            |
+------------------+----------------------+------------------------------------------------------------+
|portName          |srv-netconf           |Service name required to open the ports                     |
|                  |                      |                                                            |
+------------------+----------------------+------------------------------------------------------------+
|internalPort      |17830                 |Starting internal port number                               |
|                  |                      |                                                            |
+------------------+----------------------+------------------------------------------------------------+
|externalPort      |105                   |Last 3 digits of the starting external port                 |
|                  |                      |                                                            |
+------------------+----------------------+------------------------------------------------------------+
|portRange         |10                    |Number of ports to open. Each TR-069 Device that the adapter|
|                  |                      |manages it needs dedicated port for NetConf Server. In this |
|                  |                      |release maximum 200 devices could be connected to a single  |
|                  |                      |Adapter hence Maximum 200 ports are supported.              |
|                  |                      |                                                            |
+------------------+----------------------+------------------------------------------------------------+


3. Execute the following command to install the TR-069Adapter

   ``# helm install tr069adapter --name=<Deployment_Name>``

   For example: # helm install tr069adapter --name=AdapterDevices1To200 

4. Execute the following command to check if the TR-069Adapter is successfully installed:

   ``# helm history <Deployment_Name>``

   For example: # helm history AdapterDevices1To200

5. Execute the following command to check if all the clusters are running

   ``# kubectl get po``

