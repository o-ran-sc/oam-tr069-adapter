.. This work is licensed under a Creative Commons Attribution 4.0 International License.
.. SPDX-License-Identifier: CC-BY-4.0
.. Copyright (C) 2020 CommScope Inc 

TR-069 Adapter Deployment and Configuration
===========================================

This section lists the required steps to deploy and install the TR-069Adapter in Kubernetes environment.

Note: Before deploying and installing the TR-069Adapter, ensure the required third-party software such as Kubernetes, Docker, and Helm software is installed.

1. Pushing Docker Images to the Central Repository
   - Push all the Docker image files (dockerimages/*.tgz files) to the central repository.

2.  Edit the values.yaml file to update the following parameters:
+--------------+------------------+--------------------------------------------------------+
|Parameter     |Default Value     |Description                                             |
+==============+==================+========================================================+
|mountPath	   |/dockerdata-nfs	  |Mount point on the worker node.                         |
+--------------+------------------+--------------------------------------------------------+
|macID	       |0005B9AAAAA3      |Mac ID of the OneCell Device                            |
+--------------+------------------+--------------------------------------------------------+
|dbRootPassword|root	          |MariaDB root user password                              |
+--------------+------------------+--------------------------------------------------------+
|preConfigOnPNP|false             |Set it to True if TR-069Adapter has to provision initial|
|              |                  |set of parameters as part of BootStrap.If this parameter|
|              |                  |is set to true, then TR-069Adapter shall provision the  |
|              |                  |configurations as part of BootStrap and set to Admin    |
|              |                  |Status to true. In case of boot, TR-069Adapter shall set|
|              |                  |Admin Status to true.Allowed values: true/false         |
+--------------+------------------+--------------------------------------------------------+
|repository	   |repo.com:5100     |Domain name and port to access the central repository.  |
+--------------+----------------------+----------------------------------------------------+
|pullSecret	   |nexus-carson-city |Secret key or password to login to central repository.  |
+--------------+------------------+--------------------------------------------------------+
|onapFaultVes  |10.211.5.27       |VES collector for Fault listener IP address must        |
|CollectorIP   |                  |be configured.                                          |
+--------------+------------------+--------------------------------------------------------+
|onapFaultVesCo|30235             |VES collector for Fault listerner port number           |
|llectorPort   |                  |must be configured.                                     |
+--------------+------------------+--------------------------------------------------------+
|onapPnfRegVes |10.211.5.27       |VES collector for pnfReg listener IP address            |
|CollectorIP   |                  |must be configured.                                     |
+--------------+------------------+--------------------------------------------------------+
|onapPnfRegVes |30235             |VES collector for pnfReg listerner port number          | 
|CollectorPort |                  |must be configured.                                     |
+--------------+------------------+--------------------------------------------------------+

3. Execute the following command to install the TR-069Adapter
``# helm install tr069adapter --name=<MACID in lowercase>``
For example: # helm install tr069adapter --name=0005b9423910 

4. Execute the following command to check if the TR-069Adapter is successfully installed:

``# helm history <MACID in lowercase>``
5. Execute the following command to check if all the clusters are running
``# kubectl get po``