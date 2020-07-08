#!/bin/bash
# ============LICENSE_START========================================================================
# O-RAN-SC : tr-069-adapter
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

NGINX_CONF=/etc/nginx/nginx.conf

updateAdapter()
{
echo ""
echo "# Checking if TR069Adapter is SSL Enabled..."
if [ $tr069adapterComm != "SSL" ]; then
  echo ": SSL Not enabled, Cleaning up the entries..."
  perl -pi -e "s/tr069adapterComm//g" $NGINX_CONF
    sed -i '/tr069adapterSSLCert;/,+4 d' $NGINX_CONF
else
  echo ": SSL is enabled, Updating the entries..."
  perl -pi -e "s/tr069adapterComm/ssl/g" $NGINX_CONF
  perl -pi -e "s/tr069adapterSSLCert/${tr069adapterSSLCert}/g" $NGINX_CONF
  perl -pi -e "s/tr069adapterSSLKey/${tr069adapterSSLKey}/g" $NGINX_CONF
  if [ $tr069adapterSSLClientAuth != "TRUE" ]; then
   echo ": Client Trusted Certificate verification is NOT Enabled..."
    sed -i '/tr069adapterSSLClientTrustCRT/,+2 d' $NGINX_CONF
  else
    echo ": Client Trusted Certificate verification is Enabled..."
    perl -pi -e "s/tr069adapterSSLClientTrustCRT/${tr069adapterSSLClientTrustCRT}/g" $NGINX_CONF
  fi
fi

  if [ -f /etc/nginx/ssl/${tr069adapterCertPassPhrase} ]; then
    echo ": Updating the Certificate PassPhrase..."
    perl -pi -e "s/tr069adapterCertPassPhrase/${tr069adapterCertPassPhrase}/g" $NGINX_CONF
  else
    echo ": Certificate PassPhrase NOT Available..."
    perl -pi -e "s/^\s*ssl_password_file.*//g" $NGINX_CONF
    perl -pi -e "s/^\s*proxy_ssl_password_file.*//g" $NGINX_CONF
  fi

echo ": Updating the IP Address for $netconfServerIP..."
if [[ ${netconfServerIP} =~ .*:.* ]]; then
  perl -pi -e "s/netconfServerIP/[${netconfServerIP}]/g" $NGINX_CONF
else
  perl -pi -e "s/netconfServerIP/${netconfServerIP}/g" $NGINX_CONF
fi
}

updateFaultVes()
{
echo ""
echo "# Checking configuration for Fault VES Collector..."
echo ": Updating the IP Address and Port for $faultvesCollectorHost..."
perl -pi -e "s/faultvesCollectorPort/${faultvesCollectorPort}/g" $NGINX_CONF
if [[ ${faultvesCollectorHost} =~ .*:.* ]]; then
  if [ $faultvesCollectorComm = "noAuth" ]; then
    perl -pi -e "s|https://faultvesCollectorHost|http://[${faultvesCollectorHost}]|g" $NGINX_CONF
  else
    perl -pi -e "s/faultvesCollectorHost/[${faultvesCollectorHost}]/g" $NGINX_CONF
  fi
else
  if [ $faultvesCollectorComm = "noAuth" ]; then
    perl -pi -e "s|https://faultvesCollectorHost|http://${faultvesCollectorHost}|g" $NGINX_CONF
  else
   perl -pi -e "s/faultvesCollectorHost/${faultvesCollectorHost}/g" $NGINX_CONF
  fi
fi

if [ $faultvesCollectorComm = "noAuth" ]; then
  echo ": SSL Not enabled, Cleaning up the entries..."
  sed -i '/faultvesCollectorSSLCert/,+9 d' $NGINX_CONF

elif [ $faultvesCollectorComm = "basicAuth" ]; then
  echo ": SSL is enabled as basicAuth, Updating the entries..."
  sed -i '/faultvesCollectorSSLCert/,+2 d' $NGINX_CONF
  if [ ! -z ${faultvesCollectorBasicAuthUserPassEncrypt} ]; then
    perl -pi -e "s/faultvesCollectorBasicAuthUserPassEncrypt/${faultvesCollectorBasicAuthUserPassEncrypt}/g" $NGINX_CONF
  else
    echo ": Username/Password Credentials not provided...FAILED"
    exit 1
  fi

elif [ $faultvesCollectorComm = "certOnly" ]; then
  echo ": SSL is enabled as certOnly, Updating the entries..."
  if [ -f /etc/nginx/ssl/${faultvesCollectorSSLCert} ]; then
    perl -pi -e "s/faultvesCollectorSSLCert/${faultvesCollectorSSLCert}/g" $NGINX_CONF
  else
    echo ": SSL Certificate ${faultvesCollectorSSLCert} not available...FAILED"
    exit 1
  fi

  if [ -f /etc/nginx/ssl/${faultvesCollectorSSLKey} ]; then
    perl -pi -e "s/faultvesCollectorSSLKey/${faultvesCollectorSSLKey}/g" $NGINX_CONF
  else
    echo ": SSL Key ${faultvesCollectorSSLKey} not available...FAILED"
    exit 1
  fi

  sed -i 's|proxy_set_header Authorization "Basic faultvesCollectorBasicAuthUserPassEncrypt";||g' $NGINX_CONF

elif [ $faultvesCollectorComm = "certBasicAuth" ]; then
  echo ": SSL is enabled as certBasicAuth, Updating the entries..."
  if [ ! -z ${faultvesCollectorBasicAuthUserPassEncrypt} ]; then
    perl -pi -e "s/faultvesCollectorBasicAuthUserPassEncrypt/${faultvesCollectorBasicAuthUserPassEncrypt}/g" $NGINX_CONF
  else
    echo ": Username/Password Credentials not provided...FAILED"
    exit 1
  fi

  if [ -f /etc/nginx/ssl/${faultvesCollectorSSLCert} ]; then
    perl -pi -e "s/faultvesCollectorSSLCert/${faultvesCollectorSSLCert}/g" $NGINX_CONF
  else
    echo ": SSL Certificate ${faultvesCollectorSSLCert} not available...FAILED"
    exit 1
  fi

  if [ -f /etc/nginx/ssl/${faultvesCollectorSSLKey} ]; then
    perl -pi -e "s/faultvesCollectorSSLKey/${faultvesCollectorSSLKey}/g" $NGINX_CONF
  else
    echo ": SSL Key ${faultvesCollectorSSLKey} not available...FAILED"
    exit 1
  fi

else
  echo ": Invalid Option... FAILED"
  exit 1
fi

echo ""
echo "# Checking if Trusted CA Certificate verification is Enabled or Not..."
if [ $faultvesCollectorTrustVerify != "TRUE" ]; then
  echo ": Trusted CA Certificate verification is NOT Enabled..."
  sed -i '/faultvesCollectorTrustCRT/,+2 d' $NGINX_CONF
else
  echo ": Trusted CA Certificate verification is Enabled..."
  perl -pi -e "s/faultvesCollectorTrustCRT/${faultvesCollectorTrustCRT}/g" $NGINX_CONF
fi

echo ""
echo "# Checking if Fault VES Collector DNS Server Details are Provided..."
if [ ! -z ${faultvesCollectorDNSServer} ]; then
  echo ": DNS Server Details Provided..."
  perl -pi -e "s/faultvesCollectorDNSServer/${faultvesCollectorDNSServer}/g" $NGINX_CONF
else
  echo ": DNS Server Details NOT Provided..."
  sed -i 's|resolver faultvesCollectorDNSServer;||g' $NGINX_CONF
fi
}

updatePNFVes()
{
echo ""
echo "# Checking configuration for Pnf Req VES Collector..."
echo ": Updating the IP Address and Port for $pnfregvesCollectorHost..."
perl -pi -e "s/pnfregvesCollectorPort/${pnfregvesCollectorPort}/g" $NGINX_CONF
if [[ ${pnfregvesCollectorHost} =~ .*:.* ]]; then
  if [ $pnfregvesCollectorComm = "noAuth" ]; then
    perl -pi -e "s|https://pnfregvesCollectorHost|http://[${pnfregvesCollectorHost}]|g" $NGINX_CONF
  else
    perl -pi -e "s/pnfregvesCollectorHost/[${pnfregvesCollectorHost}]/g" $NGINX_CONF
  fi
else
  if [ $pnfregvesCollectorComm = "noAuth" ]; then
    perl -pi -e "s|https://pnfregvesCollectorHost|http://${pnfregvesCollectorHost}|g" $NGINX_CONF
  else
   perl -pi -e "s/pnfregvesCollectorHost/${pnfregvesCollectorHost}/g" $NGINX_CONF
  fi
fi

if [ $pnfregvesCollectorComm = "noAuth" ]; then
  echo ": SSL Not enabled, Cleaning up the entries..."
  sed -i '/pnfregvesCollectorSSLCert/,+9 d' $NGINX_CONF

elif [ $pnfregvesCollectorComm = "basicAuth" ]; then
  echo ": SSL is enabled as basicAuth, Updating the entries..."
  sed -i '/pnfregvesCollectorSSLCert/,+2 d' $NGINX_CONF
  if [ ! -z ${pnfregvesCollectorBasicAuthUserPassEncrypt} ]; then
    perl -pi -e "s/pnfregvesCollectorBasicAuthUserPassEncrypt/${pnfregvesCollectorBasicAuthUserPassEncrypt}/g" $NGINX_CONF
  else
    echo ": Username/Password Credentials not provided...FAILED"
    exit 1
  fi

elif [ $pnfregvesCollectorComm = "certOnly" ]; then
  echo ": SSL is enabled as certOnly, Updating the entries..."
  if [ -f /etc/nginx/ssl/${pnfregvesCollectorSSLCert} ]; then
    perl -pi -e "s/pnfregvesCollectorSSLCert/${pnfregvesCollectorSSLCert}/g" $NGINX_CONF
  else
    echo ": SSL Certificate ${pnfregvesCollectorSSLCert} not available...FAILED"
    exit 1
  fi

  if [ -f /etc/nginx/ssl/${pnfregvesCollectorSSLKey} ]; then
    perl -pi -e "s/pnfregvesCollectorSSLKey/${pnfregvesCollectorSSLKey}/g" $NGINX_CONF
  else
    echo ": SSL Key ${pnfregvesCollectorSSLKey} not available...FAILED"
    exit 1
  fi

  sed -i 's|proxy_set_header Authorization "Basic pnfregvesCollectorBasicAuthUserPassEncrypt";||g' $NGINX_CONF

elif [ $pnfregvesCollectorComm = "certBasicAuth" ]; then
  echo ": SSL is enabled as certBasicAuth, Updating the entries..."
  if [ ! -z ${pnfregvesCollectorBasicAuthUserPassEncrypt} ]; then
    perl -pi -e "s/pnfregvesCollectorBasicAuthUserPassEncrypt/${pnfregvesCollectorBasicAuthUserPassEncrypt}/g" $NGINX_CONF
  else
    echo ": Username/Password Credentials not provided...FAILED"
    exit 1
  fi

    if [ -f /etc/nginx/ssl/${pnfregvesCollectorSSLCert} ]; then
    perl -pi -e "s/pnfregvesCollectorSSLCert/${pnfregvesCollectorSSLCert}/g" $NGINX_CONF
  else
    echo ": SSL Certificate ${pnfregvesCollectorSSLCert} not available...FAILED"
    exit 1
  fi

  if [ -f /etc/nginx/ssl/${pnfregvesCollectorSSLKey} ]; then
    perl -pi -e "s/pnfregvesCollectorSSLKey/${pnfregvesCollectorSSLKey}/g" $NGINX_CONF
  else
    echo ": SSL Key ${pnfregvesCollectorSSLKey} not available...FAILED"
    exit 1
  fi

else
  echo ": Invalid Option... FAILED"
  exit 1
fi

echo ""
echo "# Checking if Trusted CA Certificate verification is Enabled or Not..."
if [ $pnfregvesCollectorTrustVerify != "TRUE" ]; then
  echo ": Trusted CA Certificate verification is NOT Enabled..."
  sed -i '/pnfregvesCollectorTrustCRT/,+2 d' $NGINX_CONF
else
  echo ": Trusted CA Certificate verification is Enabled..."
  perl -pi -e "s/pnfregvesCollectorTrustCRT/${pnfregvesCollectorTrustCRT}/g" $NGINX_CONF
fi

echo""
echo "# Checking if PNF VES Collector DNS Server Details are Provided..."
if [ ! -z ${pnfregvesCollectorDNSServer} ]; then
  echo ": DNS Server Details Provided..."
  perl -pi -e "s/pnfregvesCollectorDNSServer/${pnfregvesCollectorDNSServer}/g" $NGINX_CONF
else
  echo ": DNS Server Details NOT Provided..."
  sed -i 's|resolver pnfregvesCollectorDNSServer;||g' $NGINX_CONF
fi
}

#
# Main Call
#

updateAdapter
updateFaultVes
updatePNFVes

exec nginx -g 'daemon off;'

