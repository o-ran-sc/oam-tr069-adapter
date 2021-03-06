/*
 * ============LICENSE_START========================================================================
 * ONAP : tr-069-adapter
 * =================================================================================================
 * Copyright (C) 2020 CommScope Inc Intellectual Property.
 * =================================================================================================
 * This tr-069-adapter software file is distributed by CommScope Inc under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except in compliance with the License. You
 * may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * This file is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 * either express or implied. See the License for the specific language governing permissions and
 * limitations under the License.
 * ===============LICENSE_END=======================================================================
 */

package org.commscope.tr069adapter.mapper.netconf;

import org.commscope.tr069adapter.mapper.model.NetConfRequest;
import org.commscope.tr069adapter.mapper.model.NetConfResponse;
import org.commscope.tr069adapter.mapper.model.NetConfServerDetails;

public interface NetConfRequestHandler {

  public NetConfResponse handleSetConfigRequest(NetConfRequest request);

  public NetConfResponse handleGetRequest(NetConfRequest request);

  public NetConfResponse handleGetConfigRequest(NetConfRequest request);

  public NetConfResponse handleDelConfigRequest(NetConfRequest request);

  public boolean handelRegisterEvent(NetConfServerDetails request);

  public NetConfResponse handleSWDownloadRequest(NetConfRequest request);

  public NetConfResponse handleAddObjectRequest(NetConfRequest request);

  public NetConfResponse handleDeleteObjectRequest(NetConfRequest request);

  public NetConfResponse handleRequestWithoutInputParams(NetConfRequest request);

  public NetConfResponse handleSPAObjectRequest(NetConfRequest request);

  public NetConfResponse handleConnectionStatusRequest(NetConfRequest request);

  public NetConfResponse handleGPAObjectRequest(NetConfRequest request);

  public NetConfResponse handleDownloadRequest(NetConfRequest request);

}
