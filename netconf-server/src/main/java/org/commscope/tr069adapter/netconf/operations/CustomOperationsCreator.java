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

package org.commscope.tr069adapter.netconf.operations;

import com.google.common.collect.Sets;

import java.util.Optional;
import java.util.Set;

import org.commscope.tr069adapter.netconf.rpc.AddObjectOperation;
import org.commscope.tr069adapter.netconf.rpc.ConnectionStatus;
import org.commscope.tr069adapter.netconf.rpc.CreateSubscription;
import org.commscope.tr069adapter.netconf.rpc.DeleteConfigOperation;
import org.commscope.tr069adapter.netconf.rpc.DeleteObjectOperation;
import org.commscope.tr069adapter.netconf.rpc.DownloadOperation;
import org.commscope.tr069adapter.netconf.rpc.GPAObjectOperation;
import org.commscope.tr069adapter.netconf.rpc.GetConfigOperation;
import org.commscope.tr069adapter.netconf.rpc.GetOperation;
import org.commscope.tr069adapter.netconf.rpc.OperationCommit;
import org.commscope.tr069adapter.netconf.rpc.OperationLock;
import org.commscope.tr069adapter.netconf.rpc.OperationUnLock;
import org.commscope.tr069adapter.netconf.rpc.RebootOperation;
import org.commscope.tr069adapter.netconf.rpc.ResetOperation;
import org.commscope.tr069adapter.netconf.rpc.SPAObjectOperation;
import org.commscope.tr069adapter.netconf.rpc.SetConfigOperation;
import org.commscope.tr069adapter.netconf.rpc.SoftwareActivateOperation;
import org.commscope.tr069adapter.netconf.rpc.SoftwareDownloadOperation;
import org.opendaylight.netconf.api.capability.Capability;
import org.opendaylight.netconf.impl.SessionIdProvider;
import org.opendaylight.netconf.mapping.api.NetconfOperation;
import org.opendaylight.netconf.mapping.api.NetconfOperationService;
import org.opendaylight.netconf.test.tool.operations.OperationsCreator;
import org.opendaylight.netconf.test.tool.rpc.DataList;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public final class CustomOperationsCreator implements OperationsCreator {
  private static final Logger logger = LoggerFactory.getLogger(CustomOperationsCreator.class);

  private String macID = null;

  private String swVersion;

  private String hwVersion;

  private OperationService operationService;

  public CustomOperationsCreator(String macID, String swVersion, String hwVersion) {
    this.macID = macID;
    this.swVersion = swVersion;
    this.hwVersion = hwVersion;
  }

  @Override
  public NetconfOperationService getNetconfOperationService(final Set<Capability> caps,
      final SessionIdProvider idProvider, final String netconfSessionIdForReporting) {
    if (null == operationService) {
      operationService =
          new OperationService(idProvider.getCurrentSessionId(), macID, swVersion, hwVersion);
    }
    return operationService;
  }

  static class OperationService implements NetconfOperationService {

    private final long currentSessionId;
    private String macID;
    private String swVersion;
    private String hwVersion;

    OperationService(final long currentSessionId, String macID, String swVersion,
        String hwVersion) {
      this.currentSessionId = currentSessionId;
      this.macID = macID;
      this.swVersion = swVersion;
      this.hwVersion = hwVersion;
    }

    @Override
    public Set<NetconfOperation> getNetconfOperations() {
      final DataList storage = new DataList();
      final GetOperation oGet =
          new GetOperation(String.valueOf(currentSessionId), storage, macID, swVersion, hwVersion);
      final GetConfigOperation oGetConfig = new GetConfigOperation(String.valueOf(currentSessionId),
          Optional.empty(), macID, swVersion, hwVersion);
      final SetConfigOperation oSetConfig =
          new SetConfigOperation(String.valueOf(currentSessionId), macID, swVersion, hwVersion);
      final DeleteConfigOperation oDelConfig = new DeleteConfigOperation(
          String.valueOf(currentSessionId), storage, macID, swVersion, hwVersion);
      final OperationCommit oCommit = new OperationCommit(String.valueOf(currentSessionId));
      final OperationLock oLock = new OperationLock(String.valueOf(currentSessionId));
      final OperationUnLock oUnlock = new OperationUnLock(String.valueOf(currentSessionId));
      final CreateSubscription sCreateSubs = new CreateSubscription(
          String.valueOf(currentSessionId), Optional.empty(), macID, swVersion, hwVersion);
      SoftwareDownloadOperation swDownloadOperation =
          new SoftwareDownloadOperation(macID, swVersion, hwVersion);
      SoftwareActivateOperation swActivateOperation =
          new SoftwareActivateOperation(macID, swVersion, hwVersion);
      AddObjectOperation addObjectOperation = new AddObjectOperation(macID, swVersion, hwVersion);
      DeleteObjectOperation deleteObjectOperation =
          new DeleteObjectOperation(macID, swVersion, hwVersion);
      GPAObjectOperation gpaObjectOperation = new GPAObjectOperation(macID, swVersion, hwVersion);
      SPAObjectOperation spaObjectOperation = new SPAObjectOperation(macID, swVersion, hwVersion);
      ConnectionStatus connStatus = new ConnectionStatus(macID, swVersion, hwVersion);
      RebootOperation rebootOperation = new RebootOperation(macID, swVersion, hwVersion);
      ResetOperation resetOperation = new ResetOperation(macID, swVersion, hwVersion);
      DownloadOperation downloadOp = new DownloadOperation(macID, swVersion, hwVersion);
      return Sets.newHashSet(oGet, oGetConfig, oSetConfig, oDelConfig, oCommit, oLock, oUnlock,
          sCreateSubs, swDownloadOperation, swActivateOperation, addObjectOperation,
          deleteObjectOperation, gpaObjectOperation, spaObjectOperation, connStatus,
          rebootOperation, resetOperation, downloadOp);
    }

    @Override
    public void close() {
      logger.debug("close called on CustomOperationsCreator");
    }

  }
}
