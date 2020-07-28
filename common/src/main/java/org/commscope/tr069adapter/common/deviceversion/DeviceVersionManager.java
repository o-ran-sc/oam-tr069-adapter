package org.commscope.tr069adapter.common.deviceversion;

import java.util.List;

public interface DeviceVersionManager {

  public String getNetconfYangSchemaPath(String swVersion, String hwVersion);

  public String getBaseNetconfYangSchemaPath();

  public String getAssociatedProfileId(String swVersion, String hwVersion);
  
  public ProfileDefinition getProfileDefinition(String swVersion, String hwVersion);
  
  public List<ProfileDefinition> getSupportedProfileDefinitions();
  
}
