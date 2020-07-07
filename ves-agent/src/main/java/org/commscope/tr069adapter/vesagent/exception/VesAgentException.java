package org.commscope.tr069adapter.vesagent.exception;

/**
 * 
 * @version 1.0
 * @since May 21, 2020
 * @author Prashant
 */
public class VesAgentException extends Exception {
  private static final long serialVersionUID = -3742697051389101875L;

  private static final String ERRORMSG_PREFIX = "ves-agent";

  private String[] arguments;

  private String errorCode;
  private String message;

  // index of the error occurred in the given list or in the given file
  protected int errorIndex = -1;


  /**
   * Constructs a <code>VesOperationException</code> with no detail message.
   * 
   */
  public VesAgentException() {
    super();
  }

  /**
   * Constructs a <code>VesOperationException</code> with the specified detail message.
   * 
   * @param s as the details message
   */
  public VesAgentException(String s) {
    super(s);
  }

  public VesAgentException(String... args) {
    super();
    arguments = args;
    setErrorMessage(getErrorMessage());
  }

  public VesAgentException(String errorCode, String errorMsg) {
    super();
    this.errorCode = errorCode;
    setErrorMessage(getErrorMessage());
  }

  public int getErrorIndex() {
    return errorIndex;
  }

  public void setErrorIndex(int errorIndex) {
    this.errorIndex = errorIndex;
  }


  public String getMessage() {
    return message;
  }

  private String getErrorMessage() {
    String key = ERRORMSG_PREFIX + ".";
    return key;
  }

  private void setErrorMessage(String message) {
    this.message = message;
  }

  public String getErrorCode() {
    return errorCode;
  }

  public void setErrorCode(String errorCode) {
    this.errorCode = errorCode;
  }
}
