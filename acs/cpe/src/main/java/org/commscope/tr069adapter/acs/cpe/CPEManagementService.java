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

package org.commscope.tr069adapter.acs.cpe;

import static org.commscope.tr069adapter.acs.common.utils.AcsConstants.ACS_SESSIONID;
import static org.commscope.tr069adapter.acs.common.utils.AcsConstants.CWMP_VERSION;
import static org.commscope.tr069adapter.acs.common.utils.AcsConstants.INFORM;
import static org.commscope.tr069adapter.acs.common.utils.AcsConstants.TRANSFER_COMPLETE;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.io.Reader;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.util.Enumeration;

import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.core.Context;
import javax.xml.soap.MessageFactory;
import javax.xml.soap.MimeHeaders;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPMessage;

import org.commscope.tr069adapter.acs.common.exception.TR069EventProcessingException;
import org.commscope.tr069adapter.acs.common.response.DeviceInformResponse;
import org.commscope.tr069adapter.acs.common.utils.ErrorCode;
import org.commscope.tr069adapter.acs.cpe.handler.DeviceEventHandler;
import org.commscope.tr069adapter.acs.cpe.rpc.Fault;
import org.commscope.tr069adapter.acs.cpe.rpc.Inform;
import org.commscope.tr069adapter.acs.cpe.rpc.InformResponse;
import org.commscope.tr069adapter.acs.cpe.rpc.TransferComplete;
import org.commscope.tr069adapter.acs.cpe.rpc.TransferCompleteResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/CPEMgmt")
public class CPEManagementService {

  private static final String UTF_8 = "UTF-8";

  private static final Logger logger = LoggerFactory.getLogger(CPEManagementService.class);

  private static final int MY_MAX_ENVELOPES = 1;

  @Autowired
  DeviceEventHandler deviceEventHandler;

  /**
   * @param request
   * @param response
   */
  @SuppressWarnings("static-access")
  @PostMapping("/acs")
  public void processDeviceEvent(@Context HttpServletRequest request,
      @Context HttpServletResponse response) {

    logger.debug("A device event occurred");
    logHeaderElements(request.getHeaderNames());

    try {
      Boolean isEmptyCPERequest = true;
      SOAPMessage soapMsg = null;
      ByteArrayOutputStream out = new ByteArrayOutputStream();

      String ct = request.getContentType();
      int csix = -1;
      String csFrom = "ISO-8859-1";
      if (ct != null) {
        csix = ct.indexOf("charset=");
        response.setContentType(ct);
      } else {
        response.setContentType("text/xml;charset=UTF-8");
      }

      if (csix != -1)
        csFrom = ct.substring(csix + 8).replaceAll("\"", "");

      Cookie[] cookies = request.getCookies();
      String acsSessionID = getACSSessionCookieData(cookies);
      String cwmpVersion = getCWMPVersionCookieData(cookies);

      XmlFilterInputStream f =
          new XmlFilterInputStream(request.getInputStream(), request.getContentLength());
      MessageFactory mf = getSOAPMessageFactory();
      while (f.next()) {
        isEmptyCPERequest = false;
        MimeHeaders hdrs = new MimeHeaders();
        hdrs.setHeader("Content-Type", "text/xml; charset=UTF-8");
        InputStream in = getInputStream(csFrom, f);
        soapMsg = mf.createMessage(hdrs, in);

        logSoapMsg(soapMsg);

        TR069RPC msg = null;
        msg = TR069RPC.parse(soapMsg);

        String reqType = getRequestType(msg);
        logger.info("Event notified by the device is of type: {}", reqType);

        if (reqType != null) {
          if (reqType.equals(INFORM)) {
            processDeviceInform(msg, request, response, out);
          } else if (reqType.equals(TRANSFER_COMPLETE)) {
            processTransferComplete(msg, request, response, out);
          } else {
            processOperationResult(msg, response, reqType, acsSessionID, out);
          }
        }
      }

      if (isEmptyCPERequest.booleanValue()) {
        processEmptyCPERequest(response, cwmpVersion, acsSessionID, out);
      }

      if (out.size() < 1) {// To delete dm_sessionId cookie
        clearCookies(cookies, response);
      }

      response.setContentLength(out.size());
      response.setHeader("SOAPAction", "");
      String sout = out.toString().trim();
      logger.info(sout);
      response.getOutputStream().print(sout);
      response.getOutputStream().flush();
      logger.debug("End of processing");

    } catch (Exception e) {
      logger.error("An error occurred while processing device event, Exception: {}",
          e.getMessage());
      response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
    }

    logger.debug("End of processing the HTTP post request");
  }

  private InputStream getInputStream(String csFrom, XmlFilterInputStream f) throws IOException {
    InputStream in = null;
    if (csFrom.equalsIgnoreCase(UTF_8)) {
      in = new XmlFilterNS(f);
    } else {
      in = new CharsetConverterInputStream(csFrom, UTF_8, new XmlFilterNS(f));
    }
    return in;
  }

  private void processDeviceInform(TR069RPC msg, HttpServletRequest request,
      HttpServletResponse response, ByteArrayOutputStream out) throws IOException {
    Inform inform = (Inform) msg;
    DeviceInformResponse deviceInformResponse = null;
    try {
      deviceInformResponse =
          deviceEventHandler.processDeviceInform(inform, request.getHeader("Authorization"));
      Cookie cookie = new Cookie(ACS_SESSIONID, deviceInformResponse.getSessionId());
      cookie.setSecure(request.isSecure());
      cookie.setHttpOnly(true);
      Cookie cwmpVerCookie = new Cookie(CWMP_VERSION, msg.getCWMPVersion());
      cwmpVerCookie.setSecure(request.isSecure());
      cwmpVerCookie.setHttpOnly(true);
      response.addCookie(cookie);
      response.addCookie(cwmpVerCookie);
    } catch (TR069EventProcessingException tr069ex) {
      ErrorCode errorCode = tr069ex.getErrorCode();
      if (ErrorCode.OUI_OR_PC_MISMATCH.equals(errorCode)) {
        sendFault(response, out, Fault.FCODE_ACS_REQUEST_DENIED, "OUIandProductClassNotValid",
            inform.getId());
      } else {
        int httpStatusCode = deviceEventHandler.handleException(tr069ex);
        response.setStatus(httpStatusCode);
      }
      int httpStatusCode = deviceEventHandler.handleException(tr069ex);
      response.setStatus(httpStatusCode);
    }

    InformResponse resp = new InformResponse(inform.getId(), MY_MAX_ENVELOPES);
    resp.setCWMPVersion(msg.getCWMPVersion());
    resp.writeTo(out);
  }

  private void processTransferComplete(TR069RPC msg, HttpServletRequest request,
      HttpServletResponse response, ByteArrayOutputStream out) {
    TransferComplete tc = (TransferComplete) msg;
    try {
      DeviceInformResponse deviceInformResponse = deviceEventHandler.processTransferComplete(tc);
      Cookie cookie = new Cookie(ACS_SESSIONID, deviceInformResponse.getSessionId());
      cookie.setSecure(request.isSecure());
      cookie.setHttpOnly(true);
      Cookie cwmpVerCookie = new Cookie(CWMP_VERSION, msg.getCWMPVersion() + ";");
      cwmpVerCookie.setSecure(request.isSecure());
      cwmpVerCookie.setHttpOnly(true);
      response.addCookie(cookie);
      response.addCookie(cwmpVerCookie);
    } catch (Exception e) {
      response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
      return;
    }
    TransferCompleteResponse tr = new TransferCompleteResponse(tc.getId());
    tr.setCWMPVersion(msg.getCWMPVersion());
    tr.writeTo(out);
  }

  private void processOperationResult(TR069RPC msg, HttpServletResponse response, String reqType,
      String acsSessionID, ByteArrayOutputStream out) {
    logger.debug("Received Operation Result response {}", msg);
    if (null == acsSessionID) {
      logger.error("Received response without session ID, response: {}", reqType);
    } else {
      try {
        TR069RPC message = deviceEventHandler.processRPCResponse(msg, acsSessionID);
        if (null != message) {
          message.setCWMPVersion(msg.getCWMPVersion());
          message.writeTo(out);
        } else {
          response.setStatus(HttpServletResponse.SC_NO_CONTENT);
        }
      } catch (TR069EventProcessingException tr069ex) {
        response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
      }
    }
  }

  private void processEmptyCPERequest(HttpServletResponse response, String cwmpVersion,
      String acsSessionID, ByteArrayOutputStream out) {
    if (null == acsSessionID) {
      logger.error("Received empty response without session ID");
      response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
      return;
    }

    try {
      logger.info("Received Empty Device response");
      TR069RPC message = deviceEventHandler.processEmptyRequest(acsSessionID);
      if (null != message) {
        message.setCWMPVersion(cwmpVersion);
        message.writeTo(out);
      } else {
        response.setStatus(HttpServletResponse.SC_NO_CONTENT);
      }
    } catch (TR069EventProcessingException tr069ex) {
      response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
    }
  }

  private void clearCookies(Cookie[] cookies, HttpServletResponse response) {
    Cookie cookieToDelete = null;
    if (null != cookies) {
      logger.debug("Clearing the cookies");
      for (int i = 0; i < cookies.length; i++) {
        if (cookies[i].getName().equals(ACS_SESSIONID)
            || cookies[i].getName().equals(CWMP_VERSION)) {
          cookieToDelete = cookies[i];
          cookieToDelete.setMaxAge(0);
          response.addCookie(cookieToDelete);
        }
      }
    }
  }

  private static class XmlFilterInputStream extends InputStream {

    private InputStream istream;
    private int lvl;
    private int lastchar;
    @SuppressWarnings("unused")
    private int len;
    private int nextchar;
    private boolean intag = false;
    private StringBuilder buff = new StringBuilder(16);

    /** Creates a new instance of xmlFilterInputStream */
    public XmlFilterInputStream(InputStream is, int l) {
      len = l;
      istream = is;
    }


    @Override
    public int read() throws IOException {
      if (lastchar == '>' && lvl == 0) {
        return -1;
      }
      int l = lastchar;
      if (!readLastChar())
        return lastchar;

      if (!intag && lastchar == '&') {
        int amppos = buff.length();
        updateBuffer();
        String s = buff.substring(amppos);
        replaceSpecialChars(s, amppos);
        return read();
      }

      if (l == '<') {
        intag = true;
        if (lastchar == '/') {
          lvl--;
        } else {
          lvl++;
        }
      }

      len--;
      return lastchar;
    }

    public boolean next() throws IOException {
      if ((nextchar = istream.read()) == -1) {
        logger.debug("Next char is {}", nextchar);
        lvl = 0;
        lastchar = 0;
      }
      return (nextchar != -1);
    }

    private boolean readLastChar() throws IOException {
      if (nextchar != -1) {
        lastchar = nextchar;
        nextchar = -1;
      } else {
        if (buff.length() > 0) {
          lastchar = buff.charAt(0);
          buff.deleteCharAt(0);
          return false;
        } else {
          lastchar = istream.read();
        }
      }

      if (lastchar == '<') {
        intag = true;
      } else if (lastchar == '>') {
        intag = false;
      }

      return true;
    }

    private void updateBuffer() throws IOException {
      // fix up broken xml not encoding &
      buff.append((char) lastchar);
      for (int c = 0; c < 10; c++) {
        int ch = istream.read();
        boolean breakLoop = false;
        if (ch == -1) {
          breakLoop = true;
        }
        if (ch == '&') {
          nextchar = ch;
          breakLoop = true;
        }
        if (breakLoop)
          break;

        buff.append((char) ch);
      }
    }

    private void replaceSpecialChars(String s, int amppos) {
      if (!s.startsWith("&amp;") && !s.startsWith("&lt;") && !s.startsWith("&gt;")
          && !s.startsWith("&apos;") && !s.startsWith("&quot;") && !s.startsWith("&#")) {
        buff.replace(amppos, amppos + 1, "&amp;");
      }
    }
  }

  private static class XmlFilterNS extends InputStream {
    // Dumb class to filter out declaration of default xmlns

    private String pat = "xmlns=\"urn:dslforum-org:cwmp-1-0\"";
    private String pat2 = "xmlns=\"urn:dslforum-org:cwmp-1-1\"";
    private int length = 0;
    private int pos = 0;
    private boolean f = false;
    private byte[] buff = new byte[1024];
    private InputStream is;

    @Override
    public int read() throws IOException {
      if (!f) {
        length = is.read(buff);
        if (length < buff.length) {
          byte[] b2 = new byte[length];
          System.arraycopy(buff, 0, b2, 0, length);
          buff = b2;
        }

        String b = new String(buff, StandardCharsets.UTF_8);
        b = b.replace(pat, "");
        b = b.replace(pat2, "");
        buff = b.getBytes(StandardCharsets.UTF_8);
        length = buff.length;
        f = true;
      }

      if (pos < length) {
        return buff[pos++] & 0xFF;
      }
      return is.read();
    }

    public XmlFilterNS(InputStream is) {
      this.is = is;
    }
  }

  private static class CharsetConverterInputStream extends InputStream {

    @SuppressWarnings("unused")
    private InputStream in;
    private PipedInputStream pipein;
    private OutputStream pipeout;
    private Reader r;
    private Writer w;

    public CharsetConverterInputStream(String csFrom, String csTo, InputStream in)
        throws IOException {
      this.in = in;
      r = new InputStreamReader(in, csFrom);
      pipein = new PipedInputStream();
      pipeout = new PipedOutputStream(pipein);
      w = new OutputStreamWriter(pipeout, csTo);
    }

    @Override
    public int read() throws IOException {
      if (pipein.available() > 0) {
        return pipein.read();
      }
      int c = r.read();
      if (c == -1) {
        return -1;
      }
      w.write(c);
      w.flush();
      return pipein.read();
    }
  }

  /**
   * @return
   * @throws Exception
   */
  private MessageFactory getSOAPMessageFactory() throws SOAPException {
    MessageFactory mf = null;
    mf = MessageFactory.newInstance();
    return mf;
  }

  /**
   * @param cookies
   * @return
   */
  private String getACSSessionCookieData(Cookie[] cookies) {
    String acsSessionID = null;
    if (null != cookies) {
      for (Cookie cookie : cookies) {
        if (cookie.getName().equals(ACS_SESSIONID)) {
          acsSessionID = cookie.getValue();
          logger.debug("The session id is {}", acsSessionID);
        }
      }
    }
    return acsSessionID;
  }

  /**
   * @param cookies
   * @return
   * @throws TR069EventProcessingException
   */
  private String getCWMPVersionCookieData(Cookie[] cookies) throws TR069EventProcessingException {
    String cwmpVersion = null;
    try {
      if (null != cookies) {
        for (Cookie cookie : cookies) {
          if (cookie.getName().equals(CWMP_VERSION)) {
            cwmpVersion = cookie.getValue();
            if (cwmpVersion != null) {
              cwmpVersion = URLDecoder.decode(cwmpVersion, StandardCharsets.UTF_8.name());
            }
            logger.debug("The CWMP version supported by the device is: {}", cwmpVersion);
          }
        }
      }
    } catch (UnsupportedEncodingException e) {
      logger.error(e.getMessage());
      TR069EventProcessingException ex = new TR069EventProcessingException(
          ErrorCode.UNSUPPORTED_CHARACTER_ENCODING, StandardCharsets.UTF_8.name());
      logger.error(ex.getMessage());
      throw ex;
    }
    return cwmpVersion;
  }

  /**
   * @param soapMsg
   */
  private void logSoapMsg(SOAPMessage soapMsg) {
    StringBuilder buffer = new StringBuilder();
    buffer.append(soapMsg.toString());
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    try {
      soapMsg.writeTo(baos);
    } catch (SOAPException | IOException e) {
      logger.error("Error while writting soap message");
    }
    buffer.append(baos);
    String soapMessage = buffer.toString();
    logger.debug(soapMessage);
  }

  /**
   * @param response
   * @param out
   * @param fcodeAcsRequestDenied
   * @param faultString
   * @param id
   * @throws IOException
   */
  private void sendFault(HttpServletResponse response, ByteArrayOutputStream out,
      String fcodeAcsRequestDenied, String faultString, String id) throws IOException {
    Fault fault = new Fault(fcodeAcsRequestDenied, faultString, id);
    fault.writeTo(out);
    response.setContentLength(out.size());
    String sout = out.toString(UTF_8);
    sout = sout.replace('\'', '"');
    response.getOutputStream().print(sout);
  }

  private void logHeaderElements(Enumeration<String> headerName) {
    while (headerName.hasMoreElements()) {
      String requestHeader = headerName.nextElement();
      logger.debug("Request Headers {}", requestHeader);
    }
  }

  private String getRequestType(TR069RPC msg) {
    String requestType = msg.getName();
    if (requestType == null)
      requestType = "Empty Request";

    return requestType;
  }

  /******************************************************************************************************************/

  public DeviceEventHandler getDeviceEventHandler() {
    return deviceEventHandler;
  }

  public void setDeviceEventHandler(DeviceEventHandler deviceEventHandler) {
    this.deviceEventHandler = deviceEventHandler;
  }

}
