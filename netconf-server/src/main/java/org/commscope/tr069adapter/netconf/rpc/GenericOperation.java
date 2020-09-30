package org.commscope.tr069adapter.netconf.rpc;

import org.commscope.tr069adapter.mapper.model.ErrorCodeDetails;
import org.commscope.tr069adapter.mapper.model.NetConfResponse;
import org.commscope.tr069adapter.netconf.boot.NetConfServiceBooter;
import org.commscope.tr069adapter.netconf.config.NetConfServerProperties;
import org.opendaylight.netconf.api.DocumentedException;
import org.opendaylight.netconf.api.xml.XmlElement;
import org.opendaylight.netconf.api.xml.XmlNetconfConstants;
import org.opendaylight.netconf.mapping.api.HandlingPriority;
import org.opendaylight.netconf.mapping.api.NetconfOperation;
import org.opendaylight.netconf.mapping.api.NetconfOperationChainedExecution;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.xml.sax.InputSource;

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import java.io.StringReader;


public abstract class GenericOperation implements NetconfOperation {
    private static final Logger logger = LoggerFactory.getLogger(GenericOperation.class);

    private String opNamespace;
    private String opName;
    private String opString;

    protected String deviceID;
    protected String swVersion;
    protected String hwVersion;

    public HandlingPriority canHandle(final Document message) throws DocumentedException {
        OperationNameAndNamespace operationNameAndNamespace = new OperationNameAndNamespace(message);
        return canHandle(operationNameAndNamespace.getOperationName(),
            operationNameAndNamespace.getNamespace());
    }

    protected HandlingPriority canHandle(final String operationName,
        final String operationNamespace) {
        return operationName.equals(getOpName())
            && operationNamespace.equals(getOpNamespace())
            ? HandlingPriority.HANDLE_WITH_DEFAULT_PRIORITY.increasePriority(1100)
            : HandlingPriority.CANNOT_HANDLE;
    }

    public Document handle(Document requestMessage,
        NetconfOperationChainedExecution subsequentOperation) throws DocumentedException {
        logger.debug("rpc is received in netconfserver");

        final XmlElement requestElement = XmlElement.fromDomDocument(requestMessage);

        final String msgId = requestElement.getAttribute(XmlNetconfConstants.MESSAGE_ID);
        final Element element =
            requestMessage.createElementNS(XmlNetconfConstants.URN_IETF_PARAMS_XML_NS_NETCONF_BASE_1_0,
                XmlNetconfConstants.RPC_REPLY_KEY);
        element.setAttribute("xmlns:ns1", getOpNamespace());
        element.setAttribute("message-id", msgId);

        String requestXml = XmlUtility.convertDocumentToString(requestElement);
        logger.debug("rpc requestXml= {}", requestXml);

        NetConfServerProperties config =
            NetConfServiceBooter.getApplicationContext().getBean(NetConfServerProperties.class);

        final String baseUrl = config.getMapperPath() + "/" + getOpString();
        NetConfResponse restResponse =
            XmlUtility.invokeMapperCall(baseUrl, requestXml, deviceID, swVersion, hwVersion);
        Document document = null;

        ErrorCodeDetails errorCode = restResponse.getErrorCode();
        if (errorCode != null && errorCode.getFaultCode() != null
            && !errorCode.getFaultCode().equalsIgnoreCase("0")) {
            logger.error("Error recevied : {}", errorCode);
            throw new DocumentedException(errorCode.getErrorMessage(),
                DocumentedException.ErrorType.from(errorCode.getErrorType()), DocumentedException.ErrorTag
                .from(errorCode.getErrorTag()),
                DocumentedException.ErrorSeverity.from(errorCode.getErrorSeverity()));
        } else if (restResponse.getNetconfResponseXml() != null) {
            logger.debug("rpc response received from mapper: {}",
                restResponse.getNetconfResponseXml());
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            factory.setAttribute(XMLConstants.ACCESS_EXTERNAL_DTD, "");
            factory.setAttribute(XMLConstants.ACCESS_EXTERNAL_SCHEMA, "");
            try {
                Node child = requestMessage.createElement(XmlNetconfConstants.OK);
                element.appendChild(child);
                String xmlStr = XmlUtility.convertDocumentToString(element);
                document = getDocumentDetails(factory, xmlStr);
            } catch (Exception e) {
                logger.error("Error while contruscting the response: {}", e.getMessage());
                throw new DocumentedException("Operation Aborted", DocumentedException.ErrorType.from("application"),
                    DocumentedException.ErrorTag.from("operation-failed"), DocumentedException.ErrorSeverity
                    .from("ERROR"));
            }
        }
        return document;
    }

    protected Document getDocumentDetails(DocumentBuilderFactory factory, String xmlStr)
        throws DocumentedException {
        DocumentBuilder builder;
        Document document;
        try {
            builder = factory.newDocumentBuilder();
            document = builder.parse(new InputSource(new StringReader(xmlStr)));
        } catch (Exception e) {
            logger.error("Error while converting String to element: {}", e.getMessage());
            throw new DocumentedException("Operation Aborted", DocumentedException.ErrorType.from("application"),
                DocumentedException.ErrorTag.from("operation-failed"), DocumentedException.ErrorSeverity.from("ERROR"));
        }
        return document;
    }

    public static final class OperationNameAndNamespace {
        private final String operationName;
        private final String namespace;

        private final XmlElement operationElement;

        public OperationNameAndNamespace(final Document message) throws DocumentedException {
            XmlElement requestElement = null;
            requestElement = getRequestElementWithCheck(message);
            operationElement = requestElement.getOnlyChildElement();
            operationName = operationElement.getName();
            namespace = operationElement.getNamespace();
        }

        public String getOperationName() {
            return operationName;
        }

        public String getNamespace() {
            return namespace;
        }

        public XmlElement getOperationElement() {
            return operationElement;
        }

    }

    protected static XmlElement getRequestElementWithCheck(final Document message)
        throws DocumentedException {
        return XmlElement.fromDomElementWithExpected(message.getDocumentElement(),
            XmlNetconfConstants.RPC_KEY, XmlNetconfConstants.URN_IETF_PARAMS_XML_NS_NETCONF_BASE_1_0);
    }

    public String getOpNamespace() {
        return opNamespace;
    }

    public void setOpNamespace(String opNamespace) {
        this.opNamespace = opNamespace;
    }

    public String getOpName() {
        return opName;
    }

    public void setOpName(String opName) {
        this.opName = opName;
    }

    public String getOpString() {
        return opString;
    }

    public void setOpString(String opString) {
        this.opString = opString;
    }
}
