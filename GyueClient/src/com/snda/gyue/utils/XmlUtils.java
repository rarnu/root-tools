package com.snda.gyue.utils;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.StringWriter;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xmlpull.v1.XmlSerializer;

import android.content.Context;
import android.util.Xml;

/**
 * BBXmlUtils<br>
 * for xml operating
 * 
 * @author hexiaojie<br>
 *         Bambook in GuoKe
 */
public class XmlUtils {

	private DocumentBuilderFactory docBuilderFactory = null;
	private DocumentBuilder docBuilder = null;
	private Document doc = null;

	/**
	 * initialize dom objects<br>
	 * must be called BEFORE do operation to a xml file
	 */
	public void initialize() {
		try {
			docBuilderFactory = DocumentBuilderFactory.newInstance();
			docBuilder = docBuilderFactory.newDocumentBuilder();
		} catch (Exception e) {

		}
	}

	/**
	 * finalize dom objects<br>
	 * must be called AFTER do operations to a xml file
	 */
	public void finalize() {
		doc = null;
		docBuilder = null;
		docBuilderFactory = null;
	}

	/**
	 * Load a local xml/text file
	 * 
	 * @param fileName
	 */
	public boolean loadFile(Context context, String fileName) {
		try {
			InputStream is = context.openFileInput(fileName);
			doc = docBuilder.parse(is);
			is.close();
			return true;
		} catch (Exception e) {
			return false;
		}
	}

	/**
	 * Load a xml file with file
	 * @param context
	 * @param file
	 * @return
	 */
	public boolean loadFile(Context context, File file) {
		try {
			InputStream is = new FileInputStream(file);
			doc = docBuilder.parse(is);
			is.close();
			return true;
		} catch (Exception e) {
			return false;
		}
	}

	/**
	 * Save current Document to xml/text file
	 * 
	 * @param fileName
	 */
	public void saveFile(Context context, String fileName) {
		TransformerFactory transFactory = null;
		Transformer transformer = null;
		try {
			transFactory = TransformerFactory.newInstance();
			transformer = transFactory.newTransformer();
			DOMSource source = new DOMSource(doc);
			StreamResult result = new StreamResult(new File(fileName));
			transformer.transform(source, result);
		} catch (Exception e) {
		} finally {
			transFactory = null;
			transformer = null;
		}
	}

	/**
	 * Get a document instance<br>
	 * If you feel that you need call this for some features, please call ME
	 * first ^_^
	 * 
	 * @return
	 */
	public Document getDocument() {
		return doc;
	}

	/**
	 * Get the root node
	 * 
	 * @return
	 */
	public Element getRoot() {
		return doc.getDocumentElement();
	}

	/**
	 * Create a new xml node
	 * 
	 * @param nodeName
	 * @param nodeValue
	 * @return
	 */
	public Element createNode(String nodeName, String nodeValue) {
		Element newNode = doc.createElement(nodeName);
		if (nodeValue != null) {
			newNode.appendChild(doc.createTextNode(nodeValue));
		}
		return newNode;
	}

	/**
	 * Add an attribute for a node
	 * 
	 * @param node
	 * @param attrName
	 * @param attrValue
	 */
	public void addAttribute(Element node, String attrName, String attrValue) {
		Attr attr = doc.createAttribute(attrName);
		attr.setValue(attrValue);
		node.setAttributeNode(attr);
	}

	/**
	 * Get an atribute's value under a node
	 * 
	 * @param node
	 * @param attrName
	 * @return
	 */
	public String getAttributeValue(Element node, String attrName) {
		return node.getAttributeNode(attrName).getValue();
	}

	/**
	 * Get a node's value
	 * 
	 * @param node
	 * @return
	 */
	public String getNodeValue(Element node) {
		if (node.getFirstChild() == null) {
			return null;
		}
		return node.getFirstChild().getNodeValue();
	}

	/**
	 * Get the xml text
	 * 
	 * @return
	 */
	public String getXmlContent() {
		return xmlToStr();
	}

	/** @hide */
	private String xmlToStr() {
		XmlSerializer serializer = Xml.newSerializer();
		StringWriter writer = new StringWriter();
		try {
			serializer.setOutput(writer);
			serializer.startDocument("utf-8", true);

			Element root = doc.getDocumentElement();
			dealNode(root, serializer);
			serializer.endDocument();

			return writer.toString();
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	/** @hide */
	private void dealNode(Node node, XmlSerializer serializer) {
		try {
			if (node.getNodeType() == Node.TEXT_NODE) {
				serializer.text(node.getNodeValue());
				return;
			}

			serializer.startTag("", node.getNodeName());

			String text = node.getNodeValue();
			if (text != null)
				serializer.text(text);

			NamedNodeMap map = node.getAttributes();
			int attrSize = map.getLength();
			for (int i = 0; i < attrSize; i++) {

				Node Attrnode = map.item(i);

				String name = Attrnode.getNodeName();
				String value = Attrnode.getNodeValue();
				serializer.attribute("", name, value);

			}

			NodeList childs = node.getChildNodes();
			int nodeSize = childs.getLength();
			for (int i = 0; i < nodeSize; i++) {

				Node Childnode = childs.item(i);
				dealNode(Childnode, serializer);
			}
			serializer.endTag("", node.getNodeName());

		} catch (Exception e) {
		}
	}

}
