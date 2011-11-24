package com.snda.root.hosts.dns;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.Socket;
import java.util.Enumeration;

import com.snda.root.hosts.dns.record.Address;

public class NSLookup {
	public static void main(String[] args) {
		args = new String[1];
		args[0] = "code.google.com";
		int atIdx = args[0].indexOf("@");
		String nameServer = (atIdx > -1) ? args[0].substring(atIdx + 1)
				: "8.8.8.8";
		String hostName = (atIdx > -1) ? args[0].substring(0, atIdx) : args[0];
		System.out.println("Nameserver: " + nameServer);
		System.out.println("Request: " + hostName);
		DNSQuery query = new DNSQuery(hostName, DNS.TYPE_ANY, DNS.CLASS_IN);
		try {
			Socket socket = new Socket(nameServer, DNS.DEFAULT_PORT);
			socket.setSoTimeout(10000);
			sendQuery(query, socket);
			getResponse(query, socket);
			socket.close();
			printRRs(query);
		} catch (IOException ex) {
			System.out.println(ex);
		}
	}

	public static void sendQuery(DNSQuery query, Socket socket)
			throws IOException {
		BufferedOutputStream bufferedOut = new BufferedOutputStream(socket
				.getOutputStream());
		DataOutputStream dataOut = new DataOutputStream(bufferedOut);
		byte[] data = query.extractQuery();
		dataOut.writeShort(data.length);
		dataOut.write(data);
		dataOut.flush();
	}

	public static void getResponse(DNSQuery query, Socket socket)
			throws IOException {
		InputStream bufferedIn = new BufferedInputStream(socket
				.getInputStream());
		DataInputStream dataIn = new DataInputStream(bufferedIn);
		int responseLength = dataIn.readUnsignedShort();
		byte[] data = new byte[responseLength];
		dataIn.readFully(data);

		query.receiveResponse(data, responseLength);
	}

	public static void printRRs(DNSQuery query) {
		Enumeration<DNSRR> answers = query.getAnswers();
		
		while (answers.hasMoreElements()) {
			DNSRR dr = answers.nextElement();
			if (dr != null) {
				if (dr.getRRType() == DNS.CLASS_IN) {
					System.out.println(dr.getRRName());
					System.out.println(((Address) dr).toByteString());

					System.out.println("=========================");
				}
			}
		}
	}
}
