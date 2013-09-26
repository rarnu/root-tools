package com.rarnu.tools.root.dns;

import com.rarnu.tools.root.dns.record.Address;

import java.io.*;
import java.net.Socket;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;

public class NSLookup {

    public static List<Address> nslookup(String domain, String nameserver) {
        List<Address> result = null;
        DNSQuery query = new DNSQuery(domain, DNS.TYPE_ANY, DNS.CLASS_IN);
        try {
            Socket socket = new Socket(nameserver, DNS.DEFAULT_PORT);
            socket.setSoTimeout(10000);
            sendQuery(query, socket);
            getResponse(query, socket);
            socket.close();
            Enumeration<DNSRR> answers = query.getAnswers();
            result = new ArrayList<Address>();
            while (answers.hasMoreElements()) {
                DNSRR dr = answers.nextElement();
                if (dr != null) {
                    if (dr.getRRType() == DNS.CLASS_IN) {
                        result.add((Address) dr);
                    }
                }
            }
        } catch (IOException ex) {

        }
        return result;
    }

    public static void sendQuery(DNSQuery query, Socket socket) throws IOException {
        BufferedOutputStream bufferedOut = new BufferedOutputStream(socket.getOutputStream());
        DataOutputStream dataOut = new DataOutputStream(bufferedOut);
        byte[] data = query.extractQuery();
        dataOut.writeShort(data.length);
        dataOut.write(data);
        dataOut.flush();
    }

    public static void getResponse(DNSQuery query, Socket socket) throws IOException {
        InputStream bufferedIn = new BufferedInputStream(socket.getInputStream());
        DataInputStream dataIn = new DataInputStream(bufferedIn);
        int responseLength = dataIn.readUnsignedShort();
        byte[] data = new byte[responseLength];
        dataIn.readFully(data);

        query.receiveResponse(data, responseLength);
    }
}
