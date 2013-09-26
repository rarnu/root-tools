package com.rarnu.tools.root.dns.record;

import com.rarnu.tools.root.dns.DNSInputStream;
import com.rarnu.tools.root.dns.DNSRR;

import java.io.IOException;
import java.net.InetAddress;
import java.net.UnknownHostException;

public class Address extends DNSRR {
    private int[] ipAddress = new int[4];

    protected void decode(DNSInputStream dnsIn) throws IOException {
        for (int i = 0; i < 4; ++i)
            ipAddress[i] = dnsIn.readByte();
    }

    public byte[] getAddress() {
        byte[] ip = new byte[4];
        for (int j = 0; j < 4; ++j) {
            ip[j] = (byte) ipAddress[j];
        }
        return ip;
    }

    public InetAddress getInetAddress() throws UnknownHostException {
        return InetAddress.getByName(toByteString());
    }

    public String toByteString() {
        return ipAddress[0] + "." + ipAddress[1] + "." + ipAddress[2] + "." + ipAddress[3];
    }

    public String toString() {
        return getRRName() + "\tinternet address = " + toByteString();
    }
}
