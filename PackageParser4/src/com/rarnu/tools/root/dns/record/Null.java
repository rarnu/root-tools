package com.rarnu.tools.root.dns.record;

import com.rarnu.tools.root.dns.DNSInputStream;
import com.rarnu.tools.root.dns.DNSRR;

import java.io.IOException;

public class Null extends DNSRR {
    private byte[] data;
    private String text;

    protected void decode(DNSInputStream dnsIn) throws IOException {
        data = new byte[dnsIn.available()];
        dnsIn.read(data);
        text = new String(data, "latin1");
    }

    public byte[] getNullData() {
        byte[] copy = new byte[data.length];
        System.arraycopy(data, 0, copy, 0, data.length);
        return copy;
    }

    public String toString() {
        return getRRName() + "\tnull data = [" + text + ']';
    }
}
