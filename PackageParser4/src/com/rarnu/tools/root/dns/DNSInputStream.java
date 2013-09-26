package com.rarnu.tools.root.dns;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.EOFException;
import java.io.IOException;

public class DNSInputStream extends ByteArrayInputStream {
    protected DataInputStream dataIn;
    private DNSInputStream dnsIn;

    public DNSInputStream(byte[] data, int off, int len) {
        super(data, off, len);
        dataIn = new DataInputStream(this);
    }

    public int readByte() throws IOException {
        return dataIn.readUnsignedByte();
    }

    public int readShort() throws IOException {
        return dataIn.readUnsignedShort();
    }

    public long readInt() throws IOException {
        return dataIn.readInt() & 0xffffffffL;
    }

    public String readString() throws IOException {
        int len = readByte();
        if (len == 0) {
            return "";
        } else {
            byte[] buffer = new byte[len];
            dataIn.readFully(buffer);
            return new String(buffer, "latin1");
        }
    }

    public String readDomainName() throws IOException {
        if (pos >= count) {
            throw new EOFException("EOF reading domain name");
        }
        if ((buf[pos] & 0xc0) == 0) {
            String label = readString();
            if (label.length() > 0) {
                String tail = readDomainName();
                if (tail.length() > 0) {
                    label = label + '.' + tail;
                }
            }
            return label;
        } else {
            if ((buf[pos] & 0xc0) != 0xc0) {
                throw new IOException("Invalid domain name compression offset");
            }
            int offset = readShort() & 0x3fff;
            dnsIn = new DNSInputStream(buf, offset, buf.length - offset);
            return dnsIn.readDomainName();
        }
    }

    public DNSRR readRR() throws IOException {
        String rrName = readDomainName();
        int rrType = readShort();
        int rrClass = readShort();
        long rrTTL = readInt();
        int rrDataLen = readShort();
        DNSInputStream rrDNSIn = new DNSInputStream(buf, pos, rrDataLen);
        pos += rrDataLen;
        try {
            String myName = getClass().getName();
            int periodIndex = myName.lastIndexOf('.');
            String myPackage = myName.substring(0, 1 + periodIndex);
            Class<?> theClass = Class.forName(myPackage + "record." + DNS.typeName(rrType));
            DNSRR rr = (DNSRR) theClass.newInstance();
            rr.init(rrName, rrType, rrClass, rrTTL, rrDNSIn);
            return rr;
        } catch (Exception ex) {
            return null;
        }
    }
}
