package com.rarnu.tools.root.dns;

import java.io.IOException;

public abstract class DNSRR {
    private String rrName;
    private int rrType, rrClass;
    private long rrTTL, rrCreated;

    void init(String name, int type, int clas, long ttl, DNSInputStream dnsIn) throws IOException {
        rrName = name;
        rrType = type;
        rrClass = clas;
        rrTTL = ttl;
        rrCreated = System.currentTimeMillis();
        decode(dnsIn);
    }

    protected abstract void decode(DNSInputStream dnsIn) throws IOException;

    public String getRRName() {
        return rrName;
    }

    public int getRRType() {
        return rrType;
    }

    public int getRRClass() {
        return rrClass;
    }

    public long getRRTTL() {
        return rrTTL;
    }

    public boolean isValid() {
        return rrTTL * 1000 > System.currentTimeMillis() - rrCreated;
    }
}
