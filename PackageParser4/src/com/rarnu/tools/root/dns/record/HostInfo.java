package com.rarnu.tools.root.dns.record;

import com.rarnu.tools.root.dns.DNSInputStream;
import com.rarnu.tools.root.dns.DNSRR;

import java.io.IOException;

public class HostInfo extends DNSRR {
    private String cpu, os;

    protected void decode(DNSInputStream dnsIn) throws IOException {
        cpu = dnsIn.readString();
        os = dnsIn.readString();
    }

    public String getCPUInfo() {
        return cpu;
    }

    public String getOSInfo() {
        return os;
    }

    public String toString() {
        return getRRName() + "\tOS = " + os + ", CPU = " + cpu;
    }
}
