package com.rarnu.tools.root.dns.record;

import com.rarnu.tools.root.dns.DNSInputStream;
import com.rarnu.tools.root.dns.DNSRR;

import java.io.IOException;

public class StartOfAuthority extends DNSRR {
    private String origin, mailAddress;
    private long serial, refresh, retry, expire, ttl;

    protected void decode(DNSInputStream dnsIn) throws IOException {
        origin = dnsIn.readDomainName();
        mailAddress = dnsIn.readDomainName();
        serial = dnsIn.readInt();
        refresh = dnsIn.readInt();
        retry = dnsIn.readInt();
        expire = dnsIn.readInt();
        ttl = dnsIn.readInt();
    }

    public String getOrigin() {
        return origin;
    }

    public String getMailAddress() {
        return mailAddress;
    }

    public long getSerial() {
        return serial;
    }

    public long getRefresh() {
        return refresh;
    }

    public long getRetry() {
        return retry;
    }

    public long getExpire() {
        return expire;
    }

    public long getTTL() {
        return ttl;
    }

    public String toString() {
        return getRRName() + "\tstart of authority\n\torigin = " + origin + "\n\tmail address = " + mailAddress + "\n\tserial = " + serial + "\n\trefresh = " + refresh + "\n\tretry = " + retry + "\n\texpire = " + expire + "\n\tminimum TTL = " + ttl;
    }
}
