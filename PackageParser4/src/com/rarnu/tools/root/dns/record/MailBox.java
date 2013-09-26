package com.rarnu.tools.root.dns.record;

import com.rarnu.tools.root.dns.DNSInputStream;
import com.rarnu.tools.root.dns.DNSRR;

import java.io.IOException;

public class MailBox extends DNSRR {
    private String mailBox;

    protected void decode(DNSInputStream dnsIn) throws IOException {
        mailBox = dnsIn.readDomainName();
    }

    public String getMailbox() {
        return mailBox;
    }

    public String toString() {
        return getRRName() + "\tmailbox = " + mailBox;
    }
}
