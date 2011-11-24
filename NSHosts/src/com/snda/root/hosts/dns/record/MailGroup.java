package com.snda.root.hosts.dns.record;

import java.io.IOException;

import com.snda.root.hosts.dns.DNSInputStream;
import com.snda.root.hosts.dns.DNSRR;


public class MailGroup extends DNSRR {
  private String mailGroup;

  protected void decode (DNSInputStream dnsIn) throws IOException {
    mailGroup = dnsIn.readDomainName ();
  }

  public String getMailGroup () {
    return mailGroup;
  }

  public String toString () {
    return getRRName () + "\tmail group = " + mailGroup;
  }
}
