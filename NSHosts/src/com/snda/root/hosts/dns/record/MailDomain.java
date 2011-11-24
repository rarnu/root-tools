package com.snda.root.hosts.dns.record;

import java.io.IOException;

import com.snda.root.hosts.dns.DNSInputStream;
import com.snda.root.hosts.dns.DNSRR;


public class MailDomain extends DNSRR {
  private String mailDestination;

  protected void decode (DNSInputStream dnsIn) throws IOException {
    mailDestination = dnsIn.readDomainName ();
  }

  public String getMailDestination () {
    return mailDestination;
  }

  public String toString () {
    return getRRName () + "\tmail destination = " + mailDestination;
  }
}
