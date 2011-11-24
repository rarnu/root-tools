package com.snda.root.hosts.dns.record;

import java.io.IOException;

import com.snda.root.hosts.dns.DNSInputStream;
import com.snda.root.hosts.dns.DNSRR;


public class Pointer extends DNSRR {
  private String pointer;

  protected void decode (DNSInputStream dnsIn) throws IOException {
    pointer = dnsIn.readDomainName ();
  }

  public String getPointer () {
    return pointer;
  }

  public String toString () {
    return getRRName () + "\tpointer = " + pointer;
  }
}
