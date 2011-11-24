package com.snda.root.hosts.dns.record;

import java.io.IOException;
import java.net.InetAddress;
import java.net.UnknownHostException;

import com.snda.root.hosts.dns.DNSInputStream;
import com.snda.root.hosts.dns.DNSRR;


public class WellKnownServices extends DNSRR {
  private int[] ipAddress = new int[4];
  private int protocol;
  private byte[] data;

  protected void decode (DNSInputStream dnsIn) throws IOException {
    for (int j = 0; j < 4; ++ j)
      ipAddress[j] = dnsIn.readByte ();
    protocol = dnsIn.readByte ();
    data = new byte[dnsIn.available ()];
    dnsIn.read (data);
  }

  public byte[] getAddress () {
    byte[] ip = new byte[4];
    for (int j = 0; j < 4; ++ j)
      ip[j] = (byte) ipAddress[j];
    return ip;
  }

  public InetAddress getInetAddress () throws UnknownHostException {
    return InetAddress.getByName (toByteString ());
  }

  public int getProtocol () {
    return protocol;
  }

  public byte[] getData () {
    byte[] copy = new byte[data.length];
    System.arraycopy (data, 0, copy, 0, data.length);
    return copy;
  }

  private String toByteString () {
    return ipAddress[0] + "." + ipAddress[1] + "." +
      ipAddress[2] + "." + ipAddress[3];
  }

  public String toString () {
    StringBuffer services = new StringBuffer ();
    for (int i = data.length - 1; i >= 0; -- i)
      for (int j = 7; j >= 0; -- j)
	services.append ((data[i] >>> j) & 1);
    return getRRName () + "\twell-known services" +
      "\n\taddress = " + toByteString () +
      "\n\tprotocol = " + protocol +
      "\n\tservices = %" + services;
  }
}
