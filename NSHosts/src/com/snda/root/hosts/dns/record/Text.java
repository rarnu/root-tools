package com.snda.root.hosts.dns.record;

import java.io.IOException;
import java.util.Enumeration;
import java.util.Vector;

import com.snda.root.hosts.dns.DNSInputStream;
import com.snda.root.hosts.dns.DNSRR;


public class Text extends DNSRR {
  private Vector<String> texts = new Vector<String>();

  protected void decode (DNSInputStream dnsIn) throws IOException {
    String s;
    while ((s = dnsIn.readString ()) != null)
      texts.addElement (s);
  }

  public Enumeration<String> getTexts () {
    return texts.elements ();
  }

  public String toString () {
    StringBuffer result = new StringBuffer ();
    for (int i = 0; i < texts.size (); ++ i) {
      if (i > 0)
	result.append ("\n\t\t");
      result.append (texts.elementAt (i));
    }
    return getRRName () + "\ttext = " + result;
  }
}
