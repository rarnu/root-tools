package com.rarnu.tools.root.dns.record;

import com.rarnu.tools.root.dns.DNSInputStream;
import com.rarnu.tools.root.dns.DNSRR;

import java.io.IOException;
import java.util.Enumeration;
import java.util.Vector;

public class Text extends DNSRR {
    private Vector<String> texts = new Vector<String>();

    protected void decode(DNSInputStream dnsIn) throws IOException {
        String s;
        while ((s = dnsIn.readString()) != null) {
            texts.addElement(s);
        }
    }

    public Enumeration<String> getTexts() {
        return texts.elements();
    }

    public String toString() {
        StringBuffer result = new StringBuffer();
        for (int i = 0; i < texts.size(); ++i) {
            if (i > 0) {
                result.append("\n\t\t");
            }
            result.append(texts.elementAt(i));
        }
        return getRRName() + "\ttext = " + result;
    }
}
