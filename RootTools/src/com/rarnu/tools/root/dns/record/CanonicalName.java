package com.rarnu.tools.root.dns.record;

import java.io.IOException;

import com.rarnu.tools.root.dns.DNSInputStream;
import com.rarnu.tools.root.dns.DNSRR;

public class CanonicalName extends DNSRR {
	private String canonicalName;

	protected void decode(DNSInputStream dnsIn) throws IOException {
		canonicalName = dnsIn.readDomainName();
	}

	public String getCanonicalName() {
		return canonicalName;
	}

	public String toString() {
		return getRRName() + "\tcanonical name = " + canonicalName;
	}
}
