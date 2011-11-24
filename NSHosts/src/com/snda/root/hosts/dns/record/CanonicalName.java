package com.snda.root.hosts.dns.record;

import java.io.IOException;

import com.snda.root.hosts.dns.DNSInputStream;
import com.snda.root.hosts.dns.DNSRR;

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
