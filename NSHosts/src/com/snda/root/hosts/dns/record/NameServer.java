package com.snda.root.hosts.dns.record;

import java.io.IOException;

import com.snda.root.hosts.dns.DNSInputStream;
import com.snda.root.hosts.dns.DNSRR;

public class NameServer extends DNSRR {
	private String nameServer;

	protected void decode(DNSInputStream dnsIn) throws IOException {
		nameServer = dnsIn.readDomainName();
	}

	public String getNameServer() {
		return nameServer;
	}

	public String toString() {
		return getRRName() + "\tnameserver = " + nameServer;
	}
}
