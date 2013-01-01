package com.rarnu.tools.root.dns.record;

import java.io.IOException;

import com.rarnu.tools.root.dns.DNSInputStream;
import com.rarnu.tools.root.dns.DNSRR;

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
