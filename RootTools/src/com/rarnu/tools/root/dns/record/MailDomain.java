package com.rarnu.tools.root.dns.record;

import java.io.IOException;

import com.rarnu.tools.root.dns.DNSInputStream;
import com.rarnu.tools.root.dns.DNSRR;

public class MailDomain extends DNSRR {
	private String mailDestination;

	protected void decode(DNSInputStream dnsIn) throws IOException {
		mailDestination = dnsIn.readDomainName();
	}

	public String getMailDestination() {
		return mailDestination;
	}

	public String toString() {
		return getRRName() + "\tmail destination = " + mailDestination;
	}
}
