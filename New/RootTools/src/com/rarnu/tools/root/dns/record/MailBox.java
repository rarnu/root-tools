package com.rarnu.tools.root.dns.record;

import java.io.IOException;

import com.rarnu.tools.root.dns.DNSInputStream;
import com.rarnu.tools.root.dns.DNSRR;

public class MailBox extends DNSRR {
	private String mailBox;

	protected void decode(DNSInputStream dnsIn) throws IOException {
		mailBox = dnsIn.readDomainName();
	}

	public String getMailbox() {
		return mailBox;
	}

	public String toString() {
		return getRRName() + "\tmailbox = " + mailBox;
	}
}
