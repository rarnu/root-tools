package com.rarnu.terminal.ssh;

import org.apache.sshd.server.ForwardingFilter;
import org.apache.sshd.server.session.ServerSession;

import java.net.InetSocketAddress;

class SimpleForwardingFilter implements ForwardingFilter {

	@Override
	public boolean canConnect(InetSocketAddress inetSocketAddress, ServerSession serverSession) {
		return true;
	}

	@Override
	public boolean canForwardAgent(ServerSession serverSession) {
		return true;
	}

	@Override
	public boolean canForwardX11(ServerSession serverSession) {
		return true;
	}

	@Override
	public boolean canListen(InetSocketAddress inetSocketAddress, ServerSession serverSession) {
		return true;
	}
}
