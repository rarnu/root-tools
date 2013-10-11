package com.rarnu.terminal.ssh;

import org.apache.sshd.SshServer;
import org.apache.sshd.server.keyprovider.SimpleGeneratorHostKeyProvider;

import java.io.IOException;

public class Sshd {

    private final SshServer sshd = SshServer.setUpDefaultServer();
    private final SimplePasswordAuthenticator passwordAuth = new SimplePasswordAuthenticator();
    private final SimplePublicKeyAuthenticator publicKeyAuth = new SimplePublicKeyAuthenticator();
    private final SimpleForwardingFilter forwardingFilter = new SimpleForwardingFilter();
    private String user = "root";
    private String pwd = "root";
    private int port = 8022;
    private Status status = Status.STOPPED;

    public Sshd() {
        passwordAuth.setUser(user);
        passwordAuth.setPassword(pwd);
        sshd.setPort(port);
        sshd.setKeyPairProvider(new SimpleGeneratorHostKeyProvider("key.ser"));
        sshd.setShellFactory(new PseudoTerminalFactory("/system/bin/sh", new String[]{"-i"}));
        sshd.setPasswordAuthenticator(passwordAuth);
        sshd.setPublickeyAuthenticator(publicKeyAuth);
        sshd.setForwardingFilter(forwardingFilter);
    }

    public Status getStatus() {
        return status;
    }

    public boolean start() {
        boolean ret = false;
        try {
            status = Status.STARTING;
            sshd.start();
            status = Status.STARTED;
            ret = true;
        } catch (IOException e) {

        }
        return ret;
    }

    public boolean stop() {
        boolean ret = false;
        try {
            status = Status.STOPPING;
            sshd.stop();
            status = Status.STOPPED;
            ret = true;
        } catch (Exception e) {

        }
        return ret;
    }

    public enum Status {
        STOPPED, STARTING, STARTED, STOPPING
    }
}
