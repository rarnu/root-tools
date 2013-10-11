package com.rarnu.terminal.ssh;

import org.apache.sshd.server.PasswordAuthenticator;
import org.apache.sshd.server.session.ServerSession;

class SimplePasswordAuthenticator implements PasswordAuthenticator {

    private String user;
    private String password;

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public String getUser() {
        return user;
    }

    public void setUser(String user) {
        this.user = user;
    }

    @Override
    public boolean authenticate(String user, String password, ServerSession session) {
        return user.equals(this.user) && password.equals(this.password);
    }

}
