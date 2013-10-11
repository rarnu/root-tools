package com.rarnu.terminal.ssh;

import org.apache.sshd.common.Factory;
import org.apache.sshd.server.Command;
import org.apache.sshd.server.shell.InvertedShellWrapper;

class PseudoTerminalFactory implements Factory<Command> {

    private String cmd;
    private String[] args;

    public PseudoTerminalFactory(String cmd, String[] args) {
        super();
        this.cmd = cmd;
        this.args = args;
    }

    @Override
    public Command create() {
        PseudoTerminal terminal = new PseudoTerminal(cmd, args);
        return new InvertedShellWrapper(terminal);
    }

}
