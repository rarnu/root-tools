package com.rarnu.terminal.ssh;

import com.rarnu.terminal.Proc;
import org.apache.sshd.server.shell.InvertedShell;

import java.io.*;
import java.util.Map;

class PseudoTerminal implements InvertedShell {

    private OutputStream stdin;
    private InputStream stdout;
    private InputStream stderr;
    private FileDescriptor fd;
    private int pid;
    private String cmd;
    private String[] args;

    public PseudoTerminal(String cmd, String[] args) {
        this.cmd = cmd;
        this.args = args;
    }

    @Override
    public void destroy() {
    }

    @Override
    public int exitValue() {
        return 0;
    }

    @Override
    public InputStream getErrorStream() {
        return stderr;
    }

    @Override
    public OutputStream getInputStream() {
        return stdin;
    }

    @Override
    public InputStream getOutputStream() {
        return stdout;
    }

    @Override
    public boolean isAlive() {
        return true;
    }

    @Override
    public void start(Map<String, String> env) throws IOException {
        int[] pidOut = new int[1];
        fd = Proc.createSubprocess(cmd, args, null, pidOut);
        pid = pidOut[0];
        stdin = new FileOutputStream(fd);
        stdout = new FileInputStream(fd);
        stderr = new FileInputStream("/dev/null");
    }
}