package com.rarnu.tools.root.sign;

import java.io.ByteArrayOutputStream;
import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.OutputStream;

class WholeFileSignerOutputStream extends FilterOutputStream {
    private boolean closing = false;
    private ByteArrayOutputStream footer = new ByteArrayOutputStream();
    private OutputStream tee;

    public WholeFileSignerOutputStream(OutputStream out, OutputStream tee) {
        super(out);
        this.tee = tee;
    }

    public void notifyClosing() {
        closing = true;
    }

    public void finish() throws IOException {
        closing = false;

        byte[] data = footer.toByteArray();
        if (data.length < 2) {
            throw new IOException("Less than two bytes written to footer");
        }
        write(data, 0, data.length - 2);
    }

    public byte[] getTail() {
        return footer.toByteArray();
    }

    @Override
    public void write(byte[] b) throws IOException {
        write(b, 0, b.length);
    }

    @Override
    public void write(byte[] b, int off, int len) throws IOException {
        if (closing) {
            footer.write(b, off, len);
        } else {
            out.write(b, off, len);
            tee.write(b, off, len);
        }
    }

    @Override
    public void write(int b) throws IOException {
        if (closing) {
            footer.write(b);
        } else {
            out.write(b);
            tee.write(b);
        }
    }
}