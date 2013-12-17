package com.rarnu.tools.root.sign;

import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.OutputStream;

class CountOutputStream extends FilterOutputStream {
    private int mCount;

    public CountOutputStream(OutputStream out) {
        super(out);
        mCount = 0;
    }

    @Override
    public void write(int b) throws IOException {
        super.write(b);
        mCount++;
    }

    @Override
    public void write(byte[] b, int off, int len) throws IOException {
        super.write(b, off, len);
        mCount += len;
    }

    public int size() {
        return mCount;
    }
}