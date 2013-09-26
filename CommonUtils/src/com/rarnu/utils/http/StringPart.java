package com.rarnu.utils.http;

import org.apache.http.util.EncodingUtils;

import java.io.IOException;
import java.io.OutputStream;

public class StringPart extends PartBase {

    public static final String DEFAULT_CONTENT_TYPE = "text/plain";
    public static final String DEFAULT_CHARSET = "US-ASCII";
    public static final String DEFAULT_TRANSFER_ENCODING = "8bit";
    private byte[] content;
    private String value;

    public StringPart(String name, String value, String charset) {

        super(name, DEFAULT_CONTENT_TYPE, charset == null ? DEFAULT_CHARSET : charset, DEFAULT_TRANSFER_ENCODING);
        if (value == null) {
            throw new IllegalArgumentException();
        }
        if (value.indexOf(0) != -1) {
            throw new IllegalArgumentException();
        }
        this.value = value;
    }

    public StringPart(String name, String value) {
        this(name, value, null);
    }

    private byte[] getContent() {
        if (content == null) {
            content = EncodingUtils.getBytes(value, getCharSet());
        }
        return content;
    }

    @Override
    protected void sendData(OutputStream out) throws IOException {
        out.write(getContent());
    }

    @Override
    protected long lengthOfData() {
        return getContent().length;
    }

    @Override
    public void setCharSet(String charSet) {
        super.setCharSet(charSet);
        this.content = null;
    }

}