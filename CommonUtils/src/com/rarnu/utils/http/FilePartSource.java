package com.rarnu.utils.http;

import java.io.*;

public class FilePartSource implements PartSource {

    private File file = null;
    private String fileName = null;

    public FilePartSource(File file) throws Exception {
        this.file = file;
        if (file != null) {
            if (!file.isFile() || !file.canRead()) {
                throw new Exception();
            }
            this.fileName = file.getName();
        }
    }

    public FilePartSource(String fileName, File file) throws Exception {
        this(file);
        if (fileName != null) {
            this.fileName = fileName;
        }
    }

    public long getLength() {
        if (this.file != null) {
            return this.file.length();
        } else {
            return 0;
        }
    }

    public String getFileName() {
        return (fileName == null) ? "noname" : fileName;
    }

    public InputStream createInputStream() throws IOException {
        if (this.file != null) {
            return new FileInputStream(this.file);
        } else {
            return new ByteArrayInputStream(new byte[]{});
        }
    }

}