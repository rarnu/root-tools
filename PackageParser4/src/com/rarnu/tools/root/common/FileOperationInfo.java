package com.rarnu.tools.root.common;

import java.io.Serializable;

public class FileOperationInfo implements Serializable {

    /**
     * 0:cut
     * 1:copy
     */
    public int operation;
    public String name;
    public String fullPath;
    public int icon;
}
