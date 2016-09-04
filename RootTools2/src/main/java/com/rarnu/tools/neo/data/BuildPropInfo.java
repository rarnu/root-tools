package com.rarnu.tools.neo.data;

import java.io.Serializable;

public class BuildPropInfo implements Serializable {

    public String buildName;
    public String buildValue;

    public static BuildPropInfo parse(String str) {
        BuildPropInfo info = new BuildPropInfo();
        info.buildName = str.substring(0, str.indexOf("=")).trim();
        info.buildValue = str.substring(str.indexOf("=") + 1).trim();
        return info;
    }
}
