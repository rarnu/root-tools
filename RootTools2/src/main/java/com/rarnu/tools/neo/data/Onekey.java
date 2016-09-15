package com.rarnu.tools.neo.data;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by rarnu on 9/7/16.
 */
public class Onekey {

    public String pkgName = "";
    public String[] disabledComponents = null;

    public Onekey(String pkg, String str) {
        this.pkgName = pkg;
        try {
            String[] strs = str.split("\n");
            disabledComponents = strs;
        } catch (Exception e) {

        }
    }

}
