package com.rarnu.tools.neo.data;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by rarnu on 9/7/16.
 */
public class Onekey {

    public String pkgName = "";
    public List<String> disabledComponents = new ArrayList<>();

    public Onekey(String pkg, String str) {
        this.pkgName = pkg;
        try {
            String[] strs = str.split("\n");
            if (strs != null && strs.length != 0) {
                for (String s : strs) {
                    disabledComponents.add(s);
                }
            }
        } catch (Exception e) {

        }
    }

}
