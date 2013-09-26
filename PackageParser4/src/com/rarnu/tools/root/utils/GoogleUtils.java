package com.rarnu.tools.root.utils;

import com.rarnu.command.CommandResult;
import com.rarnu.command.RootUtils;

import java.io.IOException;
import java.util.List;

public class GoogleUtils {

    public static String getGoogleAccount() {
        String GMAIL_FILE = "/data/data/com.google.android.gm/shared_prefs/Gmail.xml";
        String TEMP_GMAIL_FILE = DirHelper.TEMP_DIR + "Gmail.xml";

        CommandResult result = RootUtils.runCommand(String.format("busybox cp %s %s", GMAIL_FILE, DirHelper.TEMP_DIR), true, null);
        if (!result.error.equals("")) {
            return "";
        }
        String ret = "";
        try {
            List<String> lstGmail = FileUtils.readFile(TEMP_GMAIL_FILE);
            for (String s : lstGmail) {
                if (s.contains("active-account")) {
                    ret = s.replace("<string name=\"active-account\">", "").replace("</string>", "").trim();
                    break;
                }
            }
        } catch (IOException e) {
            ret = "";
        }
        return ret;
    }
}
