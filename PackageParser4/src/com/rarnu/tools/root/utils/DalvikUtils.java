package com.rarnu.tools.root.utils;

import android.content.Context;
import android.os.Handler;
import android.os.Message;
import android.preference.Preference;
import android.view.View;
import android.widget.Toast;
import com.rarnu.command.CommandResult;
import com.rarnu.command.RootUtils;
import com.rarnu.root.pp4.R;

import java.io.File;

public class DalvikUtils {

    // return -1 for failed
    // return 0 and more for cleaned count
    public static int cleanDalvik() {

        CommandResult cmdResult = RootUtils.runCommand("ls /data/dalvik-cache/", true, null);
        if (!cmdResult.error.equals("")) {
            return -1;
        }

        String str = cmdResult.result.replace("@", "/").replace("/classes.dex", "");
        String[] dalvikStr = str.split("\n");

        File fClean = null;
        int cleanCount = 0;
        String dalvikName = "";
        CommandResult cleanResult = null;
        if (dalvikStr != null && dalvikStr.length != 0) {
            for (String s : dalvikStr) {
                fClean = new File(s);
                if (!fClean.exists()) {
                    dalvikName = s.replace("/", "@");
                    cleanResult = RootUtils.runCommand(String.format("rm -r /data/dalvik-cache/%s*", dalvikName), true, null);
                    if (!cleanResult.error.equals("")) {
                        return -1;
                    } else {
                        cleanCount++;
                    }

                }
            }
        }
        return cleanCount;
    }

    public static void doCleanDalvikT(final Context context, final View view, final Preference pref) {

        pref.setTitle(R.string.cleaning_dalvik);
        view.setEnabled(false);

        final Handler h = new Handler() {
            @Override
            public void handleMessage(Message msg) {
                if (msg.what == 1) {

                    pref.setTitle(R.string.clean_dalvik);
                    view.setEnabled(true);

                    if (msg.arg1 == -1) {
                        Toast.makeText(context, R.string.clean_dalvik_fail, Toast.LENGTH_LONG).show();
                    } else if (msg.arg1 == 0) {
                        Toast.makeText(context, R.string.clean_dalvik_0, Toast.LENGTH_LONG).show();
                    } else {
                        Toast.makeText(context, String.format(context.getString(R.string.clean_dalvik_succ), msg.arg1), Toast.LENGTH_LONG).show();
                    }
                }
                super.handleMessage(msg);
            }
        };
        new Thread(new Runnable() {

            @Override
            public void run() {
                Message msg = new Message();
                msg.what = 1;
                msg.arg1 = cleanDalvik();
                h.sendMessage(msg);

            }
        }).start();
    }
}
